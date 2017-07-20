
/*--------------------------------------------------------------------*/
/*--- begin                                      guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/

/*
   This file is part of Valgrind, a dynamic binary instrumentation
   framework.

   Copyright (C) 2010-2013 RT-RK
   mips-valgrind@rt-rk.com

   This program is free software; you can redistribute it and/or
   modify it under the terms of the GNU General Public License as
   published by the Free Software Foundation; either version 2 of the
   License, or (at your option) any later version.

   This program is distributed in the hope that it will be useful, but
   WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
   General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program; if not, write to the Free Software
   Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
   02111-1307, USA.

   The GNU General Public License is contained in the file COPYING.
   */

/* Translates PEP8 code to IR. */

#include "libvex_basictypes.h"
#include "libvex_ir.h"
#include "libvex.h"
#include "libvex_guest_pep8.h"

#include "main_util.h"
#include "main_globals.h"
#include "guest_generic_bb_to_IR.h"
#include "guest_pep8_defs.h"

/*------------------------------------------------------------*/
/*---                      Globals                         ---*/
/*------------------------------------------------------------*/

/* These are set at the start of the translation of a instruction, so
   that we don't have to pass them around endlessly. CONST means does
   not change during translation of the instruction. */

/* CONST: what is the host's endianness?  This has to do with float vs
   double register accesses on VFP, but it's complex and not properly
   thought out. */
static VexEndness host_endness;

/* Whether code we're analyzing comes from a big or little endian machine */
static IREndness guest_endness;

/* Pointer to the guest code area. */
static const UChar *guest_code;

/* CONST: The guest address for the instruction currently being
   translated. */
static Addr64 guest_PC_curr_instr;

/* MOD: The IRSB* into which we're generating code. */
static IRSB *irsb;

/* Is our guest binary 32 or 64bit? Set at each call to
   disInstr below. */
static Bool mode64 = False;

/* CPU has FPU and 32 dbl. prec. FP registers. */
static Bool fp_mode64 = False;

/*------------------------------------------------------------*/
/*---                     Bitmasks                         ---*/
/*------------------------------------------------------------*/

#define VexArchPEP8_RegA 0
#define VexArchPEP8_RegX 1

/*------------------------------------------------------------*/
/*---                  Offset Definitions                  ---*/
/*------------------------------------------------------------*/

#define OFFB_A			offsetof(VexGuestPEP8State, guest_a)
#define OFFB_X			offsetof(VexGuestPEP8State, guest_x)
#define OFFB_SP			offsetof(VexGuestPEP8State, guest_sp)
#define OFFB_PC			offsetof(VexGuestPEP8State, guest_pc)
#define OFFB_PC_SYSCALL	offsetof(VexGuestPEP8State, guest_pc_at_syscall)
#define OFFB_N			offsetof(VexGuestPEP8State, guest_n)
#define OFFB_Z			offsetof(VexGuestPEP8State, guest_z)
#define OFFB_V			offsetof(VexGuestPEP8State, guest_v)
#define OFFB_C			offsetof(VexGuestPEP8State, guest_c)

/*------------------------------------------------------------*/
/*---                  Debugging output                    ---*/
/*------------------------------------------------------------*/

#ifndef _WIN32
#define DIP(format, args...)           \
		if (vex_traceflags & VEX_TRACE_FE)  \
vex_printf(format, ## args)

#else
#define DIP(format, ...)           \
		if (vex_traceflags & VEX_TRACE_FE)  \
vex_printf(format, __VA_ARGS__)
#endif

/*------------------------------------------------------------*/
/*---               IR Utility functions                   ---*/
/*------------------------------------------------------------*/

/* Add a statement to the list held by "irbb". */
static void stmt ( IRStmt* st )
{
		addStmtToIRSB( irsb, st );
}

/* Generate a statement "dst := e". */
static void assign ( IRTemp dst, IRExpr* e )
{
		stmt( IRStmt_WrTmp(dst, e) );
}

static IRExpr* unop ( IROp op, IRExpr* e1)
{
		return IRExpr_Unop(op, e1);
}

static IRExpr* binop ( IROp op, IRExpr* e1, IRExpr* e2)
{
		return IRExpr_Binop(op, e1, e2);
}

static IRExpr* mkU8 ( UInt i )
{
		vassert(i < 256);
		return IRExpr_Const(IRConst_U8( (UChar)i ));
}

static IRExpr* mkU16 ( UInt i )
{
		vassert(i <= 65535);
		return IRExpr_Const(IRConst_U16( (UShort)i ));
}

static IRExpr* mkU32 ( UInt i )
{
		vassert(i <= 0xFFFFFFFF);
		return IRExpr_Const(IRConst_U32( (UInt)i ));
}

static IRExpr* mkExpr ( IRTemp tmp )
{
		return IRExpr_RdTmp(tmp);
}

static IRTemp newTemp ( IRType ty )
{
		vassert(isPlausibleIRType(ty));
		return newIRTemp( irsb->tyenv, ty );
}

/*------------------------------------------------------------*/
/*---               IR Generation helpers                  ---*/
/*------------------------------------------------------------*/

static void putPC(IRExpr * e)
{
		stmt(IRStmt_Put(OFFB_PC, e));
}

static void putPCSyscall(IRExpr * e)
{
		stmt(IRStmt_Put(OFFB_PC_SYSCALL, e));
}

static void putWordReg(Int reg, IRExpr* e)
{
		IRType ty = typeOfIRExpr(irsb->tyenv, e);
		vassert(ty == Ity_I16);

		if (reg == VexArchPEP8_RegA) {
				stmt(IRStmt_Put(OFFB_A, e));
		} else {
				stmt(IRStmt_Put(OFFB_X, e));
		}
}

/*------------------------------------------------------------*/
/*---               PEP8 Utility functions                 ---*/
/*------------------------------------------------------------*/

static UShort getWord(UShort addr)
{
		UShort value;
		value = guest_code[addr];
		value <<= 8;
		value += guest_code[addr + 1];
		return value;
}

static UChar decodeRegister(const UChar cins)
{
		UChar reg;
		Bool is_x = False;

		// Check the right bit
		if (cins >= 24 && cins <= 0x35) {
				is_x = cins & 0x1;
		} else if (cins >= 112 && cins <= 255) {
				is_x = cins & 0x8;
		} else {
				vpanic("Tried decoding register of an instruction with no coded register.");
		}

		if (is_x) {
				reg = OFFB_X;
		} else {
				reg = OFFB_A;
		}
		return reg;
}

static IRTemp resolveOperand(const UChar cins, UShort delta)
{
		UChar addr_mode;
		UShort operand;
		UShort value;
		UShort direct;

		IRType ty16 = Ity_I16;

		IRTemp res = newTemp(ty16);

		// operand = getWord(delta + 1);

		// Addressing mode is coded in 1 or 3 bits depending on how
		// many modes are valid for the instruction.
		// For addressing modes coded on 1 bit:
		//	* 0 : immediate
		//	* 1 : indexed (offsetted by the value in RegX)
		// All these instructions are coded so that the resulting
		// word is an unsigned short smaller or equal to 0b0001011a
		// where 'a' is the addressing mode
		if (cins <= 0x17) {
				addr_mode = cins & 0x01;
				if (addr_mode == 0) {
				}
		}

		// For addressing modes coded on 3 bits:
		//  * 000 : i - immediate
		//  * 001 : d - direct
		//  * 010 : n - indirect
		//  * 011 : s - offsetted on the stack
		//  * 100 : sf - indirect offsetted on the stack
		//  * 101 : x - offsetted by the value in RegX
		//  * 110 : sx - On the stack offsetted by the value in RegX
		//  * 111 : sxf - Offsetted by X in the struct referenced on the stack
		else {
				addr_mode = cins & 0x07;

				switch(addr_mode) {
						/* i - immediate
						 * to make this generalisable, we return the address of the
						 * immediate value coded in the opcode */
						case 0x0:
								{
										assign(res, mkU16(guest_PC_curr_instr + 1));
								}
								break;

						/* d - direct */
						case 0x1:
								{
										assign(res, IRExpr_Load(Iend_LE, ty16, mkU16(guest_PC_curr_instr + 1)));
								}
								break;

						/* x - indexed by X register */
						case 0x5:
								{
										vpanic("Unimplemented PEP8 addressing mode!");
								}
								break;
						default:
								  vpanic("Unimplemented PEP8 addressing mode!");
								  break;
				}
		}

		return res;
}

/*------------------------------------------------------------*/
/*---                 Decoding functions                   ---*/
/*------------------------------------------------------------*/
UInt get_opcode(UChar cins)
{
		UInt real_bitmask;

		if (cins <= 3) {
				real_bitmask = cins >> 0;

				switch (real_bitmask) {

						case 0: return PEP8_STOP;
						case 1: return PEP8_RETTR;
						case 2: return PEP8_MOVSPA;
						case 3: return PEP8_MOVFLGA;
						default: {
										 return -1;
								 }
				}

		} else if (cins <= 35) {
				real_bitmask = cins >> 1;
				switch (real_bitmask) {
						case 2: return PEP8_BR;
						case 3: return PEP8_BRLE;
						case 4: return PEP8_BRLT;
						case 5: return PEP8_BREQ;
						case 6: return PEP8_BRNE;
						case 7: return PEP8_BRGE;
						case 8: return PEP8_BRGT;
						case 9: return PEP8_BRV;
						case 10: return PEP8_BRC;
						case 11: return PEP8_CALL;
						case 12: return PEP8_NOT;
						case 13: return PEP8_NEG;
						case 14: return PEP8_ASL;
						case 15: return PEP8_ASR;
						case 16: return PEP8_ROL;
						case 17: return PEP8_ROR;
						default: {
										 return -1;
								 }
				}
		} else if (cins <= 39) {
				real_bitmask = cins >> 2;
				return PEP8_NOPN;
		} else if (cins <= 111) {
				real_bitmask = cins >> 3;
				switch (real_bitmask) {
						case 5: return PEP8_NOP;
						case 6: return PEP8_DECI;
						case 7: return PEP8_DECO;
						case 8: return PEP8_STRO;
						case 9: return PEP8_CHARI;
						case 10: return PEP8_CHARO;
						case 11: return PEP8_RETN;
						case 12: return PEP8_ADDSP;
						case 13: return PEP8_SUBSP;
						default: {
										 return -1;
								 }
				}
		} else {
				real_bitmask = cins >> 4;
				switch (real_bitmask) {
						case 7: return PEP8_ADD;
						case 8: return PEP8_SUB;
						case 9: return PEP8_AND;
						case 10: return PEP8_OR;
						case 11: return PEP8_CP;
						case 12: return PEP8_LD;
						case 13: return PEP8_LDBYTE;
						case 14: return PEP8_ST;
						case 15: return PEP8_STBYTE;
						default: {
										 return -1;
								 }
				}
		}
}

/*------------------------------------------------------------*/
/*---          Disassemble a single instruction            ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR. The instruction is
   located in host memory at guest_instr, and has guest IP of
   guest_PC_curr_instr, which will have been set before the call
   here. */

static DisResult disInstr_PEP8_WRK ( Bool(*resteerOkFn) (/*opaque */void *,
						Addr),
				Bool         resteerCisOk,
				void*        callback_opaque,
				Long         delta64,
				const VexArchInfo* archinfo,
				const VexAbiInfo*  abiinfo,
				Bool         sigill_diag )
{
		IRTemp t0, t1, t2, t3, t4, t5, t6, t7;

		IRExpr *lastn = pep8_lastn;  /* last jump addr */
		IRStmt *bstmt = pep8_bstmt;  /* branch (Exit) stmt */

		DisResult dres;
		UChar opcode;
		UChar cins;

		/* Set result defaults. */
		dres.whatNext = Dis_Continue;
		dres.len = 0;
		dres.continueAt = 0;
		dres.jk_StopHere = Ijk_INVALID;

		UShort delta = (UShort) delta64;
		const UChar *code = guest_code + delta;
		cins = *code;

		DIP("\t0x%hx:\t0x%08hhx\t", (short)guest_PC_curr_instr, cins);

		IRType ty8 = Ity_I8;
		IRType ty16 = Ity_I16;
		IRType ty32 = Ity_I32;

		opcode = get_opcode(cins);

		switch (opcode){

				case PEP8_STOP:
						putPCSyscall(mkU16(guest_PC_curr_instr));
						putPC(mkU16(0));

						dres.len = 1;
						dres.whatNext = Dis_StopHere;
						dres.jk_StopHere = Ijk_Sys_int128;
						break;

				case PEP8_NOPN:
						/* TODO implement N */
						putPC(mkU16(guest_PC_curr_instr + 1));
						dres.len = 1;
						break;

				case PEP8_ADD:
						{
								IRTemp operand = resolveOperand(cins, delta);
								UChar reg = decodeRegister(cins);
								IRTemp value = newTemp(ty16);
								IREndness end_le = Iend_LE;

								assign(value,
								       binop(Iop_Add16,
										     IRExpr_Get(reg, ty16),
										     IRExpr_Load(Iend_LE,
													     ty16,
														 IRExpr_RdTmp(operand)
														 )
											 )
									   );

								stmt(IRStmt_Put(reg, IRExpr_RdTmp(value)));

								// Useful temps for flags
								IRTemp x = newTemp(ty16);
								IRTemp y = newTemp(ty16);
								IRTemp res = newTemp(ty16);
								IRTemp z = newTemp(ty16);
								assign(z, mkExpr(value));
								assign(x, IRExpr_Get(reg, ty16));
								assign(y, IRExpr_Load(Iend_LE, ty16, IRExpr_RdTmp(operand)));
								assign(res, IRExpr_RdTmp(value));

								printf("PEPS DEBG: %d", OFFB_Z);
								// Zero Flag
								stmt(IRStmt_Put(
														OFFB_Z,
														IRExpr_ITE( binop(Iop_CmpEQ16, mkU16(0), mkExpr(value)),
																mkU16(1),
																mkU16(0) )));

								// Neg Flag
								stmt(IRStmt_Put(
														OFFB_N,
														IRExpr_ITE( binop(Iop_CmpLT32S, unop(Iop_16Sto32, mkExpr(value)), mkU32(0)),
																mkU16(1),
																mkU16(0) )));

								// Overflow Flag
								// overflow = (x < 32768 and y < 32768 and result >= 32768) or (x >= 32768 and y >= 32768 and result < 32768)
								IRTemp ovf = newTemp(ty16);
								IRTemp x15 = newTemp(ty16);
								assign(x15,
												binop(Iop_Shr16,
														binop(Iop_And16,
																IRExpr_RdTmp(x),
																mkU16(0x8000)),
														mkU8(15))
									  );

								IRTemp y15 = newTemp(ty16);
								assign(y15,
												binop(Iop_Shr16,
														binop(Iop_And16,
																IRExpr_RdTmp(y),
																mkU16(0x8000)),
														mkU8(15))
									  );

								IRTemp r15 = newTemp(ty16);
								assign(r15,
												binop(Iop_Shr16,
														binop(Iop_And16,
																IRExpr_RdTmp(res),
																mkU16(0x8000)),
														mkU8(15))
									  );

								assign(ovf,
												binop(Iop_And16,
														unop(Iop_Not16, binop(Iop_Xor16, mkExpr(x15), mkExpr(y15))),
														binop(Iop_Xor16, mkExpr(x15), mkExpr(r15)))
									  );

								stmt(IRStmt_Put(OFFB_V, mkExpr(ovf)));

								// Carry flag
								// (x+y) > 0xffff ? 1 : 0

								IRTemp ext_x = newTemp(ty32);
								IRTemp ext_y = newTemp(ty32);
								IRTemp sum = newTemp(ty32);
								IRTemp cbit = newTemp(ty16);

								// Zero extend x and y
								assign(ext_x, unop(Iop_16Uto32, IRExpr_RdTmp(x)));
								assign(ext_y, unop(Iop_16Uto32, IRExpr_RdTmp(y)));
								assign(sum, binop(Iop_Add32,
												  IRExpr_RdTmp(ext_x),
												  IRExpr_RdTmp(ext_y)));

								assign(cbit, IRExpr_ITE(binop(Iop_CmpLE32U,
															  mkU32(0xFFFF),
														      IRExpr_RdTmp(sum)
															  ),
														mkU16(0x1),
														mkU16(0x0)
													   )
									   );

								stmt(IRStmt_Put(OFFB_C, mkExpr(cbit)));


								// Update PC and instr length
								putPC(mkU16(guest_PC_curr_instr + 3));
								dres.len = 3;
						}
						break;
				case PEP8_LD:
						{
								IRTemp operand = resolveOperand(cins, delta);
								IRTemp value = newTemp(ty16);
								UChar reg = decodeRegister(cins);

								IREndness end_le = Iend_LE;
								assign(
												value,
												IRExpr_Load(Iend_LE, ty16, IRExpr_RdTmp(operand))
									  );
								stmt(IRStmt_Put(reg, IRExpr_RdTmp(value)));

								// Zero Flag
								stmt(IRStmt_Put(
														OFFB_Z,
														IRExpr_ITE( binop(Iop_CmpEQ16, mkU16(0), mkExpr(value)),
																mkU8(1),
																mkU8(0) )));

								// Neg Flag
								stmt(IRStmt_Put(
														OFFB_N,
														IRExpr_ITE( binop(Iop_CmpLT32S, unop(Iop_16Sto32, mkExpr(value)), mkU32(0)),
																mkU8(1),
																mkU8(0) )));

								// Update PC and instr length
								putPC(mkU16(guest_PC_curr_instr + 3));
								dres.len = 3;
						}
						break;
				case PEP8_ST:
						{
								IRTemp operand_addr = resolveOperand(cins, delta);
								IRTemp operand = newTemp(ty16);

								IRTemp value = newTemp(ty16);
								UChar reg = decodeRegister(cins);

								IREndness end_le = Iend_LE;

								assign(operand, IRExpr_Load(end_le, ty16, IRExpr_RdTmp(operand_addr)));
								assign(value, IRExpr_Get(reg, ty16));
								stmt(IRStmt_Store(end_le, IRExpr_RdTmp(operand), IRExpr_RdTmp(value)));

								// Update PC and instr length
								putPC(mkU16(guest_PC_curr_instr + 3));
								dres.len = 3;
						}
						break;

				default:
						DIP("Instruction not implemented.");
						break;
		}

		// dres.whatNext = Dis_StopHere;
		// dres.jk_StopHere = Ijk_Boring;
		// dres.continueAt = 0;

		return dres;

}
/*------------------------------------------------------------*/
/*--- Top-level fn                                         ---*/
/*------------------------------------------------------------*/

/* Disassemble a single instruction into IR.  The instruction
   is located in host memory at &guest_code[delta]. */
DisResult disInstr_PEP8( IRSB*        irsb_IN,
				Bool         (*resteerOkFn) ( void *, Addr ),
				Bool         resteerCisOk,
				void*        callback_opaque,
				const UChar* guest_code_IN,
				Long         delta,
				Addr         guest_IP,
				VexArch      guest_arch,
				const VexArchInfo* archinfo,
				const VexAbiInfo*  abiinfo,
				VexEndness   host_endness_IN,
				Bool         sigill_diag_IN )
{
		DisResult dres;
		/* Set globals (see top of this file) */
		vassert(guest_arch == VexArchPEP8);

		guest_code = guest_code_IN;
		irsb = irsb_IN;
		host_endness = host_endness_IN;
		guest_endness = archinfo->endness == VexEndnessLE ? Iend_LE : Iend_BE;
		guest_PC_curr_instr = (Addr64)guest_IP;

		dres = disInstr_PEP8_WRK(resteerOkFn, resteerCisOk, callback_opaque,
						delta, archinfo, abiinfo, sigill_diag_IN);

		return dres;
}

/*--------------------------------------------------------------------*/
/*--- end                                        guest_mips_toIR.c ---*/
/*--------------------------------------------------------------------*/
