/*---------------------------------------------------------------*/
/*--- begin                                 guest_pep8_defs.h ---*/
/*---------------------------------------------------------------*/

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

/* Only to be used within the guest-mips directory. */

#ifndef __VEX_GUEST_PEP8_DEFS_H
#define __VEX_GUEST_PEP8_DEFS_H

#include "libvex_basictypes.h"
#include "guest_generic_bb_to_IR.h"  /* DisResult */


#define PEP8_STOP 0
#define PEP8_RETTR 1
#define PEP8_MOVSPA 2
#define PEP8_MOVFLGA 3
#define PEP8_BR 4

#define PEP8_BRLE 5
#define PEP8_BRLT 6
#define PEP8_BREQ 7
#define PEP8_BRNE 8
#define PEP8_BRGE 9
#define PEP8_BRGT 10
#define PEP8_BRV 11
#define PEP8_BRC 12
#define PEP8_CALL 13
#define PEP8_NOT 14
#define PEP8_NEG 15
#define PEP8_ASL 16
#define PEP8_ASR 17
#define PEP8_ROL 18
#define PEP8_ROR 19

#define PEP8_NOPN 20

#define PEP8_NOP 21
#define PEP8_DECI 22
#define PEP8_DECO 23
#define PEP8_STRO 24
#define PEP8_CHARI 25
#define PEP8_CHARO 26
#define PEP8_RETN 27
#define PEP8_ADDSP 28
#define PEP8_SUBSP 29

#define PEP8_ADD 30
#define PEP8_SUB 31
#define PEP8_AND 32
#define PEP8_OR 33
#define PEP8_CP 34
#define PEP8_LD 35
#define PEP8_LDBYTE 36
#define PEP8_ST 37
#define PEP8_STBYTE 38


/*---------------------------------------------------------*/
/*---               pep8 to IR conversion               ---*/
/*---------------------------------------------------------*/

/* Convert one MIPS insn to IR. See the type DisOneInstrFn in bb_to_IR.h. */
extern DisResult disInstr_PEP8 ( IRSB*        irbb,
                                 Bool         (*resteerOkFn) (void *, Addr),
                                 Bool         resteerCisOk,
                                 void*        callback_opaque,
                                 const UChar* guest_code,
                                 Long         delta,
                                 Addr         guest_IP,
                                 VexArch      guest_arch,
                                 const VexArchInfo* archinfo,
                                 const VexAbiInfo*  abiinfo,
                                 VexEndness   host_endness,
                                 Bool         sigill_diag );

/* Used by the optimiser to specialise calls to helpers. */
extern IRExpr *guest_pep8_spechelper ( const HChar * function_name,
                                         IRExpr ** args,
                                         IRStmt ** precedingStmts,
                                         Int n_precedingStmts );

/* Describes to the optimser which part of the guest state require
   precise memory exceptions.  This is logically part of the guest
   state description. */
extern
Bool guest_pep8_state_requires_precise_mem_exns ( Int, Int,
                                                    VexRegisterUpdates );

extern VexGuestLayout pep8Guest_layout;

/* Holds expressions that need to be pushed forward to the next
   instruction because of MIPS' pipelining.

   The original implementation had them as static variables in a
   function in guest_mips_toIR.c, but it is possible to lift
   single instructions out of context and out of order, so this
   variable needs to be reset at the beginning of each basic block. */
IRExpr *pep8_lastn;
IRStmt *pep8_bstmt;

/*---------------------------------------------------------*/
/*---                mips guest helpers                 ---*/
/*---------------------------------------------------------*/

extern UInt pep8_dirtyhelper_mfc0 ( UInt rd, UInt sel );

#if defined(__mips__) && ((defined(__mips_isa_rev) && __mips_isa_rev >= 2))
extern UInt pep8_dirtyhelper_rdhwr ( UInt rt, UInt rd );
#endif

/*---------------------------------------------------------*/
/*---               Condition code stuff                ---*/
/*---------------------------------------------------------*/

typedef enum {
   PEP8CondEQ = 0,   /* equal                         : Z=1 */
   PEP8CondNE = 1,   /* not equal                     : Z=0 */

   PEP8CondHS = 2,   /* >=u (higher or same)          : C=1 */
   PEP8CondLO = 3,   /* <u  (lower)                   : C=0 */

   PEP8CondMI = 4,   /* minus (negative)              : N=1 */
   PEP8CondPL = 5,   /* plus (zero or +ve)            : N=0 */

   PEP8CondVS = 6,   /* overflow                      : V=1 */
   PEP8CondVC = 7,   /* no overflow                   : V=0 */
} PEP8Condcode;

#endif            /* __VEX_GUEST_PEP8_DEFS_H */

/*---------------------------------------------------------------*/
/*--- end                                   guest_pep8_defs.h ---*/
/*---------------------------------------------------------------*/
