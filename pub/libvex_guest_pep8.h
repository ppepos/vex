
/*---------------------------------------------------------------*/
/*--- begin                             libvex_guest_mips32.h ---*/
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

#ifndef __LIBVEX_PUB_GUEST_PEP8_H
#define __LIBVEX_PUB_GUEST_PEP8_H

#include "libvex_basictypes.h"


/*---------------------------------------------------------------*/
/*--- Vex's representation of the Pep8 CPU simulator state.   ---*/
/*---------------------------------------------------------------*/

typedef
   struct {
      /* CPU Registers */
      /* 0 */  UInt  host_EvC_FAILADDR;
      /* 4 */  UInt  host_EvC_COUNTER;
      /* 8 */  UInt  pad_1;
      /* 12 */ UInt  pad_2;
      /* 16 */ UShort guest_a;   /* Accumulator Register */
      /* 18 */ UShort guest_x;   /* Index Register */
      /* 20 */ UShort guest_sp;  /* Stack Pointer */
      /* 22 */ UShort guest_pc;  /* Program Counter */
      /* 24 */ UShort guest_pc_at_syscall; /* Needed for syscalls */

      /* CPU State Flags */
      /* 26 */ UChar guest_n;    /* Negative Flag */
      /* 27 */ UChar guest_z;    /* Zero Flag */
      /* 28 */ UChar guest_v;    /* Overflow Flag */
      /* 29 */ UChar guest_c;    /* Carry Flag */

	  /* Pad to 16 byte multiple */
      /* 30 */ UChar guest_pad0;
      /* 31 */ UChar guest_pad1;

} VexGuestPEP8State;

/*---------------------------------------------------------------*/
/*--- Utility functions for Pep8 guest stuff.                 ---*/
/*---------------------------------------------------------------*/

/* ALL THE FOLLOWING ARE VISIBLE TO LIBRARY CLIENT */

/* Initialise all guest Pep8 states. */

extern
void LibVEX_GuestPEP8_initialise ( /*OUT*/VexGuestPEP8State* vex_state );


#endif /* ndef __LIBVEX_PUB_GUEST_PEP8_H */


/*---------------------------------------------------------------*/
/*---                                     libvex_guest_pep8.h ---*/
/*---------------------------------------------------------------*/
