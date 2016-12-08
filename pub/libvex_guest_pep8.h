
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
      /* 0 */  UShort guest_a;   /* Accumulator Register */
      /* 2 */  UShort guest_x;   /* Index Register */
      /* 4 */  UShort guest_sp;  /* Stack Pointer */
      /* 6 */  UShort guest_pc;  /* Program Counter */

      /* CPU State Flags */
      /* 8 */  UChar guest_n;    /* Negative Flag */
      /* 9 */  UChar guest_z;    /* Zero Flag */
      /* 10 */ UChar guest_v;    /* Overflow Flag */
      /* 11 */ UChar guest_c;    /* Carry Flag */

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
