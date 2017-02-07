#include "libvex_basictypes.h"
#include "libvex_guest_pep8.h"
#include "libvex_ir.h"
#include "libvex.h"

#include "guest_generic_bb_to_IR.h"
#include "guest_pep8_defs.h"

/* TODO: Actually implement this. See guest_x86_helpers.c */
Bool guest_pep8_state_requires_precise_mem_exns (
        Int minoff, Int maxoff, VexRegisterUpdates pxControl
     )
{
	return True;
}

/* Stub Spechelper
 * TODO: Maybe implement this... not sure if needed
 * */

IRExpr* guest_pep8_spechelper ( const HChar* function_name,
                               IRExpr** args,
                               IRStmt** precedingStmts,
                               Int      n_precedingStmts )
{
	return NULL;
}


#define ALWAYSDEFD(field)                           \
    { offsetof(VexGuestPEP8State, field),            \
      (sizeof ((VexGuestPEP8State*)0)->field) }

VexGuestLayout
   pep8Guest_layout
      = {
          /* Total size of the guest state, in bytes. */
          .total_sizeB = sizeof(VexGuestPEP8State),

          /* Describe the stack pointer. */
          .offset_SP = offsetof(VexGuestPEP8State,guest_sp),
          .sizeof_SP = 2,

          /* Describe the instruction pointer. */
          .offset_IP = offsetof(VexGuestPEP8State,guest_pc),
          .sizeof_IP = 2,

          /* Describe any sections to be regarded by Memcheck as
             'always-defined'. */
          // .n_alwaysDefd = 24,

          /* flags thunk: OP and NDEP are always defd, whereas DEP1
             and DEP2 have to be tracked.  See detailed comment in
             gdefs.h on meaning of thunk fields. */
          .alwaysDefd
			= { ALWAYSDEFD(guest_pc),
				ALWAYSDEFD(guest_n),
				ALWAYSDEFD(guest_z),
				ALWAYSDEFD(guest_v),
				ALWAYSDEFD(guest_c)
			}
        };

