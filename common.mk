# Environment variables that the build system should respect:
# - MULTIARCH: set to 1 to enable multiarch compiliation
# - EXTRA_CFLAGS: exactly what it says on the tin
# - DEBUG: set to 1 to disable optimization and enable debug symbols

# These headers are autogenerated
GEN_HEADERS = pub/libvex_guest_offsets.h

# These headers are the public interface to libvex
PUB_HEADERS = \
	pub/libvex_basictypes.h 	\
	pub/libvex_ir.h			\
	pub/libvex.h			\
	pub/libvex_trc_values.h		\
	pub/libvex_emnote.h		\
	pub/libvex_guest_x86.h		\
	pub/libvex_guest_amd64.h	\
	pub/libvex_guest_arm.h		\
	pub/libvex_guest_ppc32.h	\
	pub/libvex_guest_ppc64.h	\
	pub/libvex_guest_s390x.h	\
	pub/libvex_s390x_common.h	\
	pub/libvex_guest_mips32.h	\
	pub/libvex_guest_pep8.h

# These headers are private, but we enumerate them to trigger rebuilds
# when any of them change
PRIV_HEADERS = \
	priv/host_x86_defs.h		\
	priv/host_amd64_defs.h		\
	priv/host_arm_defs.h		\
	priv/host_ppc_defs.h		\
	priv/host_s390_defs.h		\
	priv/host_mips_defs.h		\
	priv/host_generic_maddf.h	\
	priv/host_generic_regs.h	\
	priv/host_generic_simd64.h	\
	priv/host_generic_simd128.h	\
	priv/host_generic_simd256.h	\
	priv/main_globals.h		\
	priv/main_util.h		\
	priv/guest_generic_x87.h	\
	priv/guest_generic_bb_to_IR.h	\
	priv/guest_x86_defs.h		\
	priv/guest_amd64_defs.h		\
	priv/guest_arm_defs.h		\
	priv/guest_ppc_defs.h		\
	priv/guest_mips_defs.h		\
	priv/guest_pep8_defs.h		\
	priv/s390_disasm.h		\
	priv/s390_defs.h		\
	priv/ir_match.h			\
	priv/ir_opt.h

NORMAL_OBJS = \
	priv/ir_defs.o			\
	priv/ir_match.o			\
	priv/ir_opt.o			\
	priv/ir_inject.o		\
	priv/main_globals.o		\
	priv/main_util.o		\
	priv/s390_disasm.o		\
	priv/host_x86_defs.o		\
	priv/host_amd64_defs.o		\
	priv/host_arm_defs.o		\
	priv/host_arm64_defs.o		\
	priv/host_ppc_defs.o		\
	priv/host_s390_defs.o		\
	priv/host_mips_defs.o		\
	priv/host_x86_isel.o		\
	priv/host_amd64_isel.o		\
	priv/host_arm_isel.o		\
	priv/host_arm64_isel.o		\
	priv/host_ppc_isel.o		\
	priv/host_s390_isel.o		\
	priv/host_mips_isel.o		\
	priv/host_generic_maddf.o	\
	priv/host_generic_regs.o	\
	priv/host_generic_simd64.o	\
	priv/host_generic_simd128.o	\
	priv/host_generic_simd256.o	\
	priv/host_generic_reg_alloc2.o	\
	priv/guest_generic_x87.o	\
	priv/guest_generic_bb_to_IR.o	\
	priv/guest_x86_helpers.o	\
	priv/guest_amd64_helpers.o	\
	priv/guest_arm_helpers.o	\
	priv/guest_arm64_helpers.o	\
	priv/guest_ppc_helpers.o	\
	priv/guest_s390_helpers.o	\
	priv/guest_mips_helpers.o	\
	priv/guest_pep8_helpers.o	\
	priv/guest_x86_toIR.o		\
	priv/guest_amd64_toIR.o		\
	priv/guest_arm_toIR.o		\
	priv/guest_arm64_toIR.o		\
	priv/guest_ppc_toIR.o		\
	priv/guest_s390_toIR.o		\
	priv/guest_mips_toIR.o		\
	priv/guest_pep8_toIR.o

SINGLEARCH_OBJS = priv/main_main.o
MULTIARCH_OBJS = priv/multiarch_main_main.o

ALL_HEADERS  = $(PUB_HEADERS) $(PRIV_HEADERS) $(GEN_HEADERS)
