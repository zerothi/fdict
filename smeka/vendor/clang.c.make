# This contains default compiler options
# for the GNU vendor compiler suite.
V_C_VENDOR = clang

# Compiler binaries
V_CC = clang
V_CPP = clang -E -P -x c


V_MPICC = mpicc

# Flag for PIC compilation
V_C_PIC = -fPIC

# default compiler flags
V_CFLAGS =

# default compiler flags for debugging
V_CFLAGS_debug = -g -fbacktrace -fbounds-check -Wunused-variable -Warray-temporaries

# These are clang compiler options
V_CFLAGS_weak = -O1
V_CFLAGS_medium = -O2
V_CFLAGS_hard = -O3


# Local Variables:
#  mode: makefile-gmake
# End:
