# This contains default compiler options
# for the GNU vendor compiler suite.
V_C_VENDOR = gnu

# Compiler binaries
V_CC = gcc
V_CPP = cpp -E -P -x c

V_MPICC = mpicc

# Flag for PIC compilation
V_C_PIC = -fPIC

# default compiler flags
V_CFLAGS =

# default compiler flags for debugging
V_CFLAGS_debug = -g -fbacktrace -fbounds-check -Wunused-variable -Warray-temporaries

# These are gcc compiler options
V_CFLAGS_weak = -O1 -funroll-loops
V_CFLAGS_medium = -O2 -funroll-loops -ftree-vectorize -fprefetch-loop-arrays
V_CFLAGS_hard = -O3 -funroll-loops -ftree-vectorize -fprefetch-loop-arrays -fgraphite -fselective-scheduling


# Local Variables:
#  mode: makefile-gmake
# End:
