# This contains default compiler options
# for the PGI vendor compiler suite.
V_C_VENDOR = pgi

# Compiler binaries
V_CC = pgcc
V_CPP = pgcc -E

V_MPICC = mpicc

# Flag for PIC compilation
V_C_PIC = -fPIC

# default compiler flags
V_CFLAGS =

# default compiler flags for debugging
V_CFLAGS_debug = -g -Mbounds

# These are pgi compiler options
V_CFLAGS_weak = -O1
V_CFLAGS_medium = -O2
V_CFLAGS_hard = -O3


# Local Variables:
#  mode: makefile-gmake
# End:
