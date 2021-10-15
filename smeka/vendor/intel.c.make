# This contains default compiler options
# for the Intel vendor compiler suite.
V_C_VENDOR = intel

# Compiler binaries
V_CC = icc
V_CPP = cpp -E -P -x c

V_MPICC = mpicc

# Flag for PIC compilation
V_C_PIC = -fPIC

# default compiler flags
V_CFLAGS =

# default compiler flags for debugging
V_CFLAGS_debug ?= -g -check bounds -traceback

# These are intel compiler options
V_CFLAGS_weak = -O1
V_CFLAGS_medium = -O2 -prec-div -prec-sqrt
V_CFLAGS_hard = -O3 -prec-div -prec-sqrt -opt-prefetch


# Local Variables:
#  mode: makefile-gmake
# End:
