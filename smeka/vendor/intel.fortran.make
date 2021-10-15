# This contains default compiler options
# for the Intel vendor compiler suite.
V_F_VENDOR = intel

# Compiler binaries
V_FC = ifort
V_FPP = ifort -E -P -xc

V_MPIFC = mpif90

# Flag for output directory of modules
V_FC_MODDIR = -J

# Flag for PIC compilation
V_F_PIC = -fPIC

# default compiler flags
V_FFLAGS =

# default compiler flags for debugging
V_FFLAGS_debug ?= -g -check bounds -traceback

# These are intel compiler options
V_FFLAGS_weak = -O1
V_FFLAGS_medium = -O2 -prec-div -prec-sqrt
V_FFLAGS_hard = -O3 -prec-div -prec-sqrt -opt-prefetch


# Local Variables:
#  mode: makefile-gmake
# End:
