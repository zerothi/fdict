# This contains default compiler options
# for the PGI vendor compiler suite.
V_F_VENDOR = pgi

# Compiler binaries
V_FC = pgfortran
V_FPP = pgfortran -E

V_MPIFC = mpif90

# Flag for output directory of modules
V_FC_MODDIR = -J

# Flag for PIC compilation
V_F_PIC = -fPIC

# default compiler flags
V_FFLAGS =

# default compiler flags for debugging
V_FFLAGS_debug = -g -Mbounds

# These are pgi compiler options
V_FFLAGS_weak = -O1
V_FFLAGS_medium = -O2
V_FFLAGS_hard = -O3


# Local Variables:
#  mode: makefile-gmake
# End:
