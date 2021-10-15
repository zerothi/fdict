# This contains default compiler options
# for the GNU vendor compiler suite.
V_F_VENDOR = gnu

# Compiler binaries
V_FC = gfortran
V_FPP = gfortran -E -P -x c

V_MPIFC = mpif90

# Flag for output directory of modules
V_FC_MODDIR = -J

# Flag for PIC compilation
V_F_PIC = -fPIC

# default compiler flags
V_FFLAGS =

# default compiler flags for debugging
V_FFLAGS_debug = -g -fbacktrace -fbounds-check -Wunused-variable -Warray-temporaries

# These are gcc compiler options
V_FFLAGS_weak = -O1 -funroll-loops
V_FFLAGS_medium = -O2 -funroll-loops -ftree-vectorize -fprefetch-loop-arrays
V_FFLAGS_hard = -O3 -funroll-loops -ftree-vectorize -fprefetch-loop-arrays -fgraphite -fselective-scheduling


# Local Variables:
#  mode: makefile-gmake
# End:
