# This contains default compiler options
# for the Intel vendor compiler suite.
V_CXX_VENDOR = intel

# Compiler binaries
V_CXX = icpc
V_CXXPP = cpp -E -P -x c

V_MPICXX = mpic++

# Flag for PIC compilation
V_CXX_PIC = -fPIC

# default compiler flags
V_CXXFLAGS =

# default compiler flags for debugging
V_CXXFLAGS_debug ?= -g -check bounds -traceback

# These are intel compiler options
V_CXXFLAGS_weak = -O1
V_CXXFLAGS_medium = -O2 -prec-div -prec-sqrt
V_CXXFLAGS_hard = -O3 -prec-div -prec-sqrt -opt-prefetch


# Local Variables:
#  mode: makefile-gmake
# End:
