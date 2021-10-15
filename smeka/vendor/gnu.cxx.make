# This contains default compiler options
# for the GNU vendor compiler suite.
V_CXX_VENDOR = gnu

# Compiler binaries
V_CXX = g++
V_CXXPP = cpp -E -P -x c

V_MPICXX = mpic++

# Flag for PIC compilation
V_CXX_PIC = -fPIC

# default compiler flags
V_CXXFLAGS =

# default compiler flags for debugging
V_CXXFLAGS_debug = -g -fbacktrace -fbounds-check -Wunused-variable -Warray-temporaries

# These are gcc compiler options
V_CXXFLAGS_weak = -O1 -funroll-loops
V_CXXFLAGS_medium = -O2 -funroll-loops -ftree-vectorize -fprefetch-loop-arrays
V_CXXFLAGS_hard = -O3 -funroll-loops -ftree-vectorize -fprefetch-loop-arrays -fgraphite -fselective-scheduling


# Local Variables:
#  mode: makefile-gmake
# End:
