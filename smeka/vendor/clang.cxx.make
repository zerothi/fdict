# This contains default compiler options
# for the GNU vendor compiler suite.
V_CXX_VENDOR = clang

# Compiler binaries
V_CXX = clang
V_CXXPP = clang -E -P -x c

V_MPICXX = mpic++

# Flag for PIC compilation
V_CXX_PIC = -fPIC

# default compiler flags
V_CXXFLAGS =

# default compiler flags for debugging
V_CXXFLAGS_debug = -g -fbacktrace -fbounds-check -Wunused-variable -Warray-temporaries

# These are clang compiler options
V_CXXFLAGS_weak = -O1
V_CXXFLAGS_medium = -O2
V_CXXFLAGS_hard = -O3


# Local Variables:
#  mode: makefile-gmake
# End:
