# This contains default compiler options
# for the PGI vendor compiler suite.
V_CXX_VENDOR = pgi

# Compiler binaries
V_CXX = pgc++
V_CXXPP = pgcc -E

V_MPICXX = mpic++

# Flag for PIC compilation
V_CXXPIC = -fPIC

# default compiler flags
V_CXXFLAGS =

# default compiler flags for debugging
V_CXXFLAGS_debug = -g -Mbounds

# These are pgi compiler options
V_CXXFLAGS_weak = -O1
V_CXXFLAGS_medium = -O2
V_CXXFLAGS_hard = -O3


# Local Variables:
#  mode: makefile-gmake
# End:
