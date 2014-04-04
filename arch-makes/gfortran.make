FC=gfortran
FC_SERIAL=gfortran

# Optimal flags
FFLAGS=-O3 -m64 -fPIC -fno-second-underscore -ftree-vectorize -fexpensive-optimizations \
	-finline-functions-called-once -funroll-loops -fvariable-expansion-in-unroller \
	-ftree-loop-optimize -frename-registers -fprefetch-loop-arrays -finline-small-functions \
	-fipa-pure-const -foptimize-sibling-calls -fipa-cp

# Default flags
FFLAGS=-O2 -m64 -fPIC

# Add openmp to the process
#FFLAGS += -fopenmp

# This is for debugging purposes
#FFLAGS = -g -O0 -Warray-bounds -Wunused

PP = gcc -E -P -C 

AR=ar
ARFLAGS = cru
RANLIB=ranlib

.F90.o:
	$(FC) -c $(INC) $(FFLAGS) $(FPPFLAGS) $< 
.f90.o:
	$(FC) -c $(INC) $(FFLAGS) $<

