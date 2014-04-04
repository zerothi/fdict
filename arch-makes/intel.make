FC=ifort
FC_SERIAL=ifort

# Optimal flags
FFLAGS= -O3 -xHost -fPIC -m64 -fp-model strict

# -warn unused
# For debugging:
FFLAGS= -O0 -g -check bounds -traceback -warn unused -fp-model strict


# Add openmp to the process
#FFLAGS += -openmp

PP = fpp -P

AR=xiar
ARFLAGS = cru
RANLIB=ranlib

.F90.o:
	$(FC) -c $(INC) $(FFLAGS) $(FPPFLAGS) $< 
.f90.o:
	$(FC) -c $(INC) $(FFLAGS) $<

