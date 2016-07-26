FC=ifort
FC_SERIAL=ifort

# Optimal flags
FFLAGS= -O3 -xHost -fPIC -m64 -fp-model strict

# -warn unused
# For debugging:
FFLAGS= -O0 -g -check bounds -traceback -warn unused -fp-model strict

# Add openmp to the process
#FFLAGS += -openmp

CPP = fpp -P

AR=xiar
ARFLAGS = cru
RANLIB=ranlib
