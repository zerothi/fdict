FC=clang
FC_SERIAL=clang

FFLAGS=-O3 -m64 -fPIC -funroll-loops -freroll-loops 

#FFLAGS += -Wunused

AR=ar
ARFLAGS = cru
RANLIB=ranlib

.F90.o:
	$(FC) -c $(INC) $(FFLAGS) $(FPPFLAGS) $< 
.f90.o:
	$(FC) -c $(INC) $(FFLAGS) $<

