FC=gfortran
FFLAGS = -g

.F90.o:
	$(FC) -c $(INC) $(FFLAGS) $<

.f90.o:
	$(FC) -c $(INC) $(FFLAGS) $<

