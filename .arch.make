FC=gfortran
FFLAGS = -g 

.F90.o:
	$(FC) -c $(INC) $(FFLAGS) $(FPPFLAGS) $<

.f90.o:
	$(FC) -c $(INC) $(FFLAGS) $<

