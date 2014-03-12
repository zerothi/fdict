.SUFFIXES: .f90 .o .a

include ../arch.make

.PHONY: default test
default: test

test: tests testd

LIBVAR  = ../libvar.a
FFLAGS += -I../

tests: tests.o $(LIBVAR)
	$(FC) -o $@ $^ $(FFLAGS) $(LIBVAR)
	./$@

testd: testd.o $(LIBVAR)
	$(FC) -o $@ $^ $(FFLAGS) $(LIBVAR)
	./$@

.PHONY: clean
clean:
	-rm -f tests testd *.o *.mod
