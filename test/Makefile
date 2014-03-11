.SUFFIXES: .f90 .o .a

include ../arch.make

.PHONY: default
default: test1

LIBVAR  = ../libvar.a
FFLAGS += -I../
test1: test1.o
	$(FC) -o $@ $^ $(FFLAGS) $(LIBVAR)
	./$@

.PHONY: clean
clean:
	-rm -f test1 *.o *.mod
