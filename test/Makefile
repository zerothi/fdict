.SUFFIXES: .f90 .o .a

include ../arch.make

.PHONY: default test
default: test

test: tests testd test_dict test_dict_long

LIB  = ../libvardict.a
FFLAGS += -I../

tests: tests.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

testd: testd.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@


test_dict: test_dict.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

test_dict_long: test_dict_long.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

.PHONY: clean
clean:
	-rm -f tests testd test_dict *.o *.mod
