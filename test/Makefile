.SUFFIXES: .f90 .o .a

include ../arch.make

.PHONY: default test
default: test

TEST_EXE = tests testd test_mem
TEST_EXE += test_dict test_dict_long test_dict_mem


test: $(TEST_EXE)

LIB  = ../libvardict.a
FFLAGS += -I../

tests: tests.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

testd: testd.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

test_mem: test_mem.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@


test_dict: test_dict.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

test_dict_long: test_dict_long.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

test_dict_mem: test_dict_mem.o $(LIB)
	$(FC) -o $@ $^ $(FFLAGS) $(LIB)
	./$@

.PHONY: clean
clean:
	-rm -f $(TEST_EXE) *.o *.mod
