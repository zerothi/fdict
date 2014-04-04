.SUFFIXES: .f90 .o .a

include ../arch.make

.PHONY: default test
default: test

TEST_EXE = tests testd test_mem
TEST_EXE += test_dict test_dict_long
TEST_EXE += test_dict_mem1 test_dict_mem2
TEST_EXE += test_dict_remove

LIB  = ../libvardict.a
FFLAGS += -I../

test: $(TEST_EXE)


tests: tests.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

testd: testd.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

test_mem: test_mem.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@


test_dict: test_dict.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

test_dict_long: test_dict_long.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

test_dict_mem1: test_dict_mem1.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@
test_dict_mem2: test_dict_mem2.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

test_dict_remove: test_dict_remove.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

.PHONY: clean
clean:
	-rm -f $(TEST_EXE) *.o *.mod
