.SUFFIXES: .f90 .o .a

include ../arch.make

TST_UTILS = tst_utils.o
LIB_TST = libtst.a

.PHONY: default test
default: $(LIB_TST) test


$(LIB_TST): $(TST_UTILS)
	$(AR) $(ARFLAGS) $(LIB_TST) $(TST_UTILS)
	$(RANLIB) $(LIB_TST)

TST_EXE = tst_simple tst_sp tst_dp tst_mem
TST_EXE += tst_dict tst_dict_long
TST_EXE += tst_dict_mem1 tst_dict_mem2
TST_EXE += tst_dict_overwrite
TST_EXE += tst_dict_remove

LIB  = ../libvardict.a
FFLAGS += -I../

# Make all executables dependent on the test-library
$(TST_EXE): $(LIB_TST)

test: $(TST_EXE)

tst_simple: tst_simple.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_sp: tst_sp.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dp: tst_dp.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_mem: tst_mem.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict: tst_dict.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_long: tst_dict_long.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_mem1: tst_dict_mem1.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_mem2: tst_dict_mem2.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_overwrite: tst_dict_overwrite.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_remove: tst_dict_remove.o $(LIB) $(LIB_TST)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

.PHONY: clean
clean:
	-rm -f $(TST_EXE) $(LIB_TST) $(TST_UTILS) *.o *.mod
