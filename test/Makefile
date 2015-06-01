.SUFFIXES: .F90 .f90 .o .a

ARCH_MAKE_DEFAULT=../arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# Setup the default things, in case they haven't been set.
AR      ?= ar
ARFLAGS ?= cru
RANLIB  ?= ranlib
PP      ?= cpp -E -P -C

TST_UTILS = tst_utils.o
LIB_TST = libtst.a

.PHONY: default test
default: $(LIB_TST) test


$(LIB_TST): $(TST_UTILS)
	$(AR) $(ARFLAGS) $(LIB_TST) $(TST_UTILS)
	$(RANLIB) $(LIB_TST)

TST_EXE_  = tst_simple tst_sp tst_dp tst_mem
TST_EXE_ += tst_char
TST_EXE_ += tst_type
TST_EXE_ += tst_dict tst_dict_long
TST_EXE_ += tst_dict_mem1 tst_dict_mem2
TST_EXE_ += tst_dict_mem3 tst_dict_mem4
TST_EXE_ += tst_dict_overwrite
TST_EXE_ += tst_dict_remove
TST_EXE_ += tst_dict_dict

TST_EXE ?= $(TST_EXE_)

LIB  = ../libvardict.a $(LIB_TST)
FFLAGS += -I../

# Make all executables dependent on the test-library
$(TST_EXE): $(LIB_TST)

test: $(TST_EXE)

tst_simple: tst_simple.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_char: tst_char.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_sp: tst_sp.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dp: tst_dp.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_mem: tst_mem.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_type: tst_type.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict: tst_dict.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_long: tst_dict_long.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_mem1: tst_dict_mem1.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_mem2: tst_dict_mem2.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_mem3: tst_dict_mem3.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_mem4: tst_dict_mem4.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_overwrite: tst_dict_overwrite.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_remove: tst_dict_remove.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

tst_dict_dict: tst_dict_dict.o $(LIB)
	$(FC) -o $@ $(FFLAGS) $^
	./$@

.PHONY: clean
clean:
	-rm -f $(TST_EXE) $(LIB_TST) $(TST_UTILS) *.o *.mod
