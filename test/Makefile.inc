test_DIR = $(TOP_DIR)/test
VPATH := $(VPATH):$(test_DIR)

# Define common codes used for the tests
TEST_COMMON = tst_utils.o

TEST_EXE_  = tst_simple tst_sp tst_dp tst_mem
TEST_EXE_ += tst_char
TEST_EXE_ += tst_type
TEST_EXE_ += tst_dict_a_ tst_dict_a1
TEST_EXE_ += tst_dict tst_dict_long
TEST_EXE_ += tst_dict_mem1 tst_dict_mem2
TEST_EXE_ += tst_dict_mem3 tst_dict_mem4
TEST_EXE_ += tst_dict_mem5
TEST_EXE_ += tst_dict_overwrite
TEST_EXE_ += tst_dict_remove
TEST_EXE_ += tst_dict_dict tst_dict_speed
TEST_EXE_ += tst_dict_hash

TEST ?= $(TEST_EXE_)

# Create the objects
TEST_OBJECTS = $(addsuffix .o, $(TEST))


ifeq ($(STATIC),1)
  TEST_LIB    = libfdict.a
else
  ifeq ($(SHARED),1)
    TEST_LIB  = libfdict.so
  else
    $(error No library is build, the tests cannot be performed.)
  endif
endif


# Create correct dependency schemes without adding too
# many flags in the direct compilation targets.
# Ensure the common utility is compiled before any test
$(TEST_OBJECTS): $(TEST_COMMON)
# Ensure the executables depend on the utility and the library
$(TEST): $(TEST_COMMON) $(TEST_LIB)
# There are *.inc files in the src directory that is
# needed
$(TEST): INCLUDES += -I$(src_DIR) -I.


.PHONY: test
.NOTPARALLEL: test
test: $(TEST_LIB) $(TEST)


tst_simple: tst_simple.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_char: tst_char.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_sp: tst_sp.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dp: tst_dp.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_mem: tst_mem.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_type: tst_type.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict: tst_dict.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_a_: tst_dict_a_.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@
tst_dict_a1: tst_dict_a1.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_speed: tst_dict_speed.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_long: tst_dict_long.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_hash: tst_dict_hash.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	-cp -f $(test_DIR)/list_words_en.dat .
	./$@
	rm -f list_words_en.dat

tst_dict_mem1: tst_dict_mem1.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_mem2: tst_dict_mem2.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_mem3: tst_dict_mem3.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_mem4: tst_dict_mem4.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_mem5: tst_dict_mem5.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_overwrite: tst_dict_overwrite.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_remove: tst_dict_remove.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@

tst_dict_dict: tst_dict_dict.o
	$(FC) -o $@ $(FFLAGS) $(INCLUDES) $^
	./$@


.PHONY: clean-test
clean-test:
	-rm -f $(TEST) $(LIB_TEST) $(TEST_COMMON) *.o *.mod

clean: clean-test


# Local Variables:
#  mode: makefile-gmake
# End:
