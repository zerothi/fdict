.SUFFIXES: .f90 .o .a

ARCH_MAKE_DEFAULT=arch.make
ARCH_MAKE?=$(ARCH_MAKE_DEFAULT)
include $(ARCH_MAKE)

# Setup the default things, in case they haven't been set.
AR      ?= ar
ARFLAGS ?= cru
RANLIB  ?= ranlib
PP      ?= cpp -E -P -C

VPATH   ?= $(shell pwd)

.PHONY: default
default: lib

# By compiling with 
#  make PIPE_SILENT=
# all the preprocessing will be shown on stdout.
# This is handy for debugging.
PIPE_SILENT ?= 2> /dev/null

# The different libraries
OBJS = variable.o iso_var_str.o dictionary.o

LIB  = libvardict.a

.PHONY: lib
lib: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

.PHONY: test
test: lib
	$(MAKE) -C test

.PHONY: prep-var prep-dict
prep-var:
	$(VPATH)/var.sh
	$(PP) -I. -I$(VPATH) $(VPATH)/variable_pp.F90 \
		| sed -f filter.sed > tmp.F90 $(PIPE_SILENT)
	$(PP) -I. -I$(VPATH) tmp.F90 \
		| sed -f filter.sed > variable.f90 $(PIPE_SILENT)

prep-dict:
	$(VPATH)/dictionary.sh
	$(PP) -I. -I$(VPATH) $(VPATH)/dictionary_pp.F90 \
		| sed -f filter.sed > tmp.F90 $(PIPE_SILENT)
	$(PP) -I. -I$(VPATH) tmp.F90 \
		| sed -f filter.sed > dictionary.f90 $(PIPE_SILENT)

.PHONY: clean
clean:
	-rm -f $(OBJS) $(LIB) *.s *.o *.mod tmp.F90 variable.f90 dictionary.f90
	-rm -f dict_funcs.inc dict_interface.inc
	-rm -f var_nullify.inc var_delete.inc var_content.inc var_funcs.inc var_interface.inc
	-rm -f var_var_set.inc var_var_alloc.inc var_var_assoc.inc
	-rm -f var_declarations.inc var_declarations2.inc
	$(MAKE) -C test clean

# Dependencies
dictionary.f90: | prep-dict
dictionary.o: variable.o | prep-dict
variable.f90: | prep-var
variable.o: iso_var_str.o | prep-var
