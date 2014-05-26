.SUFFIXES: .f90 .o .a
include arch.make

VPATH?=$(shell pwd)

.PHONY: default
default: lib

# The different libraries
OBJS  = variable.o iso_var_str.o dictionary.o

LIB  = libvardict.a

.PHONY: lib
lib: $(LIB)

$(LIB): $(OBJS)
	$(AR) $(ARFLAGS) $(LIB) $(OBJS)
	$(RANLIB) $(LIB)

.PHONY: test
test: lib
	(cd test ; $(MAKE))

.PHONY: prep
SED_DEL = 's/NEWLINE/\n/g;/^$$/d;/^\!.*&/d;\
s/[[:space:]]*\#\#[[:space:]]*\([^[:space:]]*\)/\1/g;\
s/[[:space:]]*\#\([^i][^[:space:]]*\)/"\1"/g;\
s/"endif"/\n\#endif/g'
prep:
	$(VPATH)/var.sh
	$(PP) -I$(VPATH) $(VPATH)/variable.F90 | sed -e $(SED_DEL) > tmp.F90 #2> /dev/null
	$(PP) -I$(VPATH) tmp.F90 | sed -e $(SED_DEL) > variable.f90 #2> /dev/null
	$(VPATH)/dictionary.sh
	$(PP) -I$(VPATH) $(VPATH)/dictionary.F90 | sed -e $(SED_DEL) > tmp.F90 2> /dev/null
	$(PP) -I$(VPATH) tmp.F90 | sed -e $(SED_DEL) > dictionary.f90 2> /dev/null

.PHONY: clean
clean:
	-rm -f $(OBJS) $(LIB) *.s *.o *.mod tmp.F90 variable.f90 dictionary.f90
	-rm -f dict_funcs.inc dict_interface.inc
	-rm -f var_nullify.inc var_delete.inc var_content.inc var_funcs.inc var_interface.inc
	-rm -f var_var_set.inc var_var_alloc.inc var_var_assoc.inc
	(cd test ; $(MAKE) clean)

# Dependencies
dictionary.f90: | prep
dictionary.o: variable.o | prep
variable.f90: | prep
variable.o: iso_var_str.o | prep
