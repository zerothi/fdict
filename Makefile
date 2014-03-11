.SUFFIXES: .f90 .o .a
include ./arch.make

.PHONY: default
default: lib

# The different libraries
VAR_OBJS  = var.o

LIBVAR  = libvar.a

.PHONY: lib
lib: $(LIBVAR)

$(LIBVAR): $(VAR_OBJS)
	$(AR) $(ARFLAGS) $(LIBVAR) $(VAR_OBJS)
	$(RANLIB) $(LIBVAR)

.PHONY: test
test: lib
	(cd test ; $(MAKE))

.PHONY: prep_var
prep_var:
	./var.sh
	fpp -P var.F90 | sed -e "s/NEWLINE/\n/g" > tmp.F90 
	fpp -P tmp.F90 var.f90

.PHONY: clean
clean:
	-rm -f $(VAR_OBJS) $(LIBVAR) *.o *.mod tmp.F90 var.f90
	-rm -f mods.inc nullify.inc delete.inc types.inc funcs.inc

# Dependencies
var.o: | prep_var
