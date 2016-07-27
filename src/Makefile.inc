# Define src-directory
src_DIR = $(TOP_DIR)/src
VPATH := $(VPATH):$(src_DIR)


# Define sources in this src directory
src_CPP_SOURCES = variable.f90 dictionary.f90
src_SOURCES = iso_var_str.f90 $(src_CPP_SOURCES)
src_OBJECTS = $(patsubst %.f90, %.o, $(src_SOURCES))


# Create dependency tree such that we may bypass phony targets
variable.o: iso_var_str.o
variable.f90: variable.sh settings.inc var_funcs_inc.inc variable_pp.F90
	cd $(src_DIR) && ./variable.sh
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) $(src_DIR)/variable_pp.F90 \
		| sed -f $(src_DIR)/filter.sed > variable.f90 $(PIPE_SILENT)

dictionary.o: variable.o
dictionary.f90: dictionary.sh settings.inc dict_funcs_inc.inc dictionary_pp.F90
	cd $(src_DIR) && ./dictionary.sh
	$(CPP) $(CPPFLAGS) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) $(src_DIR)/dictionary_pp.F90 \
		| sed -f $(src_DIR)/filter.sed > dictionary.f90 $(PIPE_SILENT)

.PHONY: clean-src
clean-src:
	-rm -f *.o *.mod
	-rm -f variable.f90 dictionary.f90
	-rm -f $(call addpresuf,$(src_DIR)/dict_,.inc, funcs interface)
	-rm -f $(call addpresuf,$(src_DIR)/var_,.inc, nullify delete content funcs interface declarations declarations2)
	-rm -f $(call addpresuf,$(src_DIR)/var_var_,.inc, set alloc assoc)
clean: clean-src


# Define objects that needs compilation
SOURCES += $(src_SOURCES)
CPP_SOURCES += $(src_CPP_SOURCES)
F_MODS  += $(patsubst %.f90, %.mod, $(src_SOURCES))
OBJECTS += $(src_OBJECTS)

# Ensure that the objects are built with the correct module path
$(src_OBJECTS): INCLUDES += -I$(src_DIR)


# Local Variables:
#  mode: makefile-gmake
# End:
