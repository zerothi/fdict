# Define src-directory
src_DIR = $(TOP_DIR)/src
VPATH := $(VPATH):$(src_DIR)


# Define sources in this src directory
src_OBJECTS = variable.o dictionary.o
src_SOURCES = $(patsubst %.o, %.f90, $(src_OBJECTS))
src_FPP_SOURCES = variable.f90 dictionary.f90
src_MODS = $(patsubst %.f90, %.mod, $(src_SOURCES))


# Create dependency tree such that we may bypass phony targets
variable.f90: variable_pp.F90 variable.sh settings.inc var_funcs_inc.inc
	VPATH="$(TOP_DIR)" $(src_DIR)/variable.sh
	$(FPP) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) -I. $< | sed -f $(src_DIR)/filter.sed > $@

dictionary.o: variable.o
dictionary.f90: dictionary_pp.F90 dictionary.sh settings.inc dict_funcs_inc.inc
	VPATH="$(TOP_DIR)" $(src_DIR)/dictionary.sh
	$(FPP) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) -I. $< | sed -f $(src_DIR)/filter.sed > $@


# Define objects that needs compilation
SOURCES += $(src_SOURCES)
FPP_SOURCES += $(src_FPP_SOURCES)
F_MODS  += $(src_MODS)
OBJECTS += $(src_OBJECTS)


# Ensure that the objects are built with the correct module path
$(src_FPP_SOURCES): INCLUDES += -I$(src_DIR)

.PHONY: source-src
source-src: $(src_SOURCES)
	mkdir -p $(SOURCES_DIR)/src
	mv $(src_FPP_SOURCES) $(SOURCES_DIR)/src
	@rm *.inc

.PHONY: clean-src
clean-src:
	-rm -f $(src_FPP_SOURCES)
	-rm -f $(src_OBJECTS)
	-rm -f *.mod
	-rm -f $(call addpresuf,dict_,.inc, funcs interface)
	-rm -f $(call addpresuf,var_,.inc, nullify delete content funcs interface declarations declarations2)
	-rm -f $(call addpresuf,var_var_,.inc, set alloc assoc)

clean: clean-src

# Local Variables:
#  mode: makefile-gmake
# End:
