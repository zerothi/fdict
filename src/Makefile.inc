# Define src-directory
src_DIR = $(TOP_DIR)/src
VPATH := $(VPATH):$(src_DIR)


# Define sources in this src directory
src_OBJECTS = variable.o dictionary.o
src_SOURCES = $(patsubst %.o, %.f90, $(src_OBJECTS))
src_FPP_SOURCES = variable.f90 dictionary.f90
src_MODS = $(patsubst %.f90, %.mod, $(src_SOURCES))


# Create dependency tree such that we may bypass phony targets
variable.f90: variable_pp.F90 variable.sh settings.inc variable_funcs_inc.inc
	VPATH="$(TOP_DIR)" $(src_DIR)/variable.sh
	$(FPP) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) -I. $< | sed -f $(src_DIR)/filter.sed > $@

dictionary.o: variable.o
dictionary.f90: dictionary_pp.F90 dictionary.sh settings.inc dictionary_funcs_inc.inc
	VPATH="$(TOP_DIR)" $(src_DIR)/dictionary.sh
	$(FPP) $(FPPFLAGS) $(INCLUDES) -I$(src_DIR) -I. $< | sed -f $(src_DIR)/filter.sed > $@

fdict.inc:
	@$(ECHO) "#ifndef _FDICT_INCLUDE_DEFINED" > $@
	@$(ECHO) "#define _FDICT_INCLUDE_DEFINED" >> $@
	@$(ECHO) "#define _FDICT_MAJOR_ $(PROJECT_MAJOR)" >> $@
	@$(ECHO) "#define _FDICT_MINOR_ $(PROJECT_MINOR)" >> $@
	@$(ECHO) "#define _FDICT_MICRO_ $(PROJECT_MICRO)" >> $@
	@$(ECHO) "#define _FDICT_VERSION_ $(PROJECT_VERSION)" >> $@
	@$(ECHO) "#endif" >> $@

# Define objects that needs compilation
SOURCES += $(src_SOURCES) fdict.inc
FPP_SOURCES += $(src_FPP_SOURCES)
F_MODS  += $(src_MODS)
OBJECTS += $(src_OBJECTS)


# Ensure that the objects are built with the correct module path
$(src_FPP_SOURCES): INCLUDES += -I$(src_DIR)

.PHONY: source-src
source-src: $(src_SOURCES) fdict.inc
	$(MKDIR) $(MKDIR_FLAG_PARENT) $(SOURCES_DIR)/src
	mv $(src_SOURCES) fdict.inc $(SOURCES_DIR)/src


.PHONY: clean-src
clean-src:
	-$(RM) $(RM_FLAG_FORCE) $(src_FPP_SOURCES)
	-$(RM) $(RM_FLAG_FORCE) $(src_OBJECTS)
	-$(RM) $(RM_FLAG_FORCE) *.mod fdict.inc
	-$(RM) $(RM_FLAG_FORCE) $(call addpresuf,dictionary_,_.inc, funcs interface)
	-$(RM) $(RM_FLAG_FORCE) $(call addpresuf,variable_,_.inc, nullify delete content funcs interface declarations declarations2)
	-$(RM) $(RM_FLAG_FORCE) $(call addpresuf,variable_variable_,_.inc, set alloc assoc)

clean: clean-src

# Local Variables:
#  mode: makefile-gmake
# End:
