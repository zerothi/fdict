# Define src-directory
src_DIR = $(TOP_DIR)/src
VPATH := $(VPATH):$(src_DIR)


# Define sources in this src directory
src_OBJECTS = fdict_types.o variable.o dictionary.o
src_SOURCES = $(patsubst %.o, %.f90, $(src_OBJECTS))
src_FPP_SOURCES = fdict_types.f90 variable.f90 dictionary.f90
src_MODS = $(patsubst %.f90, %.mod, $(src_SOURCES))

variable.o: fdict_types.o
dictionary.o: variable.o fdict_types.o

# Create dependency tree such that we may bypass phony targets
fdict_types.f90: fdict_types.fypp
	$(FYPP) $(FYPPFLAGS) $< > $@

variable.f90: variable.fypp
	$(FYPP) $(FYPPFLAGS) $< > $@

dictionary.f90: dictionary.fypp
	$(FYPP) $(FYPPFLAGS) $< > $@

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

clean: clean-src

# Local Variables:
#  mode: makefile-gmake
# End:
