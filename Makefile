
# Define VPATH
VPATH ?= $(shell pwd)

# Define default target:
default: lib

# SMEKASETTINGS (DO NOT DELETE)
# DO NOT CHANGE CONTENT IN THIS BLOCK
# IT MAY BE OVERWRITTEN WHEN REINSTALLING SMEKA
#
# This Makefile was created by smeka:
#  github.com/zerothi/smeka

# Top-directory of Makefile/source tree
# If need set, do so ABOVE this block!
TOP_DIR ?= .

# Directory of smeka default Makefiles
SMEKA_DIR = smeka

# Include the smeka settings!
include $(TOP_DIR)/$(SMEKA_DIR)/Makefile.smeka

# SMEKAENDSETTINGS (DO NOT DELETE)


# Define the creation of the inclusion data
fdict.inc:
	@$(ECHO) "Creating $@ version inclusion file"
	@$(ECHO) "! fdict version inclusion" > $@
	@$(ECHO) "#ifndef _FDICT_INCLUDE_DEFINED" >> $@
	@$(ECHO) "#define _FDICT_INCLUDE_DEFINED" >> $@
	@$(ECHO) "#define _FDICT_MAJOR_ $(PROJECT_VERSION_MAJOR)" >> $@
	@$(ECHO) "#define _FDICT_MINOR_ $(PROJECT_VERSION_MINOR)" >> $@
	@$(ECHO) "#define _FDICT_PATCH_ $(PROJECT_VERSION_PATCH)" >> $@
	@$(ECHO) "#define _FDICT_VERSION_ $(PROJECT_VERSION)" >> $@
	@$(ECHO) "#endif" >> $@


# Dependent on the option we can fake a VPATH to contain
# any pre-created sources, if they exist we can simply use those
SOURCES_DIR = $(TOP_DIR)/sources

# Include the makefile in the src directory
include $(TOP_DIR)/src/Makefile.inc

# Libraries depend on the objects
$(LIBRARIES): $(OBJECTS)

# Create target
lib: fdict.inc fdict.fypp $(LIBRARIES)

# Include the makefile in the test directory
include $(TOP_DIR)/test/Makefile.inc


##
# This handy target copies from the SOURCES_DIR all sources
# to the current directory
# But ONLY if the current directory is not the top of the project
.PHONY: copy
ifeq ($(TOP_DIR),.)
copy:
	@$(ECHO) ""
	@$(ECHO) "make copy does not work when executed from the top fdict directory"
	@$(ECHO) "Please create an object directory with an appropriate Makefile"
	@$(ECHO) ""
else
copy:
	$(CP) $(SOURCES_DIR)/src/*.f90 $(SOURCES_DIR)/src/*.inc $(SOURCES_DIR)/*.inc .
endif


# Create source target for creating _only_ the sources.
.PHONY: source
source: fdict-inc source-src
	$(MKDIR) $(MKDIR_FLAG_PARENT) $(SOURCES_DIR)
	mv fdict.inc $(SOURCES_DIR)

##
# Distribution targets for creating the distribution of flook
# Create distribution for releases
.PHONY: dist-fdict
dist-fdict:
	git archive --format=tar --prefix fdict-$(PROJECT_VERSION)/ HEAD > fdict-$(PROJECT_VERSION).tar
# Force the creation of the 3 pre-defined source directories
	$(MAKE) source
# Clean up
	-$(RM) $(RM_FLAG_FORCE) *.inc
	tar --transform 's,^,fdict-$(PROJECT_VERSION)/,' -rf fdict-$(PROJECT_VERSION).tar sources*
	-@$(RM) $(RM_FLAG_FORCE) fdict-$(PROJECT_VERSION).tar.gz
	gzip fdict-$(PROJECT_VERSION).tar

dist: dist-fdict
