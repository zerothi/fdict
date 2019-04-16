
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

# Include the makefile in the src directory
include $(TOP_DIR)/src/Makefile.inc

# Libraries depend on the objects
$(LIBRARIES): $(OBJECTS)

# Create target
lib: settings.bash fdict.inc $(LIBRARIES)


# Include the makefile in the test directory
include $(TOP_DIR)/test/Makefile.inc


##
# This handy target copies from the SOURCES_DIR all sources
# to the current directory
# But ONLY if the current directory is not the top of the project
.PHONY: copy
ifeq ($(TOP_DIR),.)
copy:
	@echo ""
	@echo "make copy does not work when executed from the top fdict directory"
	@echo "Please create an object directory with an appropriate Makefile"
	@echo ""
else
copy:
	cp $(SOURCES_DIR)/src/*.f90 $(SOURCES_DIR)/src/*.inc .
endif

# Create source target for creating _only_ the sources.
.PHONY: source
source: source-src

# Dependent on the option we can fake a VPATH to contain
# any pre-created sources, if they exist we can simply use those
SOURCES_DIR = $(TOP_DIR)/sources

# Create target
source: source-src

##
# Distribution targets for creating the distribution of flook
# Create distribution for releases
.PHONY: dist-fdict
dist-fdict:
	git archive --format=tar --prefix fdict-$(PROJECT_VERSION)/ HEAD > fdict-$(PROJECT_VERSION).tar
# Force the creation of the 3 pre-defined source directories
	$(MAKE) source
# Clean up
	-rm -f *.inc
	tar --transform 's,^,fdict-$(PROJECT_VERSION)/,' -rf fdict-$(PROJECT_VERSION).tar sources*
	-@rm -f fdict-$(PROJECT_VERSION).tar.gz
	gzip fdict-$(PROJECT_VERSION).tar

dist: dist-fdict

