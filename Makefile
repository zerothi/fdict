
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
include src/Makefile.inc

# Libraries depend on the objects
$(LIBRARIES): $(OBJECTS)

# Create target
lib: $(LIBRARIES)

# Include the makefile in the test directory
include test/Makefile.inc

