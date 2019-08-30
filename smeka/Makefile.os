_SMEKA_os = 1

# Detect and create smeka variable for OS
# detection.
# These commands are heavily inspired by
# the git makefile.
# See: git.kernel.org/.../Makefile

MKDIR ?= mkdir
MKDIR_FLAG_PARENT ?= -p
CP ?= cp
RM ?= rm
RM_FLAG_FORCE ?= -f
RM_FLAG_RECURSE ?= -r
SLEEP ?= sleep
SED ?= sed
ECHO ?= echo

SMEKA_uname_S := $(shell sh -c 'uname -s 2>/dev/null || $(ECHO) not')
SMEKA_uname_M := $(shell sh -c 'uname -m 2>/dev/null || $(ECHO) not')
SMEKA_uname_O := $(shell sh -c 'uname -o 2>/dev/null || $(ECHO) not')
SMEKA_uname_R := $(shell sh -c 'uname -r 2>/dev/null || $(ECHO) not')
SMEKA_uname_P := $(shell sh -c 'uname -p 2>/dev/null || $(ECHO) not')
SMEKA_uname_V := $(shell sh -c 'uname -v 2>/dev/null || $(ECHO) not')

# Local Variables:
#  mode: makefile-gmake
# End:
