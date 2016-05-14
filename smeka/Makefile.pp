_SMEKA_pp = 1

# This smeka file creates the preprocessor
# settings.

# Default pre-processor commands
# (probably we should do a check to ensure
#  it works)
CPP ?= cpp
CPPFLAGS ?= -E -P -C -nostdinc

ifeq ($(uname_S),Linux)
endif
ifeq ($(uname_S),Darwin)
endif
ifeq ($(uname_S),SunOS)
endif
ifeq ($(uname_S),FreeBSD)
endif
ifeq ($(uname_S),OpenBSD)
endif
ifeq ($(uname_S),NetBSD)
endif
ifeq ($(uname_S),Cygwin)
endif

.PHONY: smeka-show-pp
.NOTPARALLEL: smeka-show-pp
smeka-show-pp:
	@echo "  CPP      = $(CPP)"
	@echo "  CPPFLAGS = $(CPPFLAGS)"
	@echo "  INCLUDES = $(INCLUDES)"
show: smeka-show-pp

# Local Variables:
#  mode: makefile-gmake
# End:
