_SMEKA_cpp = 1

# This smeka file creates the preprocessor
# settings.

# Default pre-processor commands
# (probably we should do a check to ensure
#  it works)
CPP ?= cpp
# Sadly there are many variations of CPP
#   -C is needed to disregard C-comments (the operator // is a comment!?!)
#   -nostdinc is needed to not include standard includes (which are C comments)
#   -E only preprocess, do not try and compile
#   -P do not add line markers
# clang-3.5 (MacOSx): -E -EP -xc
# clang-3.7> (MacOSx): -E -P -x c
# cpp(gnu): -E -P -x c -nostdinc
CPPFLAGS ?= -E -P -x c

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

.PHONY: smeka-show-cpp
.NOTPARALLEL: smeka-show-cpp
smeka-show-cpp:
	@echo "  CPP      = $(CPP)"
	@echo "  CPPFLAGS = $(CPPFLAGS)"
	@echo "  INCLUDES = $(INCLUDES)"
show: smeka-show-cpp

# Local Variables:
#  mode: makefile-gmake
# End:
