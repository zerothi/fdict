_SMEKA_pp = 1

# This smeka file creates the preprocessor
# settings.

# Default pre-processor commands are defined in
#    Makefile.default
# (probably we should do a check to ensure
#  it works)
# Sadly there are many variations of CPP
#   -C is needed to disregard C-comments (the operator // is a comment!?!)
#   -nostdinc is needed to not include standard includes (which are C comments)
#   -E only preprocess, do not try and compile
#   -P do not add line markers
# clang-3.5 (MacOSx): -E -xc
# clang-3.7> (MacOSx): -E -P -x c
# cpp(gnu): -E -P -x c -nostdinc


.PHONY: smeka-show-pp
.NOTPARALLEL: smeka-show-pp
smeka-show-pp:
	@echo "  CPP      = $(CPP)"
	@echo "  CPPFLAGS = $(CPPFLAGS)"
	@echo "  FPP      = $(FPP)"
	@echo "  FPPFLAGS = $(FPPFLAGS)"
	@echo "  INCLUDES = $(INCLUDES)"

show: smeka-show-pp

# Local Variables:
#  mode: makefile-gmake
# End:
