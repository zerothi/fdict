# Define src-directory
SRC_DIR = $(TOP_DIR)/src
VSRC_DIR = $(VPATH)/$(SRC_DIR)


# Define sources in this src directory
SRC_CPP_SOURCES = $(addprefix $(VSRC_DIR)/,variable.f90 dictionary.f90)
SRC_SOURCES = $(addprefix $(VSRC_DIR)/,iso_var_str.f90) $(SRC_CPP_SOURCES)
SRC_OBJECTS = $(patsubst %.f90, %.o, $(SRC_SOURCES))


# Create dependency tree such that we may bypass phony targets
$(VSRC_DIR)/variable.o: $(VSRC_DIR)/iso_var_str.o
$(VSRC_DIR)/variable.f90: $(addprefix $(VSRC_DIR)/,variable.sh settings.inc var_funcs_inc.inc variable_pp.F90)
	cd $(VSRC_DIR) ; ./variable.sh
	$(CPP) $(CPPFLAGS) $(INCLUDES) -I$(VSRC_DIR) $(VSRC_DIR)/variable_pp.F90 \
		| sed -f $(VSRC_DIR)/filter.sed > $(VSRC_DIR)/variable.f90 $(PIPE_SILENT)

$(VSRC_DIR)/dictionary.o: $(VSRC_DIR)/variable.o
$(VSRC_DIR)/dictionary.f90: $(addprefix $(VSRC_DIR)/,dictionary.sh settings.inc dict_funcs_inc.inc dictionary_pp.F90)
	cd $(VSRC_DIR) ; ./dictionary.sh
	$(CPP) $(CPPFLAGS) $(INCLUDES) -I$(VSRC_DIR) $(VSRC_DIR)/dictionary_pp.F90 \
		| sed -f $(VSRC_DIR)/filter.sed > $(VSRC_DIR)/dictionary.f90 $(PIPE_SILENT)

.PHONY: clean-src
clean-src:
	-rm -f $(addprefix $(VSRC_DIR)/, *.o *.mod)
	-rm -f $(addprefix $(VSRC_DIR)/, variable.f90 dictionary.f90)
	-rm -f $(call addpresuf,$(VSRC_DIR)/dict_,.inc, funcs interface)
	-rm -f $(call addpresuf,$(VSRC_DIR)/var_,.inc, nullify delete content funcs interface declarations declarations2)
	-rm -f $(call addpresuf,$(VSRC_DIR)/var_var_,.inc, set alloc assoc)
clean: clean-src


# Define objects that needs compilation
SOURCES += $(SRC_SOURCES)
CPP_SOURCES += $(SRC_CPP_SOURCES)
F_MODS  += $(patsubst %.f90, %.mod, $(SRC_SOURCES))
OBJECTS += $(SRC_OBJECTS)

# Ensure that the objects are built with the correct module path
$(SRC_OBJECTS): FFLAGS += $(FC_MODDIR)$(VSRC_DIR)

# Local Variables:
#  mode: makefile-gmake
# End:
