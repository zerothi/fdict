# Make CMake run the scripts for creating the input files

# Ensure that the cpp flags are added when preprocessing the fortran files
# This currently does not work
set_source_files_properties(
  variable.f90 dictionary.f90
  PROPERTIES Fortran_PREPROCESS ON
  )

add_custom_target(variable_pp.F90
  DEPENDS variable_declarations_.inc variable_declarations2_.inc variable_interface_.inc variable_delete_.inc
  variable_variable_alloc_.inc variable_variable_assoc_.inc variable_funcs_.inc
  )

# Here we create the variable include files
add_custom_command(
  OUTPUT variable_declarations_.inc variable_declarations2_.inc variable_interface_.inc variable_delete_.inc
         variable_variable_alloc_.inc variable_variable_assoc_.inc variable_funcs_.inc
  # Command to run to create include file defaults	 
  COMMAND VPATH=${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/variable.sh
  VERBATIM
  COMMENT "Creating variable*.inc files"
)

add_custom_command(
  OUTPUT variable.f90
  DEPENDS variable_declarations_.inc variable_declarations2_.inc variable_interface_.inc variable_delete_.inc
          variable_variable_alloc_.inc variable_variable_assoc_.inc variable_funcs_.inc
  COMMAND gfortran -E -P -x c -I${CMAKE_CURRENT_LIST_DIR} -I${CMAKE_CURRENT_BINARY_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/variable_pp.F90 | sed -f ${CMAKE_CURRENT_SOURCE_DIR}/filter.sed > variable.f90
  VERBATIM
  COMMENT "Preprocessing variable.f90"
  )


# Here we create the dictionary include files
add_custom_command(
  OUTPUT dictionary_interface_.inc dictionary_funcs_.inc
  COMMAND VPATH=${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/dictionary.sh
  VERBATIM
  COMMENT "Creating dictionary*.inc files"
  )

add_custom_command(
  OUTPUT dictionary.f90
  DEPENDS dictionary_interface_.inc dictionary_funcs_.inc
  COMMAND gfortran -E -P -I${CMAKE_CURRENT_LIST_DIR} -I. ${CMAKE_CURRENT_SOURCE_DIR}/dictionary_pp.F90 | sed -f ${CMAKE_CURRENT_SOURCE_DIR}/filter.sed > dictionary.f90
  VERBATIM
  COMMENT "Preprocessing dictionary.f90"
  )


# Generate the include file for discovering versions
configure_file(fdict-template.inc fdict.inc @ONLY)

# Finally define the fdict library
add_library(fdict
  variable.f90
  dictionary.f90
  )

# Attach a property to the fdict library
set_target_properties(fdict
  PROPERTIES
  PUBLIC_HEADERS ${CMAKE_CURRENT_BINARY_DIR}/fdict.inc
  )

