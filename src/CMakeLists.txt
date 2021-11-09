add_library(fdict)

# Make CMake run the scripts for creating the input files

# Here we create the variable include files
add_custom_command(
  OUTPUT variable_declarations_.inc variable_declarations2_.inc variable_interface_.inc variable_delete_.inc
         variable_variable_alloc_.inc variable_variable_assoc_.inc variable_funcs_.inc
  # Command to run to create include file defaults	 
  COMMAND VPATH=${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/src/variable.sh
  VERBATIM
)

add_custom_command(
  OUTPUT variable.f90
  DEPENDS variable_declarations_.inc variable_declarations2_.inc variable_interface_.inc variable_delete_.inc
          variable_variable_alloc_.inc variable_variable_assoc_.inc variable_funcs_.inc
  COMMAND gfortran -E -P -x c -I${CMAKE_CURRENT_LIST_DIR} -I. ${CMAKE_SOURCE_DIR}/src/variable_pp.F90 | sed -f ${CMAKE_SOURCE_DIR}/src/filter.sed > variable.f90
  VERBATIM
  )

# Here we create the dictionary include files
add_custom_command(
  OUTPUT dictionary_interface_.inc dictionary_funcs_.inc
  COMMAND VPATH=${CMAKE_SOURCE_DIR} ${CMAKE_SOURCE_DIR}/src/dictionary.sh
  VERBATIM
  )

add_custom_command(
  OUTPUT dictionary.f90
  DEPENDS dictionary_interface_.inc dictionary_funcs_.inc
  COMMAND gfortran -E -P -I${CMAKE_CURRENT_LIST_DIR} -I. ${CMAKE_SOURCE_DIR}/src/dictionary_pp.F90 | sed -f ${CMAKE_SOURCE_DIR}/src/filter.sed > dictionary.f90
  VERBATIM
  )


# Generate the include file for discovering versions
configure_file(fdict-template.inc fdict.inc @ONLY)

target_sources(fdict
  PRIVATE
    variable.f90
    dictionary.f90
  )
