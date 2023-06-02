# Make CMake run the scripts for creating the input files

# The output directory of the module files
set(LIB_MOD_DIR "${CMAKE_CURRENT_BINARY_DIR}/modules")
if(NOT EXISTS "${LIB_MOD_DIR}")
  make_directory("${LIB_MOD_DIR}")
endif()


# Do the preprocessing
fdict_fyppify(
	EXTIN "fypp" EXTOUT "f90"
	FLAGS "${FYPPFLAGS}"
	COMMENT "fyppifying source codes"
	FILES
	  fdict_types.fypp
	  variable.fypp
	  dictionary.fypp
	OUTPUT fdict_sources
)


# Also fyppify the global inclusion file
fdict_fyppify(
	EXTIN "fypp.in" EXTOUT "fypp"
	FLAGS "${FYPPFLAGS}"
	COMMENT "fyppifying global input file"
	FILES
	  fdict.fypp.in
	OUTPUT outFiles
	)

# Ensure it is a target (otherwise installation will not fyppify it)
add_custom_target(fdict.fypp ALL DEPENDS ${outFiles})


install(
  FILES
    ${outFiles}
	DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

# Finally define the fdict library
add_library(${PROJECT_NAME} ${fdict_sources})

# Attach the headers to the target
set_target_properties(${PROJECT_NAME}
  PROPERTIES
  POSITION_INDEPENDENT_CODE ON
  Fortran_MODULE_DIRECTORY ${LIB_MOD_DIR}
)

# Install all modules (omitting the directory)
install(DIRECTORY ${LIB_MOD_DIR}/ DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")

# Ensure that the target contains the headers/modules/include files
# and that it gets exported with the Targets output
target_include_directories(${PROJECT_NAME}
  INTERFACE
  $<BUILD_INTERFACE:${LIB_MOD_DIR}>
  $<INSTALL_INTERFACE:${CMAKE_INSTALL_INCLUDEDIR}>
)

# Install the library targets
# It also exports to the name ${PROJECT_NAME}-targets
# for subsequent usage in the installation of the library etc.
install(TARGETS ${PROJECT_NAME}
	EXPORT ${PROJECT_NAME}-targets
	RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
	ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
	LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
	PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
