# Make CMake run the scripts for creating the input files
# Define this directory's module location
set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/modules/)

# Define the files that needs to be fyppified
set(fyppFiles
	fdict_types.fypp
	variable.fypp
	dictionary.fypp
)


# Do the preprocessing
fyppify(
	EXTIN "fypp" EXTOUT "f90"
	FLAGS ${FYPPFLAGS}
	COMMENT "fyppifying source codes"
	FILES ${fyppFiles}
	OUTPUT outFiles
)

set(SRC
	${outFiles}
)

# Also fyppify the global inclusion file
fyppify(
	EXTIN "fypp.in" EXTOUT "fypp"
	FLAGS "${FYPPFLAGS}"
	COMMENT "fyppifying global input file"
	FILES fdict.fypp.in
	OUTPUT outFiles
	)

# Ensure it is a target
add_custom_target(fdict.fypp ALL DEPENDS ${outFiles})

# Generate the include file for discovering versions
configure_file(
	"${CMAKE_CURRENT_SOURCE_DIR}/fdict.inc.in"
	"${CMAKE_CURRENT_BINARY_DIR}/fdict.inc"
	@ONLY
)

# Define all
set(INTERFACES
	${outFiles}
	${CMAKE_CURRENT_BINARY_DIR}/fdict.inc
)

install(FILES ${INTERFACES}
	DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
)

# Finally define the fdict library
add_library(${PROJECT_NAME} ${SRC})

# Attach the headers to the target
set_target_properties(${PROJECT_NAME}
  PROPERTIES
  POSITION_INDEPENDENT_CODE ON
)

# Install all modules
install(DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}")

# Install the library targets
install(TARGETS ${PROJECT_NAME}
	EXPORT "${PROJECT_NAME}-targets"
	RUNTIME DESTINATION "${CMAKE_INSTALL_BINDIR}"
	ARCHIVE DESTINATION "${CMAKE_INSTALL_LIBDIR}"
	LIBRARY DESTINATION "${CMAKE_INSTALL_LIBDIR}"
	PUBLIC_HEADER DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
	INCLUDES DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)
