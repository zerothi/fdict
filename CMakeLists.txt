cmake_minimum_required(VERSION 3.17 FATAL_ERROR)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE "${CMAKE_CURRENT_SOURCE_DIR}/cmake/fdictDefaultFlags.cmake")

# Define fdict project and languages used
project(fdict
  LANGUAGES Fortran
  DESCRIPTION "Fortran dictionary for arbitrary data-types"
)

if (NOT DEFINED CMAKE_Fortran_MODULE_DIRECTORY)
	set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_BINARY_DIR}/fortran_modules)
endif ()

# Project installation follows GNU installation directory convention
include(GNUInstallDirs)


# Define project versions
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)

# Listify the string
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)
unset(VERSION_LIST)


# Define library specific content
set(PROJECT_AUTHOR       "Nick Papior")
set(PROJECT_DESCRIPTION  "Fortran dictionary for arbitrary data-types")
set(PROJECT_URL          "https://github.com/zerothi/fdict")
set(PROJECT_LICENSE      "Mozilla Public License, 2.0 (MPL-2.0)")
message(STATUS "
** PROJECT_NAME = ${PROJECT_NAME}
** Please report any build problems here: ${PROJECT_URL}
")


list(APPEND FYPPFLAGS
	"-DPROJECT_NAME=\\\"${PROJECT_NAME}\\\""
	"-DPROJECT_VERSION=\\\"${PROJECT_VERSION}\\\""
	"-DPROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}"
	"-DPROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}"
	"-DPROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}"
)

# Add project options
include(cmake/fdictOptions.cmake)
include(cmake/fdictFyppify.cmake)

# create library.
add_subdirectory(src)


# Now for installing stuff.

# Generate the version include file
# TODO: This file should not be necessary
#  This file is already parsed in the preprocessor
#  Instead move this information into fortran functions to extract the version
configure_file(fdict.inc.in fdict.inc @ONLY)
install(
  FILES
	  "${CMAKE_CURRENT_BINARY_DIR}/fdict.inc"
	DESTINATION "${CMAKE_INSTALL_INCLUDEDIR}"
)

# Globally define a place where we will install
# cmake configuration stuff, *Target.cmake *Version.cmake etc.
set(FDICT_INSTALL_CMAKECONFIG_DIR
  "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  CACHE STRING
  "Determine where the CMake configuration files will be installed"
)
mark_as_advanced(FDICT_INSTALL_CMAKECONFIG_DIR)


# Create pkg-config file
configure_file(fdict.pc.in fdict.pc @ONLY)
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
)

# Export CMake package file
include(CMakePackageConfigHelpers)

# Export a cmake package configure file
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake/Config.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
  INSTALL_DESTINATION "${FDICT_INSTALL_CMAKECONFIG_DIR}"
)

if(BUILD_SHARED_LIBS OR PROJECT_VERSION_MAJOR EQUAL 0)
  # Due to the uncertain ABI compatibility of Fortran shared libraries
  # limit compatibility for dynamic linking to same minor version.
  set(COMPATIBILITY SameMinorVersion)
else()
  # Require API compatibility via semantic versioning for static linking.
  set(COMPATIBILITY SameMajorVersion)
endif()

# Export a package version file
write_basic_package_version_file(
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  VERSION ${PROJECT_VERSION}
  COMPATIBILITY ${COMPATIBILITY}
)

# Install cmake configuration files
install(FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}Config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}ConfigVersion.cmake"
  DESTINATION "${FDICT_INSTALL_CMAKECONFIG_DIR}"
)


# Install project
install(EXPORT ${PROJECT_NAME}-targets
  FILE ${PROJECT_NAME}Targets.cmake
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${FDICT_INSTALL_CMAKECONFIG_DIR}"
)

# Export a cmake configuration for the targets if used as a sub-project
export(EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  FILE "${PROJECT_NAME}Targets.cmake"
)
