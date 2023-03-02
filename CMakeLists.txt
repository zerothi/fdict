cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

# Define fdict project and languages used
project(fdict
  LANGUAGES Fortran
  DESCRIPTION "Fortran dictionary for arbitrary data-types"
)

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
message("** PROJECT_NAME = ${PROJECT_NAME}")
message("** Please report any build problems here: ${PROJECT_URL}")


list(APPEND FYPPFLAGS
	"-DPROJECT_NAME=\\\"${PROJECT_NAME}\\\""
	"-DPROJECT_VERSION=\\\"${PROJECT_VERSION}\\\""
	"-DPROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}"
	"-DPROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}"
	"-DPROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}"
)

# Add project options
include(${CMAKE_CURRENT_SOURCE_DIR}/config/ProjectOptions.cmake)


# Add our custom definitions
add_subdirectory(config)
add_subdirectory(src)


# Install project
install(EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)

# Export a cmake configuration for the targets
export(EXPORT ${PROJECT_NAME}-targets
  NAMESPACE ${PROJECT_NAME}::
  FILE "${CMAKE_CURRENT_BINARY_DIR}/cmake/${PROJECT_NAME}-targets.cmake"
)
