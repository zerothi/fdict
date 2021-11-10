cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

# Define fdict project and version and languages used
project(fdict
  VERSION 0.7.0
  LANGUAGES Fortran
  DESCRIPTION "Fortran dictionary for arbitrary data-types"
  )

# Define library specific content
set(FDICT_AUTHOR       "Nick Papior")
set(FDICT_DESCRIPTION  "Fortran dictionary for arbitrary data-types")
set(FDICT_URL          "https://github.com/zerothi/fdict")
set(FDICT_LICENSE      "Mozilla Public License, 2.0 (MPL-2.0)")
set(FDICT_MAJOR_VERSION ${PROJECT_VERSION_MAJOR})
set(FDICT_MINOR_VERSION ${PROJECT_VERSION_MINOR})
set(FDICT_MICRO_VERSION ${PROJECT_VERSION_PATCH})


# Project installation follows GNU installation directory convention
include(GNUInstallDirs)

# Add sub-directory
add_subdirectory(src)

# Define the settings.bash target
add_custom_command(
  OUTPUT settings.bash
  COMMAND VPATH=${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/setup.sh --default
  VERBATIM
  COMMENT Creating settings.bash
  )


# Installation of the fdict library
install(
  TARGETS fdict
  EXPORT "${PROJECT_NAME}Targets"
  RUNTIME DESTINATION ${CMAKE_INSTALL_BINDIR}
  PUBLIC_HEADER DESTINATION ${CMAKE_INSTALL_INCDIR}
  ARCHIVE DESTINATION ${CMAKE_INSTALL_LIBDIR}
  LIBRARY DESTINATION ${CMAKE_INSTALL_LIBDIR}
  )

# Retrieve the header files I need to install
get_target_property(fdict_headers fdict PUBLIC_HEADERS)
install(
  FILES ${fdict_headers}
  DESTINATION ${CMAKE_INSTALL_INCLUDEDIR}
  )

#install(
#  FILES settings.bash
#  DESTINATION ${CMAKE_INSTALL_BINDIR}
#  )

