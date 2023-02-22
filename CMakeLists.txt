cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

# Define fdict project and languages used
project(fdict
  LANGUAGES Fortran
  DESCRIPTION "Fortran dictionary for arbitrary data-types"
  )


# Read version from file
file(STRINGS "${PROJECT_SOURCE_DIR}/VERSION" PROJECT_VERSION)
# Listify the string
string(REPLACE "." ";" VERSION_LIST ${PROJECT_VERSION})
list(GET VERSION_LIST 0 PROJECT_VERSION_MAJOR)
list(GET VERSION_LIST 1 PROJECT_VERSION_MINOR)
list(GET VERSION_LIST 2 PROJECT_VERSION_PATCH)
unset(VERSION_LIST)


set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/modules)

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


# --- compiler feature checks
include(CheckFortranSourceCompiles)
include(CheckFortranSourceRuns)


if(NOT DEFINED CMAKE_MAXIMUM_RANK)
  set(CMAKE_MAXIMUM_RANK 3 CACHE STRING "Maximum array rank for generated procedures")
endif()

# Define the allocatable array sizes
set(F_ARRAY ":")
foreach(IR RANGE 2 ${CMAKE_MAXIMUM_RANK})
  list(APPEND F_ARRAY ":")
endforeach()
string(REPLACE ";" "," F_ARRAY "${F_ARRAY}")

# Once we have the sizes of the settings defined, we will add a check that the compiler will actually handle this.
check_fortran_source_compiles("real, allocatable :: array(${F_ARRAY}); end" fmaximum_rank SRC_EXT f90)
unset(F_ARRAY)
check_fortran_source_runs("use, intrinsic :: iso_fortran_env, only : real128; real(real128) :: x; x = x+1; end" f03real128)

foreach(V IN ITEMS fmaximum_rank f03real128)
  if(NOT ${${V}})
    message(FATAL_ERROR "Could not succesfully test compiler capability: ${V}")
  endif()
endforeach()



# We are done setting up things for CMAKE
# Now we can do project based compilation and installation

# Define the settings.bash target
add_custom_command(
  OUTPUT settings.bash
  COMMAND VPATH=${CMAKE_SOURCE_DIR} ${CMAKE_CURRENT_SOURCE_DIR}/setup.sh -A ${CMAKE_MAXIMUM_RANK}
  VERBATIM
  COMMENT "Creating settings.bash"
  )

install(
  FILES settings.bash
  DESTINATION "${CMAKE_INSTALL_BINDIR}"
  )


# Ensure module files are installed
#set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_INSTALL_INCDIR})


# Add sub-directory
add_subdirectory(src)


#[==[
# Installation of the fdict library
install(
  TARGETS ${PROJECT_NAME}
  EXPORT "${PROJECT_NAME}-targets"
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
install(EXPORT ${PROJECT_NAME}-targets
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
]==]
