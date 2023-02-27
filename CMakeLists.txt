cmake_minimum_required(VERSION 3.13 FATAL_ERROR)

# Include overwrites before setting up the project
set(CMAKE_USER_MAKE_RULES_OVERRIDE ${CMAKE_CURRENT_SOURCE_DIR}/config/DefaultFlags.cmake)

# Define fdict project and languages used
project(fdict
  LANGUAGES Fortran
  DESCRIPTION "Fortran dictionary for arbitrary data-types"
  )


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


# Define the module temporary location
#set(CMAKE_Fortran_MODULE_DIRECTORY ${CMAKE_CURRENT_SOURCE_DIR}/modules/)

list(APPEND FYPPFLAGS
	"-DPROJECT_NAME=\\\"${PROJECT_NAME}\\\""
	"-DPROJECT_VERSION=\\\"${PROJECT_VERSION}\\\""
	"-DPROJECT_VERSION_MAJOR=${PROJECT_VERSION_MAJOR}"
	"-DPROJECT_VERSION_MINOR=${PROJECT_VERSION_MINOR}"
	"-DPROJECT_VERSION_PATCH=${PROJECT_VERSION_PATCH}"
	)


# Add our custom definitions
add_subdirectory(config)

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


add_subdirectory(src)


install(EXPORT ${PROJECT_NAME}-targets
        NAMESPACE ${PROJECT_NAME}::
        DESTINATION "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
)
