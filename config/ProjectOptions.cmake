# Here we define all options that are related to fdict

# Ensure we have the program fypp installed
find_program(FYPP fypp)
if(NOT FYPP)
  message(FATAL_ERROR "Could not find executable fypp -- it is required for the pre-processing step")
endif()

# We need to define certain variables before the initial configuration of this project
if(NOT DEFINED CMAKE_MAXIMUM_RANK)
  set(CMAKE_MAXIMUM_RANK 5 CACHE STRING "Maximum array rank for generated procedures")
endif()

# Whether we should use the iso_fortran_env for data-types
message(CHECK_START "Requested use of intrinsic fortran module (iso_fortran_env) for data-types")
if(DEFINED "WITH_ISO_ENV}")
   message(CHECK_PASS "used")
   list(APPEND FYPPFLAGS "-DWITH_ISO_ENV=$<BOOL:${WITH_ISO_ENV}>")
else()
   message(CHECK_FAIL "not used")
endif()


# Parse data-type options
message(CHECK_START "Checking for data-type interfaces")
list(APPEND CMAKE_MESSAGE_INDENT "  ")
foreach(var INT8 INT16 REAL80 REAL128 LOG8 LOG16 LOG64 ISO_C)
  message(CHECK_START "data-type ${var}")
  if(DEFINED "WITH_${var}")
     message(CHECK_PASS "added")
     list(APPEND FYPPFLAGS "-DWITH_${var}=$<BOOL:${WITH_${var}}>")
  else()
     message(CHECK_FAIL "not added")
  endif()
endforeach()
list(POP_BACK CMAKE_MESSAGE_INDENT)
message(CHECK_PASS "done")

# Global maxrank
message(CHECK_START "Checking for data-type rank size interfaces")
list(APPEND CMAKE_MESSAGE_INDENT "  ")
message(CHECK_START "default rank size")
if(DEFINED "MAXRANK")
  message(CHECK_PASS "found size = ${MAXRANK}")
  list(APPEND FYPPFLAGS "-DMAXRANK=${MAXRANK}")
else()
  set(MAXRANK 5)
  message(CHECK_FAIL "using default size = ${MAXRANK}")
endif()

# Parse rank sizes
foreach(var INT REAL CMPLX LOG ISO_C)
  message(CHECK_START "rank size of ${var}")
  if(DEFINED "MAXRANK_${var}")
     message(CHECK_PASS "found size = ${MAXRANK_${var}}")
     list(APPEND FYPPFLAGS "-DMAXRANK_${var}=${MAXRANK_${var}}")
  else()
     message(CHECK_FAIL "using default size = ${MAXRANK}")
  endif()
endforeach()
list(POP_BACK CMAKE_MESSAGE_INDENT)
message(CHECK_PASS "done")
