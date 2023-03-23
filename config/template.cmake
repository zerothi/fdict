@PACKAGE_INIT@

function(set_defined name value)
  string(STRIP "${value}" stripvalue)
  if(NOT "${stripvalue}" STREQUAL "")
    set(${name} "${stripvalue}")
  endif()
endmacro()
string(TOUPPER PROJECT_NAME_UPPER "@PROJECT_NAME@")

# define the fyppflags used to compile
set_defined(${PROJECT_NAME_UPPER}_FYPPFLAGS "@FYPPFLAGS@")

# define the different data-types used
set_defined(${PROJECT_NAME_UPPER}_WITH_INT8 "@WITH_INT8@")
set_defined(${PROJECT_NAME_UPPER}_WITH_INT16 "@WITH_INT16@")
set_defined(${PROJECT_NAME_UPPER}_WITH_REAL80 "@WITH_REAL80@")
set_defined(${PROJECT_NAME_UPPER}_WITH_REAL128 "@WITH_REAL128@")
set_defined(${PROJECT_NAME_UPPER}_WITH_LOG8 "@WITH_LOG16@")
set_defined(${PROJECT_NAME_UPPER}_WITH_LOG64 "@WITH_LOG64@")
set_defined(${PROJECT_NAME_UPPER}_WITH_ISO_C "@WITH_ISO_C@")

# Set the ranks as defined by the input
set_defined(${PROJECT_NAME_UPPER}_MAXRANK "@MAXRANK@")
set_defined(${PROJECT_NAME_UPPER}_MAXRANK_INT "@MAXRANK_INT@")
set_defined(${PROJECT_NAME_UPPER}_MAXRANK_REAL "@MAXRANK_REAL@")
set_defined(${PROJECT_NAME_UPPER}_MAXRANK_CMPLX "@MAXRANK_CMPLX@")
set_defined(${PROJECT_NAME_UPPER}_MAXRANK_LOG "@MAXRANK_LOG@")
set_defined(${PROJECT_NAME_UPPER}_MAXRANK_ISO_C "@MAXRANK_ISO_C@")

if(NOT TARGET "@PROJECT_NAME@::@PROJECT_NAME@")
  include("${CMAKE_CURRENT_LIST_DIR}/@PROJECT_NAME@Targets.cmake")
endif()
