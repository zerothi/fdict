# Figure out if we have quadruple precision

# Check for available features
# Note: users can overwrite the automatic check by setting the value at configure time
include(CheckFortranSourceRuns)

if (NOT DEFINED WITH_QP)
  check_fortran_source_runs(
    "if (selected_real_kind(33) == -1) stop 1; end"
    WITH_QP
  )
  set(WITH_QP ${WITH_QP} PARENT_SCOPE)
endif()




# Define a global variable that defines the directory of cmake configuration
# files.
set(
  CMAKE_INSTALL_CMAKECONFIGDIR
  "${CMAKE_INSTALL_LIBDIR}/cmake/${PROJECT_NAME}"
  PARENT_SCOPE
  )


# Export a pkg-config file
configure_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/template.pc"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  @ONLY
)
# Install pkg-config files
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}.pc"
  DESTINATION "${CMAKE_INSTALL_LIBDIR}/pkgconfig"
)

# Export CMake package file
include(CMakePackageConfigHelpers)

# Export a cmake package configure file
configure_package_config_file(
  "${CMAKE_CURRENT_SOURCE_DIR}/template.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  INSTALL_DESTINATION "${CMAKE_INSTALL_CMAKECONFIGDIR}"
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
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  VERSION "${PROJECT_VERSION}"
  COMPATIBILITY ${COMPATIBILITY}
)

# Install cmake configuration files
install(
  FILES
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config.cmake"
  "${CMAKE_CURRENT_BINARY_DIR}/${PROJECT_NAME}-config-version.cmake"
  DESTINATION "${CMAKE_INSTALL_CMAKECONFIGDIR}"
)
