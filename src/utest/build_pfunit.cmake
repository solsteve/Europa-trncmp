#/ ====================================================================== BEGIN FILE =====
#/ **                              B U I L D _ P F U N I T                              **
#/ =======================================================================================
#/ **                                                                                   **
#/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
#/ **                      stephen.soliday@trncmp.org                                   **
#/ **                      http://research.trncmp.org                                   **
#/ **                                                                                   **
#/ **  -------------------------------------------------------------------------------  **
#/ **                                                                                   **
#/ **  This program is free software: you can redistribute it and/or modify it under    **
#/ **  the terms of the GNU General Public License as published by the Free Software    **
#/ **  Foundation, either version 3 of the License, or (at your option)                 **
#/ **  any later version.                                                               **
#/ **                                                                                   **
#/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
#/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
#/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
#/ **                                                                                   **
#/ **  You should have received a copy of the GNU General Public License along with     **
#/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
#/ **                                                                                   **
#/ ----- Modification History ------------------------------------------------------------
#/
#/ @brief   Build pFUnit test.
#/
#/ @details Provides a common common file included in subdirectories.
#/
#/ @author  Stephen W. Soliday
#/ @date    2018-04-21
#/
#/ ---------------------------------------------------------------------------------------
#/
#/          Each subdirectory must contain a CMakeLists.txt file with:
#/
#/            project(math Fortran)
#/            include( ../build_pfunit.cmake )
#/
#/     where 'math' is the name of the project in the subdirectory
#/
#/ =======================================================================================

file(GLOB pf_files "*.pf")

set(testSuites ${PROJECT_BINARY_DIR}/include/testSuites.inc)

file(WRITE ${testSuites} "")

set(_test_sources)
foreach(pf ${pf_files})
  get_filename_component(_test ${pf} NAME_WE)
  message(STATUS "Adding test ${_test}")
  set(test_dependency ${PROJECT_SOURCE_DIR}/${_test}.pf)
  add_custom_command(
    OUTPUT ${PROJECT_BINARY_DIR}/include/${_test}.F90
    COMMAND python ${PFUNIT_DIR}/bin/pFUnitParser.py ${PROJECT_SOURCE_DIR}/${_test}.pf ${PROJECT_BINARY_DIR}/include/${_test}.F90
    DEPENDS ${test_dependency}
    )
  set(_test_sources ${_test_sources} ${PROJECT_BINARY_DIR}/include/${_test}.F90)
  file(APPEND ${testSuites} "ADD_TEST_SUITE(${_test}_suite)\n")
endforeach()

set_source_files_properties(${PFUNIT_DIR}/include/driver.F90 PROPERTIES GENERATED 1)

#string(REGEX REPLACE " -Wall" "" CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG}")

include_directories(
  ${PROJECT_SOURCE_DIR}
  ${PROJECT_BINARY_DIR}/include
  ${PFUNIT_DIR}/include
  ${PFUNIT_DIR}/mod
  )

set(CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/temp)

add_executable(
  ${PROJECT_NAME}
  ${PFUNIT_DIR}/include/driver.F90
  ${_test_sources}
  )

target_link_libraries(
  ${PROJECT_NAME}
  ${PFUNIT_DIR}/lib/libpfunit.a
  europa
  ${LAPACK_LIBRARIES}
  ${BLAS_LIBRARIES}
  )

install(TARGETS ${PROJECT_NAME}
  RUNTIME DESTINATION test
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib)


#/ =======================================================================================
#/ **                                  M A K E F I L E                                  **
#/ =========================================================================== end FILE ==
