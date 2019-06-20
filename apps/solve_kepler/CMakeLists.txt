#/ ====================================================================== BEGIN FILE =====
#/ **                      Tools Subdirectory - C M A K E L I S T                       **
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
#/ =======================================================================================

project ( solve_kepler Fortran )

file ( GLOB ${PROJECT_NAME}_sources "*.f90" )

message ( STATUS "Sources for Project ${PROJECT_NAME}: ${${PROJECT_NAME}_sources}" )

set ( CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/temp )
add_executable ( ${PROJECT_NAME} ${${PROJECT_NAME}_sources} )
set ( SDSPEC "-std=f2008ts -ffree-form" )

target_link_libraries ( ${PROJECT_NAME} europa )
target_link_libraries ( ${PROJECT_NAME} ${LAPACK_LIBRARIES} )
target_link_libraries ( ${PROJECT_NAME} ${BLAS_LIBRARIES} )

install ( TARGETS ${PROJECT_NAME}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib )

#/ =======================================================================================
#/ **                      Tools Subdirectory - C M A K E L I S T                       **
#/ =========================================================================== end FILE ==
