#/ ====================================================================== BEGIN FILE =====
#/ **                            Library - C M A K E L I S T                            **
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

project ( europa Fortran )

file ( GLOB lib_src "*.f90" )
file ( GLOB la_src "linalg/*.f90" )

set ( ${PROJECT_NAME}_sources ${lib_src} ${la_src} )

message ( STATUS "Sources for project ${PROJECT_NAME}: ${${PROJECT_NAME}_sources}" )

add_library ( ${PROJECT_NAME} STATIC ${${PROJECT_NAME}_sources} )

install ( TARGETS ${PROJECT_NAME}
  RUNTIME DESTINATION bin
  LIBRARY DESTINATION lib
  ARCHIVE DESTINATION lib )

#/ =======================================================================================
#/ **                            Library - C M A K E L I S T                            **
#/ =========================================================================== end FILE ==
