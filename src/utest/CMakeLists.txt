#/ ====================================================================== BEGIN FILE =====
#/ **                           Unit Test - C M A K E L I S T                           **
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

project ( tests Fortran )

add_subdirectory ( arrays )
add_subdirectory ( container )
add_subdirectory ( mathaux )
add_subdirectory ( strings )
add_subdirectory ( gis )
add_subdirectory ( stats )
add_subdirectory ( linalg )

set ( CMAKE_Fortran_MODULE_DIRECTORY "" )

#/ =======================================================================================
#/ **                           Unit Test - C M A K E L I S T                           **
#/ =========================================================================== end FILE ==
