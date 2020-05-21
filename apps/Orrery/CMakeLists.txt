#/ ====================================================================== BEGIN FILE =====
#/ **                      Orrery Subdirectory - C M A K E L I S T                      **
#/ =======================================================================================
#/ **                                                                                   **
#/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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

project ( Orrery Fortran )

file ( GLOB progs "*.f90" )

foreach ( ft ${progs} )
  get_filename_component ( _exec ${ft} NAME_WE )
  
  set ( CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/temp )

  add_executable ( ${_exec} ${ft} )
  
  target_link_libraries ( ${_exec} europa )
  
  install (TARGETS ${_exec}
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)
  
endforeach ( )

#/ =======================================================================================
#/ **                      Orrery Subdirectory - C M A K E L I S T                      **
#/ =========================================================================== end FILE ==
