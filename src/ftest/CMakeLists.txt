#/ ====================================================================== BEGIN FILE =====
#/ **                             FTest - C M A K E L I S T                             **
#/ =======================================================================================
#/ **                                                                                   **
#/ **  Copyright  (c) 2018, Stephen W. Soliday                                           **
#/ **                      stephen.soliday@trncmp.org                                   **
#/ **                      http://research.trncmp.org                                   **
#/ **                                                                                   **
#/ **  -------------------------------------------------------------------------------  **
#/ **                                                                                   **
#/ **  This program is free software: you can redistribute it and/or modify it under    **
#/ **  the terms of the GNU General Public License as published by the Free Software    **
#/ **  Foundation, either version 3 of the License, or  (at your option)                 **
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

project ( ftest Fortran )

file ( GLOB fsrc "ftest_*.f90" )

foreach ( ft ${fsrc} )
  get_filename_component ( _exec ${ft} NAME_WE )
  
  set ( CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/temp )

  add_executable ( ${_exec} ${ft} )
  
target_include_directories( ${_exec} PRIVATE ${HDF5_INCLUDE_DIRS})

  target_link_libraries ( ${_exec} europa )
  target_link_libraries ( ${_exec} ${LAPACK_LIBRARIES} )
  target_link_libraries ( ${_exec} ${BLAS_LIBRARIES} )
  target_link_libraries ( ${_exec} ${HDF5_Fortran_LIBRARIES} )
  
  install (TARGETS ${_exec}
    RUNTIME DESTINATION bin
    LIBRARY DESTINATION lib
    ARCHIVE DESTINATION lib)
  
endforeach ( )

#/ =======================================================================================
#/ **                             FTest - C M A K E L I S T                             **
#/ =========================================================================== END FILE ==
