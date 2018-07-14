!/ ====================================================================== BEGIN FILE =====
!/ **                                F T E S T _ H D F 5                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  This program is free software: you can redistribute it and/or modify it under    **
!/ **  the terms of the GNU General Public License as published by the Free Software    **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
!/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
!/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
!/ **                                                                                   **
!/ =======================================================================================
module ftest_hdf5
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-07-01
  !! license: GPL
  !!
  !!##Test of .
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use hdf5
  implicit none
  private


  public :: h5test01

  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine h5test01
    !/ -----------------------------------------------------------------------------------
    use hdf5
    implicit none

    CHARACTER(LEN=8), PARAMETER :: filename = "file.h5" ! File name
    INTEGER(HID_T)              :: file_id
    INTEGER                     :: error
    
    CALL h5open_f(error)
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL h5fclose_f(file_id, error)
    CALL h5close_f(error)


  end subroutine h5test01




  

end module ftest_hdf5




!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_hdf5
  implicit none


  call h5test01

    
end program main

!/ =======================================================================================
!/ **                                F T E S T _ H D F 5                                **
!/ =========================================================================== END FILE ==
