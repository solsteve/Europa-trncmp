!/ ====================================================================== BEGIN FILE =====
!/ **                                 F T E S T _ F F T                                 **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module ftest_fft_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-17
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use fft_mod

  


  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  subroutine test01
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(kind=dp), dimension(8) :: data = (/1.0, 1.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0/)
    integer :: i

    call fft(data)

    do i=1,8
       write(*,'("(", F20.15, ",", F20.15, "i )")') data(i)
    end do

  end subroutine test01


end module ftest_fft_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_fft_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test01



end program main

!/ =======================================================================================
!/ **                                 F T E S T _ F F T                                 **
!/ =========================================================================== END FILE ==
