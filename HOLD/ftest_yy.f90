!/ ====================================================================== BEGIN FILE =====
!/ **                                  F T E S T _ Y Y                                  **
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
module ftest_yy
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-mm-dd
  !! license: GPL
  !!
  !!##Test of .
  !!
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use deque_integer_class
  implicit none


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine test01
    !/ -----------------------------------------------------------------------------------
    type(DequeInteger) :: Q

    write(*,*) Q%size()

    if ( Q%empty() ) then
       write(*,*) 'Empty'
    else
        write(*,*) 'Not Empty'
     end if
    
  end subroutine test01


end module ftest_yy


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_yy
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test01

end program main

!/ =======================================================================================
!/ **                                  F T E S T _ Y Y                                  **
!/ =========================================================================== END FILE ==
