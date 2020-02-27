!/ ====================================================================== BEGIN FILE =====
!/ **                                F T E S T _ T I M E                                **
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
module ftest_time_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-12-02
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use time_tools_mod
  implicit none


  interface dv_jday_f77
     SUBROUTINE DV_JDay ( Year,Mon,Day,Hr,minute, Sec, JD, JDFrac )
       use trncmp_env
       integer,  intent(in)  :: Year
       integer,  intent(in)  :: Mon
       integer,  intent(in)  :: Day
       integer,  intent(in)  :: Hr
       integer,  intent(in)  :: minute
       real(dp), intent(in)  :: Sec
       real(dp), intent(out) :: JD
       real(dp), intent(out) :: JDFrac
     end SUBROUTINE DV_JDay
  end interface dv_jday_f77

  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  subroutine test01
    implicit none
    real(dp) :: jd, jdf

    call dv_jday_f77( 2000, 6, 18, 6, 0, 0.0d0, JD, JDF )

    print *, JD, JDF 

  end subroutine test01

end module ftest_time_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_time_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test01



end program main

!/ =======================================================================================
!/ **                                F T E S T _ T I M E                                **
!/ =========================================================================== END FILE ==
