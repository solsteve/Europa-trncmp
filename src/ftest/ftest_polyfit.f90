!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ P O L Y F I T                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  Europa is free software: you can redistribute it and/or modify it under the      **
!/ **  terms of the GNU General Public License as published by the Free Software        **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  Europa is distributed in the hope that it will be useful, but WITHOUT ANY        **
!/ **  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR    **
!/ **  A PARTICULAR PURPOSE. See the GNU General Public License for more details.       **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  Europa. If not, see <http://www.gnu.org/licenses/>.                              **
!/ **                                                                                   **
!/ =======================================================================================
module ftest_polyfit
  !/ -------------------------------------------------------------------------------------
  use polyfit_mod
  implicit none

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  !/ =====================================================================================
  subroutine TEST01
    !/ -----------------------------------------------------------------------------------
    implicit none

    real(dp), parameter :: P0(1) = [ 2.0d0 ]
    real(dp), parameter :: P1(2) = [ 2.0d0, -3.0d0 ]
    real(dp), parameter :: P2(3) = [ 2.0d0, -3.0d0, 5.0d0 ]
    real(dp), parameter :: P3(4) = [ 2.0d0, -3.0d0, 5.0d0, -7.0d0 ]
    real(dp), parameter :: P4(5) = [ 2.0d0, -3.0d0, 5.0d0, -7.0d0, 6.0d0 ]

    real(dp), parameter :: x0 = 1.0d0
    real(dp), parameter :: x1 = 11.0d0
    real(dp), parameter :: x2 = x1*x1
    real(dp), parameter :: x3 = x2*x1
    real(dp), parameter :: x4 = x2*x2

    real(dp) :: y0, y1, y2, y3, y4, t0, t1, t2, t3, t4

    y0 = x0*P0(1)
    y1 = x1*P1(1) + x0*P1(2)
    y2 = x2*P2(1) + x1*P2(2) + x0*P2(3)
    y3 = x3*P3(1) + x2*P3(2) + x1*P3(3) + x0*P3(4)
    y4 = x4*P4(1) + x3*P4(2) + x2*P4(3) + x1*P4(4) + x0*P4(5)

    t0 = poly1d( P0, X1 )
    t1 = poly1d( P1, X1 )
    t2 = poly1d( P2, X1 )
    t3 = poly1d( P3, X1 )
    t4 = poly1d( P4, X1 )

    print *, y0, t0
    print *, y1, t1
    print *, y2, t2
    print *, y3, t3
    print *, y4, t4

  end subroutine TEST01


end module ftest_polyfit


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_polyfit
  implicit none

  call TEST01

end program main

!/ =======================================================================================
!/ **                             F T E S T _ P O L Y F I T                             **
!/ =========================================================================== END FILE ==
