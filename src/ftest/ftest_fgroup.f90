!/ ====================================================================== BEGIN FILE =====
!/ **                              F T E S T _ F G R O U P                              **
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
!
!> @brief   Test Fuzzy Group.
!!
!! @details Provides a test harness for Fuzzy Group.
!! 
!! @author  Stephen W. Soliday
!! @date    2020-01-09
!
!/ =======================================================================================
program ftest_fgroup
  !/ -------------------------------------------------------------------------------------
  use psgraph_mod
  use fuzzy_group_mod
  implicit none
  !/ -------------------------------------------------------------------------------------
  class(PSGraph), pointer :: ps
  class(PSDraw),  pointer :: pd
  integer,  parameter :: np(4)  = [ 3, 5, 7, 9 ]
  real(dp), parameter :: mnc(4) = [ -1.0d0, -1.0d0, -1.0d0, -1.0d0 ]
  real(dp), parameter :: mxc(4) = [  1.0d0,  1.0d0,  1.0d0,  1.0d0 ]

  real(sp), parameter :: red(4)   = [ 1.0, 0.0, 0.0, 0.0]
  real(sp), parameter :: green(4) = [ 0.0, 1.0, 0.0, 0.0]
  real(sp), parameter :: blue(4)  = [ 0.0, 0.0, 1.0, 0.0]

  type(FuzzyGroup)      :: FG
  real(dp), allocatable :: mu(:), xIn(:), xOut(:)
  integer  :: m, n, i, j, ns
  real(dp) :: min_value, max_value, delta
  real(dp), allocatable :: x(:,:), y(:,:)
  !/ -------------------------------------------------------------------------------------

  call FG%init( np, mnc, mxc )

  print *, 'In   ', FG%nIn()
  print *, 'Out  ', FG%nOut()
  print *, 'Size ', FG%size()

  m = FG%nIn()
  n = FG%size()

  allocate( xIn(m) )
  allocate( xOut(m) )
  allocate( mu(n) )

  min_value = min( minval(mnc), minval(mxc) )
  max_value = max( maxval(mnc), maxval(mxc) )

  pd => PSDraw( 7.0d0, 7.0d0, min_value, min_value, max_value, max_value )
  ps => PSGraph( 1 )
  call ps%add( pd, 1, 2.0d0, 0.75d0 )

  call pd%drawBorder

  ns = 1000
  delta = (max_value - min_value) / real(ns, dp)

  allocate( x(ns,m) )
  allocate( y(ns,m) )

  do i=1,ns
     do j=1,m
        xIn(j) = min_value
        x(i,j) = min_value
     end do
     call FG%fuzzify( mu, xIn )
     call FG%defuzzify( xOut, mu )
     do j=1,m
        y(i,j) = xOut(j)
     end do
     min_value = min_value + delta
  end do

  do j=1,m
     call pd%setRGB( red(j), green(j), blue(j) )
     do i=2,ns
        call pd%drawLine( x(i-1,j), y(i-1,j), x(i,j), y(i,j) )
     end do
  end do

  call ps%pswrite( 'fgroup.ps' )

  deallocate( x )
  deallocate( y )
  deallocate( xIn )
  deallocate( xOut )
  deallocate( mu )

end program ftest_fgroup

!/ =======================================================================================
!/ **                              F T E S T _ F G R O U P                              **
!/ =========================================================================== END FILE ==
