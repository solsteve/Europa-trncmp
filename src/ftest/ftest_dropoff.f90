!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ D R O P O F F                             **
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
module ftest_dropoff
  !/ -------------------------------------------------------------------------------------
  use dropoff_mod
  use psgraph_mod
  implicit none


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine graph( pd, Vo, Vf, n, dt )
    !/ -------------------------------------------------------------------------------------
    type(PSDraw), intent(inout) :: pd
    real(dp),     intent(in)    :: Vo  !! starting value.
    real(dp),     intent(in)    :: Vf  !! final value.
    integer,      intent(in)    :: n   !! number of samples.
    character(1), intent(in)    :: dt  !! drop off type (default linear).
    !/ -------------------------------------------------------------------------------------
    type(DropOff) :: drop
    integer       :: i
    real(dp)      :: last_x, last_y, x, y
    !/ -------------------------------------------------------------------------------------

    call drop%build( Vo, Vf, n, dt )

    last_x = D_ONE
    last_y = drop%next()

    do i=2,n
       x = last_x + D_ONE
       y = drop%next()
       call pd%drawLine( last_x, last_y, x, y )
       last_x = x
       last_y = y
    end do
  end subroutine graph


end module ftest_dropoff

!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_dropoff
  implicit none

  real(dp), parameter :: first = 100.0d0
  real(dp), parameter :: last  =  10.0d0
  integer,  parameter :: count = 100

  type(PSGraph), pointer :: ps
  type(PSDraw),  pointer :: pd

  pd => PSDraw( 10d0, 7.5d0, D_ZERO, D_ZERO, real(count,dp), max( first, last ) )

  ps => PSGraph(1)

  call pd%drawBorder

  call pd%setRGB( color_red )
  call graph( pd, first, last, count, 'Linear' )

  call pd%setRGB( color_green )
  call graph( pd, first, last, count, 'Exp' )

  call pd%setRGB( color_blue )
  call graph( pd, first, last, count, 'Gaussian' )

  call ps%add( pd, 1, 0.5d0, 0.5d0 )

  call ps%pswrite( 'dropoff.ps' )

end program main

!/ =======================================================================================
!/ **                             F T E S T _ D R O P O F F                             **
!/ =========================================================================== END FILE ==
