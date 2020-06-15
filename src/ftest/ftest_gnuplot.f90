!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ G N U P L O T                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module ftest_gnuplot
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-JUN-01
  !! license: GPL
  !!
  !! ## Test GNUPlot file generation
  !
  !/ -------------------------------------------------------------------------------------
  use statistics_mod
  use dice_mod





  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine TEST01
    !/ -----------------------------------------------------------------------------------
    use file_tools, only : WriteUnit
    implicit none

  character(*), parameter :: f_dat = 'test.dat'
  character(*), parameter :: f_cfg = 'test.cfg'

  integer,  parameter :: NUM_SAMP = 1000
  real(dp), parameter :: Y_NOISE  = D_TWO

  real(dp), parameter :: START_X  = D_TWO
  real(dp), parameter :: FINIS_X  = 2.0d1
  real(dp), parameter :: DELTA    = (FINIS_X - START_X) / real( NUM_SAMP - 1, dp )

  real(dp), parameter :: TEST_M = 2.3d0
  real(dp), parameter :: TEST_B = 1.1d0
  
  real(dp), allocatable, dimension(:) :: x, y
  !/ -------------------------------------------------------------------------------------
  real(dp)   :: tx, ty, m, b
  integer    :: i, fh
  type(Dice) :: dd
  !/ -------------------------------------------------------------------------------------

  allocate( x(NUM_SAMP) )
  allocate( y(NUM_SAMP) )
  
  call dd%seed_set
  
  tx = START_X
  do i=1,NUM_SAMP
     ty = ( TEST_M * tx ) + TEST_B + ( Y_NOISE * dd%normal() )
     x(i) = tx
     y(i) = ty
     tx = tx + DELTA
  end do

  !/ -------------------------------------------------------------------------------------

  call LeastSquares( m, b, x, y )

  fh = WriteUnit( FILE=f_dat )

  do i=1,NUM_SAMP
     ty = ( m * x(i) ) + b
     write( fh, 100 ) x(i), y(i), ty
  end do

  close( fh )

  !/ -------------------------------------------------------------------------------------

  deallocate( x )
  deallocate( y )
  
100 format( G0,1X,G0,1X,G0 )

end subroutine TEST01
  
end module ftest_gnuplot




!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
use ftest_gnuplot

call TEST01

end program main


!/ =======================================================================================
!/ **                             F T E S T _ G N U P L O T                             **
!/ =========================================================================== END FILE ==
