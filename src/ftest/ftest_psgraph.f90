!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ P S G R A P H                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2012,2015,2019, Stephen W. Soliday                                 **
!/ **                                stephen.soliday@trncmp.org                         **
!/ **                                http://research.trncmp.org                         **
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
!> @brief   Test PSGraph.
!!
!! @details Provides a test harness for PSGraph.
!! 
!! @author  Stephen W. Soliday
!! @date    2019-12-23
!
!/ =======================================================================================
program ftest_psgraph
  !/ -------------------------------------------------------------------------------------
  use psgraph_mod
  implicit none

  real(dp), parameter :: px1(7) = [ 0.8_dp, 0.7_dp, 1.4_dp, 1.5_dp, 3.2_dp, 1.7_dp, 2.0_dp ]
  real(dp), parameter :: py1(7) = [ 1.6_dp, 2.6_dp, 3.5_dp, 4.2_dp, 3.5_dp, 2.5_dp, 0.8_dp ]
  real(dp), parameter :: px2(6) = [ 1.2_dp, 1.5_dp, 2.2_dp, 2.8_dp, 3.3_dp, 2.3_dp ]
  real(dp), parameter :: py2(6) = [ 5.0_dp, 6.2_dp, 5.2_dp, 5.8_dp, 5.3_dp, 4.4_dp ]

  integer :: pn1, pn2

  class(PSDraw), pointer :: pd11
  class(PSDraw), pointer :: pd12
  class(PSDraw), pointer :: pd13
  class(PSDraw), pointer :: pd14

  class(PSDraw), pointer :: pd21
  class(PSDraw), pointer :: pd22
  class(PSDraw), pointer :: pd23

  class(PSGraph), pointer :: ps

  !/ -------------------------------------------------------------------------------------

  pn1 = size( px1 )
  pn2 = size( px2 )

  !/ -------------------------------------------------------------------------------------

  pd11 => PSDraw( 8.5d0, 3.0d0, -85.0d0, -30.0d0, 85.0d0, 30.0d0 )
  pd12 => PSDraw( 4.0d0, 3.0d0, -10.0d0, -10.0d0, 10.0d0, 10.0d0 )
  pd13 => PSDraw( 4.0d0, 2.0d0, -10.0d0, -10.0d0, 10.0d0, 10.0d0 )
  pd14 => PSDraw( 4.0d0, 0.5d0, -10.0d0, -10.0d0, 10.0d0, 10.0d0 )

  pd21 => PSDraw( 4.5d0, 3.5d0, -10.0d0, -10.0d0, 10.0d0, 10.0d0 )
  pd22 => PSDraw( 4.5d0, 2.5d0, -10.0d0, -10.0d0, 10.0d0, 10.0d0 )
  pd23 => PSDraw( 4.0d0, 6.5d0,   0.0d0,   0.0d0,  4.0d0,  6.5d0 )
  
  !/ -------------------------------------------------------------------------------------

  call pd11%drawBorder
  call pd12%drawBorder
  call pd13%drawBorder
  call pd14%drawBorder
  
  call pd21%drawBorder
  call pd22%drawBorder
  call pd23%drawBorder
  
  !/ -------------------------------------------------------------------------------------
  
  call pd11%setRGB( 1.0d0, 0.0d0, 0.0d0 )
  call pd11%drawEllipse( 0.0d0, 0.0d0, 30.0d0, 10.0d0, 45.0d0 )
  call pd11%drawCircle( 0.0d0, 0.0d0, 10.0d0, parts=7 )
  
  call pd12%setRGB( color_blue )
  call pd12%drawLine( -9.0d0, -9.0d0, 9.0d0, 9.0d0 )
  
  call pd14%write( 'Heather', -10.0d0, -10.0d0, 10.0d0, 10.0d0 )
  
  call pd13%write( 'Rebekah', -8.0d0, -8.0d0, 2.0d0, 1.0d0, 30.0d0 )
  
  call pd11%saveColor( )
  
  call pd11%setRGB( color_cyan )
  call pd11%write_inch( 'Stephen', 0.0d0, 0.0d0, 2.0d0, 1.0d0, 30.0d0 )
  
  call pd11%setRGB( color_green )
  call pd11%write_inch( 'Stephen', 0.0d0, 0.0d0, 2.0d0, 1.0d0 )
  
  call pd11%restoreColor( )
  call pd11%drawRay( -85.0d0, 30.0d0, 60.d0, -45.0d0 )
  
  call pd21%setFont( PSFont_NORMAL )
  call pd21%write_inch( 'Roman Font',  0.5d0, 2.5d0, 2.0d0, 0.5d0 )
  call pd21%setFont( PSFont_BOLD )
  call pd21%write_inch( 'Bold Font',   0.5d0, 1.5d0, 2.0d0, 0.5d0 )
  call pd21%setFont( PSFont_ITALIC )
  call pd21%write_inch( 'Italic Font', 0.5d0, 0.5d0, 2.0d0, 0.5d0 )
  
  call pd22%drawRectangle( -8.0d0, -8.0d0, -1.0d0, -1.0d0 )
  call pd22%drawRectangle(  4.0d0,  4.0d0,  6.0d0,  5.0d0, .true. )
  
  !/ -------------------------------------------------------------------------------------
  
  call pd23%drawPolygon( px1, py1, pn1 )
  call pd23%setRGB( color_blue )
  call pd23%drawPolygon( px2, py2, pn2, .true. )
  
  ps => PSGraph( 2 )
  
  call log_info('MARK 1')
  call ps%add( pd11, 1, 1.0d0, 1.0d0 )
  call ps%add( pd12, 1, 1.0d0, 4.5d0 )
  call ps%add( pd13, 1, 5.5d0, 5.5d0 )
  call ps%add( pd14, 1, 5.5d0, 4.5d0 )
  
  call ps%add( pd21, 2, 5.5d0, 1.0d0 )
  call ps%add( pd22, 2, 5.5d0, 5.0d0 )
  call ps%add( pd23, 2, 1.0d0, 1.0d0 )
  
  call log_info('MARK 2')
  call ps%pswrite( 'psgraph-f08.ps' )
  call log_info('MARK 3')
  
end program ftest_psgraph

!/ =======================================================================================
!/ **                             F T E S T _ P S G R A P H                             **
!/ =========================================================================== END FILE ==
