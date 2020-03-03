!/ ====================================================================== BEGIN FILE =====
!/ **                                  M A T H _ A U X                                  **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
module math_aux
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2015-10-23
  !! license: GPL
  !!
  !!## Auxillary Mathematical Procedures
  !!
  !! Provides a set of additional math procedures.
  !!
  !! todo: add a four quadrant arctangent procedure.
  !
  !/ -------------------------------------------------------------------------------------
  use constants_env
  implicit none

  public :: nsum, n2sum, rotate

  private :: sum_one_to_n, sumsq_one_to_n

  !/ -------------------------------------------------------------------------------------
  interface nsum
     !/ ----------------------------------------------------------------------------------
     module procedure :: sum_one_to_n
  end interface nsum

  !/ -------------------------------------------------------------------------------------
  interface n2sum
     !/ ----------------------------------------------------------------------------------
     module procedure :: sumsq_one_to_n
  end interface n2sum

  !/ -------------------------------------------------------------------------------------
  interface rotate
     !/ ----------------------------------------------------------------------------------
     module procedure :: rotate_2d
     module procedure :: rotate_2d_array
  end interface rotate


  !/ -------------------------------------------------------------------------------------
  interface area
     !/ ----------------------------------------------------------------------------------
     module procedure :: area_triangle
  end interface area



  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  pure subroutine fraction( whole_part, fraction_part, x, MULTIPLY, DIVIDE )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),            intent(out) :: whole_part
    real(dp),            intent(out) :: fraction_part
    real(dp),           intent(in)   :: x
    real(dp), optional, intent(in)   :: MULTIPLY
    real(dp), optional, intent(in)   :: DIVIDE
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp
    !/ -----------------------------------------------------------------------------------
    temp = x

    if ( present( MULTIPLY ) ) temp = temp * MULTIPLY
    if ( present( DIVIDE ) )   temp = temp / DIVIDE
    
    whole_part    = real(floor(temp),dp)
    fraction_part = temp - whole_part
  end subroutine fraction
  

  !/ =====================================================================================
  function area_triangle( a, b, c ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Area of an arbitrary triangle where the length of all three sides in known.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: a  !! length of the first  side
    real(dp), intent(in) :: b  !! length of the second side
    real(dp), intent(in) :: c  !! length of the third  side
    real(dp)             :: x  !! area
    !/ -----------------------------------------------------------------------------------
    real(dp) :: s  !! semiperimeter of the triangle
    !/ -----------------------------------------------------------------------------------
    s = D_HALF*(a+b+c)
    x = sqrt(s*(s-a)*(s-b)*(s-c))
  end function area_triangle

  
  !/ =====================================================================================
  function center_radian( rad ) result( mid )
    !/ -----------------------------------------------------------------------------------
    !/ Find the mean angle using radians.
    !/ -----------------------------------------------------------------------------------
    real(dp), optional, intent(in) :: rad(:)  !! radians
    real(dp)                       :: mid     !! mid
    !/ -----------------------------------------------------------------------------------
    real(dp) :: C, S
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------
    C=D_ZERO
    S=D_ZERO
    n=size(rad)
    do i=1,n
       C = C + cos(rad(i))
       S = S + sin(rad(i))
    end do
    mid = atan( S / C )
  end function center_radian


  !/ =====================================================================================
  function center_degree( deg ) result( mid )
    !/ -----------------------------------------------------------------------------------
    !/ Find the mean angle using degrees.
    !/ -----------------------------------------------------------------------------------
    real(dp), optional, intent(in) :: deg(:)  !! degrees
    real(dp)                       :: mid     !! mid
    !/ -----------------------------------------------------------------------------------
    real(dp) :: C, S
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------
    C=D_ZERO
    S=D_ZERO
    n=size(deg)
    do i=1,n
       C = C + cos(deg(i) * DEG2RAD)
       S = S + sin(deg(i) * DEG2RAD)
    end do
    mid = atan( S / C ) * RAD2DEG
  end function center_degree


  !/ =====================================================================================
  subroutine rotate_2d( xr, yr, x, y, theta )
    !/ -----------------------------------------------------------------------------------
    !! Two dimensional affine rotation from positive x towards positive y 
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: xr  !! rotated  x coordinate
    real(dp), intent(out) :: yr  !! rotated  y coordinate
    real(dp), intent(in)  :: x   !! original x coordinate
    real(dp), intent(in)  :: y   !! original y coordinate
    real(dp), intent(in)  :: theta !! rotation angle in radians from positive x to positive y
    !/ -----------------------------------------------------------------------------------
    real(dp) :: C, S
    !/ -----------------------------------------------------------------------------------
    C  = cos(theta)
    S  = sin(theta)
    xr = C*x - S*y
    yr = S*x + C*y
  end subroutine rotate_2d

  
  !/ =====================================================================================
  subroutine rotate_2d_array( xr, yr, x, y, theta )
    !/ -----------------------------------------------------------------------------------
    !! Two dimensional affine rotation from positive x towards positive y 
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: xr(:)  !! rotated  x coordinate
    real(dp), intent(out) :: yr(:)  !! rotated  y coordinate
    real(dp), intent(in)  :: x(:)   !! original x coordinate
    real(dp), intent(in)  :: y(:)   !! original y coordinate
    real(dp), intent(in)  :: theta  !! rotation angle in radians from positive x to positive y
    !/ -----------------------------------------------------------------------------------
    real(dp) :: C, S
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------
    C  = cos(theta)
    S  = sin(theta)

    n = size(x)

    do concurrent (i=1:n)
       xr(i) = C*x(i) - S*y(i)
       yr(i) = S*x(i) + C*y(i)
    end do
    
  end subroutine rotate_2d_array

  
  
  !/ =====================================================================================
  pure function sum_one_to_n( n ) result( s )
    !/ -----------------------------------------------------------------------------------
    !! Sum all integers one to n inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: n !! number to sum to
    integer             :: s !! resulting sum
    !/ -----------------------------------------------------------------------------------
    s = n*(n + 1)/2
  end function sum_one_to_n

  
   !/ =====================================================================================
  pure function sumsq_one_to_n( n ) result( s )
    !/ -----------------------------------------------------------------------------------
    !! Sum all squares of integers one to n inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: n !! number to sum to
    integer             :: s !! resulting sum
    !/ -----------------------------------------------------------------------------------
    s = n*(n + 1)*(2*n + 1)/6
    !s = n*n*n/3 + n*n/2 + n/6
  end function sumsq_one_to_n

  
  end module math_aux

  
  !/ =======================================================================================
  !/ **                                  M A T H _ A U X                                  **
  !/ =========================================================================== END FILE ==
