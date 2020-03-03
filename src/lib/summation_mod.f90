!/ ====================================================================== BEGIN FILE =====
!/ **                             S U M M A T I O N _ M O D                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
module summation_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2015-12-24
  !! license: GPL
  !!
  !!##Summation procedures.
  !!
  !! Provides the interface and procedures for various summations.
  !! Simple summation, sum of the squares, sum of the square differences, sum of
  !! the conjugate squares. Generic routines exist for 1, 2, 3, & 4 rank arrays.
  !! Higher arrays may be summed by passing thier shape.
  !
  !/ -------------------------------------------------------------------------------------
  use constants_env
  implicit none

  public :: sumsq, wsum, dist2


  private :: sum_square_I4_1d
  private :: sum_square_I4_2d
  private :: sum_square_I4_3d
  private :: sum_square_I4_4d

  private :: sum_square_difference_I4_0d
  private :: sum_square_difference_I4_1d
  private :: sum_square_difference_I4_2d
  private :: sum_square_difference_I4_3d
  private :: sum_square_difference_I4_4d

  private :: sum_square_R4_1d
  private :: sum_square_R4_2d
  private :: sum_square_R4_3d
  private :: sum_square_R4_4d

  private :: sum_square_difference_R4_0d
  private :: sum_square_difference_R4_1d
  private :: sum_square_difference_R4_2d
  private :: sum_square_difference_R4_3d
  private :: sum_square_difference_R4_4d

  private :: sum_square_R8_1d
  private :: sum_square_R8_2d
  private :: sum_square_R8_3d
  private :: sum_square_R8_4d

  private :: sum_square_difference_R8_0d
  private :: sum_square_difference_R8_1d
  private :: sum_square_difference_R8_2d
  private :: sum_square_difference_R8_3d
  private :: sum_square_difference_R8_4d

  private :: sum_square_C16_1d
  private :: sum_square_C16_2d
  private :: sum_square_C16_3d
  private :: sum_square_C16_4d

  private :: sum_square_difference_C16_0d
  private :: sum_square_difference_C16_1d
  private :: sum_square_difference_C16_2d
  private :: sum_square_difference_C16_3d
  private :: sum_square_difference_C16_4d

  private :: weighted_sum_R4_1d
  private :: weighted_sum_R4_2d
  private :: weighted_sum_R4_3d
  private :: weighted_sum_R4_4d

  private :: weighted_sum_R8_1d
  private :: weighted_sum_R8_2d
  private :: weighted_sum_R8_3d
  private :: weighted_sum_R8_4d

  private :: weighted_sum_C16_1d
  private :: weighted_sum_C16_2d
  private :: weighted_sum_C16_3d
  private :: weighted_sum_C16_4d

  !/ -------------------------------------------------------------------------------------
  interface sumsq                                 ! sum of squares and square differences.
     !/ ----------------------------------------------------------------------------------

     ! ----- integer arrays ---------------------------
     module procedure :: sum_square_I4_1d
     module procedure :: sum_square_I4_2d
     module procedure :: sum_square_I4_3d
     module procedure :: sum_square_I4_4d

     module procedure :: sum_square_difference_I4_0d
     module procedure :: sum_square_difference_I4_1d
     module procedure :: sum_square_difference_I4_2d
     module procedure :: sum_square_difference_I4_3d
     module procedure :: sum_square_difference_I4_4d

     ! ----- real arrays ------------------------------

     ! --- single precision
     module procedure :: sum_square_R4_1d
     module procedure :: sum_square_R4_2d
     module procedure :: sum_square_R4_3d
     module procedure :: sum_square_R4_4d

     module procedure :: sum_square_difference_R4_0d
     module procedure :: sum_square_difference_R4_1d
     module procedure :: sum_square_difference_R4_2d
     module procedure :: sum_square_difference_R4_3d
     module procedure :: sum_square_difference_R4_4d

     ! --- double precision
     module procedure :: sum_square_R8_1d
     module procedure :: sum_square_R8_2d
     module procedure :: sum_square_R8_3d
     module procedure :: sum_square_R8_4d

     module procedure :: sum_square_difference_R8_0d
     module procedure :: sum_square_difference_R8_1d
     module procedure :: sum_square_difference_R8_2d
     module procedure :: sum_square_difference_R8_3d
     module procedure :: sum_square_difference_R8_4d

     ! ----- complex arrays ---------------------------
     module procedure :: sum_square_C16_1d
     module procedure :: sum_square_C16_2d
     module procedure :: sum_square_C16_3d
     module procedure :: sum_square_C16_4d

     module procedure :: sum_square_difference_C16_0d
     module procedure :: sum_square_difference_C16_1d
     module procedure :: sum_square_difference_C16_2d
     module procedure :: sum_square_difference_C16_3d
     module procedure :: sum_square_difference_C16_4d

  end interface sumsq


  !/ -------------------------------------------------------------------------------------
  interface wsum                                                           ! weighted sums
     !/ ----------------------------------------------------------------------------------

     ! ----- real arrays ------------------------------

     ! --- single precision
     module procedure :: weighted_sum_R4_1d
     module procedure :: weighted_sum_R4_2d
     module procedure :: weighted_sum_R4_3d
     module procedure :: weighted_sum_R4_4d

     ! --- double precision
     module procedure :: weighted_sum_R8_1d
     module procedure :: weighted_sum_R8_2d
     module procedure :: weighted_sum_R8_3d
     module procedure :: weighted_sum_R8_4d

     ! ----- complex arrays ---------------------------
     module procedure :: weighted_sum_C16_1d
     module procedure :: weighted_sum_C16_2d
     module procedure :: weighted_sum_C16_3d
     module procedure :: weighted_sum_C16_4d

  end interface wsum


  !/ -------------------------------------------------------------------------------------
  interface dist2                                   ! alias for euclidian distance squared
     !/ ----------------------------------------------------------------------------------
     module procedure :: sum_square_difference_R8_0d
     module procedure :: sum_square_difference_R8_1d
  end interface dist2




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function sum_square_I4_1d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:), intent(in) :: array !! reference to the array to be summed. 
    integer                           :: total !! sum of the squares: s = sum(i=1,n) A(i)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = size(array)
    total = 0

    !$omp parallel private(i) shared(array,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( array(i) * array(i) )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_I4_1d


  !/ =====================================================================================
  function sum_square_I4_2d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:), intent(in) :: array !! reference to the array to be summed. 
    integer                             :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    total = 0

    !$omp parallel private(i1,i2) shared(array,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( array(i1,i2) * array(i1,i2) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_I4_2d


  !/ =====================================================================================
  function sum_square_I4_3d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:), intent(in) :: array !! reference to the array to be summed. 
    integer                               :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    total = 0

    !$omp parallel private(i1,i2,i3) shared(array,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( array(i1,i2,i3) * array(i1,i2,i3) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_I4_3d


  !/ =====================================================================================
  function sum_square_I4_4d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:,:), intent(in) :: array !! reference to the array to be summed. 
    integer                                 :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    n4    = size(array,DIM=4)
    total = 0

    !$omp parallel private(i1,i2,i3,i4) shared(array,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( array(i1,i2,i3,i4) * array(i1,i2,i3,i4) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_I4_4d




  !/ =====================================================================================
  function sum_square_R4_1d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:), intent(in) :: array !! reference to the array to be summed. 
    real(sp)                           :: total !! sum of the squares: s = sum(i=1,n) A(i)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = size(array)
    total = 0.0e0

    !$omp parallel private(i) shared(array,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( array(i) * array(i) )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R4_1d


  !/ =====================================================================================
  function sum_square_R4_2d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:), intent(in) :: array !! reference to the array to be summed. 
    real(sp)                             :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    total = 0.0e0

    !$omp parallel private(i1,i2) shared(array,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( array(i1,i2) * array(i1,i2) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R4_2d


  !/ =====================================================================================
  function sum_square_R4_3d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:), intent(in) :: array !! reference to the array to be summed. 
    real(sp)                               :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    total = 0.0e0

    !$omp parallel private(i1,i2,i3) shared(array,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( array(i1,i2,i3) * array(i1,i2,i3) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R4_3d


  !/ =====================================================================================
  function sum_square_R4_4d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:,:), intent(in) :: array !! reference to the array to be summed. 
    real(sp)                                 :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    n4    = size(array,DIM=4)
    total = 0.0e0

    !$omp parallel private(i1,i2,i3,i4) shared(array,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( array(i1,i2,i3,i4) * array(i1,i2,i3,i4) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R4_4d








  !/ =====================================================================================
  function sum_square_R8_1d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(in) :: array !! reference to the array to be summed. 
    real(dp)                           :: total !! sum of the squares: s = sum(i=1,n) A(i)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = size(array)
    total = 0.0d0

    !$omp parallel private(i) shared(array,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( array(i) * array(i) )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R8_1d


  !/ =====================================================================================
  function sum_square_R8_2d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:), intent(in) :: array !! reference to the array to be summed. 
    real(dp)                             :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    total = 0.0d0

    !$omp parallel private(i1,i2) shared(array,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( array(i1,i2) * array(i1,i2) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R8_2d


  !/ =====================================================================================
  function sum_square_R8_3d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:), intent(in) :: array !! reference to the array to be summed. 
    real(dp)                               :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    total = 0.0d0

    !$omp parallel private(i1,i2,i3) shared(array,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( array(i1,i2,i3) * array(i1,i2,i3) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R8_3d


  !/ =====================================================================================
  function sum_square_R8_4d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:,:), intent(in) :: array !! reference to the array to be summed. 
    real(dp)                                 :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    n4    = size(array,DIM=4)
    total = 0.0d0

    !$omp parallel private(i1,i2,i3,i4) shared(array,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( array(i1,i2,i3,i4) * array(i1,i2,i3,i4) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_R8_4d








  !/ =====================================================================================
  function sum_square_C16_1d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(in) :: array !! reference to the array to be summed. 
    complex(dp)                           :: total !! sum of the squares: s = sum(i=1,n) A(i)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n    = size(array)
    total = (0.0d0,0.0d0)

    !$omp parallel private(i) shared(array,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( array(i) * conjg(array(i)) )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_C16_1d


  !/ =====================================================================================
  function sum_square_C16_2d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:), intent(in) :: array !! reference to the array to be summed. 
    complex(dp)                             :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    total = (0.0d0,0.0d0)

    !$omp parallel private(i1,i2) shared(array,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( array(i1,i2) * conjg(array(i1,i2)) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_C16_2d


  !/ =====================================================================================
  function sum_square_C16_3d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:), intent(in) :: array !! reference to the array to be summed. 
    complex(dp)                               :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    total = (0.0d0,0.0d0)

    !$omp parallel private(i1,i2,i3) shared(array,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( array(i1,i2,i3) * conjg(array(i1,i2,i3)) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_C16_3d


  !/ =====================================================================================
  function sum_square_C16_4d( array ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square of each element of the array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:,:), intent(in) :: array !! reference to the array to be summed. 
    complex(dp)                                 :: total !! sum of the squares: s = sum(i=1,n) A(i)**2
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = size(array,DIM=1)
    n2    = size(array,DIM=2)
    n3    = size(array,DIM=3)
    n4    = size(array,DIM=4)
    total = (0.0d0,0.0d0)

    !$omp parallel private(i1,i2,i3,i4) shared(array,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( array(i1,i2,i3,i4) * conjg(array(i1,i2,i3,i4)) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_C16_4d




  !/ =====================================================================================
  function sum_square_difference_I4_0d( x1, x2 ) result( d2 )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between two scalar values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: x1 !! reference to the first scalar.
    integer, intent(in) :: x2 !! reference to the second scalar.
    integer             :: d2 !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    integer             :: d
    !/ -----------------------------------------------------------------------------------

    d  = ( x1 - x2 )
    d2 = d * d

  end function sum_square_difference_I4_0d


  !/ =====================================================================================
  function sum_square_difference_I4_1d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between corresponding elements of the two arrays array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    integer, dimension(:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    integer                           :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    integer :: diff
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(lhs), size(rhs) )
    total = 0

    !$omp parallel private(i,diff) shared(lhs,rhs,n)
    !$omp do reduction (+:total)
    do i=1,n
       diff  = lhs(i) - rhs(i)
       total = total + ( diff * diff )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_I4_1d


  !/ =====================================================================================
  function sum_square_difference_I4_2d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    integer, dimension(:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    integer                             :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    integer :: diff
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    total = 0

    !$omp parallel private(i1,i2,diff) shared(lhs,rhs,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          diff  = lhs(i1,i2) - rhs(i1,i2)
          total = total + ( diff * diff )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_I4_2d


  !/ =====================================================================================
  function sum_square_difference_I4_3d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    integer, dimension(:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    integer                               :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    integer :: diff
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    total = 0

    !$omp parallel private(i1,i2,i3,diff) shared(lhs,rhs,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             diff  = lhs(i1,i2,i3) - rhs(i1,i2,i3)
             total = total + ( diff * diff )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_I4_3d


  !/ =====================================================================================
  function sum_square_difference_I4_4d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    integer, dimension(:,:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    integer                                 :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    integer :: diff
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    n4    = min( size(lhs,DIM=4), size(rhs,DIM=4) )
    total = 0

    !$omp parallel private(i1,i2,i3,i4,diff) shared(lhs,rhs,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                diff  = lhs(i1,i2,i3,i4) - rhs(i1,i2,i3,i4)
                total = total + ( diff * diff )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_I4_4d








  !/ =====================================================================================
  function sum_square_difference_R4_0d( x1, x2 ) result( d2 )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between two scalar values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), intent(in) :: x1 !! reference to the first scalar.
    real(sp), intent(in) :: x2 !! reference to the second scalar.
    real(sp)             :: d2 !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(sp)             :: d
    !/ -----------------------------------------------------------------------------------

    d  = ( x1 - x2 )
    d2 = d * d

  end function sum_square_difference_R4_0d


  !/ =====================================================================================
  function sum_square_difference_R4_1d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between corresponding elements of the two arrays array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(sp), dimension(:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(sp)                           :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(sp) :: diff
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(lhs), size(rhs) )
    total = 0.0e0

    !$omp parallel private(i,diff) shared(lhs,rhs,n)
    !$omp do reduction (+:total)
    do i=1,n
       diff  = lhs(i) - rhs(i)
       total = total + ( diff * diff )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R4_1d


  !/ =====================================================================================
  function sum_square_difference_R4_2d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(sp), dimension(:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(sp)                             :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(sp) :: diff
    integer  :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    total = 0.0e0

    !$omp parallel private(i1,i2,diff) shared(lhs,rhs,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          diff  = lhs(i1,i2) - rhs(i1,i2)
          total = total + ( diff * diff )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R4_2d


  !/ =====================================================================================
  function sum_square_difference_R4_3d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(sp), dimension(:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(sp)                               :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(sp) :: diff
    integer  :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    total = 0.0e0

    !$omp parallel private(i1,i2,i3,diff) shared(lhs,rhs,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             diff  = lhs(i1,i2,i3) - rhs(i1,i2,i3)
             total = total + ( diff * diff )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R4_3d


  !/ =====================================================================================
  function sum_square_difference_R4_4d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(sp), dimension(:,:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(sp)                                 :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(sp) :: diff
    integer  :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    n4    = min( size(lhs,DIM=4), size(rhs,DIM=4) )
    total = 0.0e0

    !$omp parallel private(i1,i2,i3,i4,diff) shared(lhs,rhs,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                diff  = lhs(i1,i2,i3,i4) - rhs(i1,i2,i3,i4)
                total = total + ( diff * diff )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R4_4d




  !/ =====================================================================================
  function sum_square_difference_R8_0d( x1, x2 ) result( d2 )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between two scalar values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: x1 !! reference to the first scalar.
    real(dp), intent(in) :: x2 !! reference to the second scalar.
    real(dp)             :: d2 !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(dp)             :: d
    !/ -----------------------------------------------------------------------------------

    d  = ( x1 - x2 )
    d2 = d * d

  end function sum_square_difference_R8_0d


  !/ =====================================================================================
  function sum_square_difference_R8_1d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between corredponding elements of the two arrays array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(dp), dimension(:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(dp)                           :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(lhs), size(rhs) )
    total = 0.0d0

    !$omp parallel private(i,diff) shared(lhs,rhs,n)
    !$omp do reduction (+:total)
    do i=1,n
       diff  = lhs(i) - rhs(i)
       total = total + ( diff * diff )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R8_1d


  !/ =====================================================================================
  function sum_square_difference_R8_2d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(dp), dimension(:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(dp)                             :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff
    integer  :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    total = 0.0d0

    !$omp parallel private(i1,i2,diff) shared(lhs,rhs,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          diff  = lhs(i1,i2) - rhs(i1,i2)
          total = total + ( diff * diff )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R8_2d


  !/ =====================================================================================
  function sum_square_difference_R8_3d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(dp), dimension(:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(dp)                               :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff
    integer  :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    total = 0.0d0

    !$omp parallel private(i1,i2,i3,diff) shared(lhs,rhs,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             diff  = lhs(i1,i2,i3) - rhs(i1,i2,i3)
             total = total + ( diff * diff )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R8_3d


  !/ =====================================================================================
  function sum_square_difference_R8_4d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    real(dp), dimension(:,:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    real(dp)                                 :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff
    integer  :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    n4    = min( size(lhs,DIM=4), size(rhs,DIM=4) )
    total = 0.0d0

    !$omp parallel private(i1,i2,i3,i4,diff) shared(lhs,rhs,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                diff  = lhs(i1,i2,i3,i4) - rhs(i1,i2,i3,i4)
                total = total + ( diff * diff )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_R8_4d








  !/ =====================================================================================
  function sum_square_difference_C16_0d( x1, x2 ) result( d2 )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between two scalar values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), intent(in) :: x1 !! reference to the first scalar.
    complex(dp), intent(in) :: x2 !! reference to the second scalar.
    complex(dp)             :: d2 !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    complex(dp)             :: d
    !/ -----------------------------------------------------------------------------------

    d  = ( x1 - x2 )
    d2 = d * conjg(d)

  end function sum_square_difference_C16_0d


  !/ =====================================================================================
  function sum_square_difference_C16_1d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Sum the square difference between corredponding elements of the two arrays array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    complex(dp), dimension(:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    complex(dp)                           :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    complex(dp) :: diff
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(lhs), size(rhs) )
    total = (0.0d0,0.0d0)

    !$omp parallel private(i,diff) shared(lhs,rhs,n)
    !$omp do reduction (+:total)
    do i=1,n
       diff  = lhs(i) - rhs(i)
       total = total + ( diff * conjg(diff) )
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_C16_1d


  !/ =====================================================================================
  function sum_square_difference_C16_2d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    complex(dp), dimension(:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    complex(dp)                             :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    complex(dp) :: diff
    integer  :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    total = (0.0d0,0.0d0)

    !$omp parallel private(i1,i2,diff) shared(lhs,rhs,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          diff  = lhs(i1,i2) - rhs(i1,i2)
          total = total + ( diff * conjg(diff) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_C16_2d


  !/ =====================================================================================
  function sum_square_difference_C16_3d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    complex(dp), dimension(:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    complex(dp)                               :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    complex(dp) :: diff
    integer  :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    total = (0.0d0,0.0d0)

    !$omp parallel private(i1,i2,i3,diff) shared(lhs,rhs,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             diff  = lhs(i1,i2,i3) - rhs(i1,i2,i3)
             total = total + ( diff * conjg(diff) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_C16_3d


  !/ =====================================================================================
  function sum_square_difference_C16_4d( lhs, rhs ) result( total )
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:,:), intent(in) :: lhs   !! reference to the left  hand side array to be summed.
    complex(dp), dimension(:,:,:,:), intent(in) :: rhs   !! reference to the right hand side array to be summed.
    complex(dp)                                 :: total !! sum of the square difference.
    !/ -----------------------------------------------------------------------------------
    complex(dp) :: diff
    integer  :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(lhs,DIM=1), size(rhs,DIM=1) )
    n2    = min( size(lhs,DIM=2), size(rhs,DIM=2) )
    n3    = min( size(lhs,DIM=3), size(rhs,DIM=3) )
    n4    = min( size(lhs,DIM=4), size(rhs,DIM=4) )
    total = (0.0d0,0.0d0)

    !$omp parallel private(i1,i2,i3,i4,diff) shared(lhs,rhs,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                diff  = lhs(i1,i2,i3,i4) - rhs(i1,i2,i3,i4)
                total = total + ( diff * conjg(diff) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function sum_square_difference_C16_4d




  !/ =====================================================================================
  function weighted_sum_R4_1d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:), intent(in) :: W     !! reference to the weight array.
    real(sp), dimension(:), intent(in) :: X     !! reference to the value array.
    real(sp)                           :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(W), size(X) )
    total = 0.0e0

    !$omp parallel private(i) shared(W,X,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( W(i) * X(i) )
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R4_1d


  !/ =====================================================================================
  function weighted_sum_R4_2d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:), intent(in) :: W     !! reference to the weight array.
    real(sp), dimension(:,:), intent(in) :: X     !! reference to the value array.
    real(sp)                             :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    total = 0.0e0

    !$omp parallel private(i1,i2) shared(W,X,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( W(i1,i2) * X(i1,i2) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R4_2d


  !/ =====================================================================================
  function weighted_sum_R4_3d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:), intent(in) :: W     !! reference to the weight array.
    real(sp), dimension(:,:,:), intent(in) :: X     !! reference to the value array.
    real(sp)                               :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    n3    = min( size(W,DIM=3), size(X,DIM=3) )
    total = 0.0e0

    !$omp parallel private(i1,i2,i3) shared(W,X,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( W(i1,i2,i3) * X(i1,i2,i3) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R4_3d


  !/ =====================================================================================
  function weighted_sum_R4_4d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:,:), intent(in) :: W     !! reference to the weight array.
    real(sp), dimension(:,:,:,:), intent(in) :: X     !! reference to the value array.
    real(sp)                                 :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    n3    = min( size(W,DIM=3), size(X,DIM=3) )
    n4    = min( size(W,DIM=4), size(X,DIM=4) )
    total = 0.0e0

    !$omp parallel private(i1,i2,i3,i4) shared(W,X,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( W(i1,i2,i3,i4) * X(i1,i2,i3,i4) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R4_4d








  !/ =====================================================================================
  function weighted_sum_R8_1d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(in) :: W     !! reference to the weight array.
    real(dp), dimension(:), intent(in) :: X     !! reference to the value array.
    real(dp)                           :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(W), size(X) )
    total = 0.0d0

    !$omp parallel private(i) shared(W,X,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( W(i) * X(i) )
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R8_1d


  !/ =====================================================================================
  function weighted_sum_R8_2d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:), intent(in) :: W     !! reference to the weight array.
    real(dp), dimension(:,:), intent(in) :: X     !! reference to the value array.
    real(dp)                             :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    total = 0.0d0

    !$omp parallel private(i1,i2) shared(W,X,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( W(i1,i2) * X(i1,i2) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R8_2d


  !/ =====================================================================================
  function weighted_sum_R8_3d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:), intent(in) :: W     !! reference to the weight array.
    real(dp), dimension(:,:,:), intent(in) :: X     !! reference to the value array.
    real(dp)                               :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    n3    = min( size(W,DIM=3), size(X,DIM=3) )
    total = 0.0d0

    !$omp parallel private(i1,i2,i3) shared(W,X,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( W(i1,i2,i3) * X(i1,i2,i3) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R8_3d


  !/ =====================================================================================
  function weighted_sum_R8_4d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:,:), intent(in) :: W     !! reference to the weight array.
    real(dp), dimension(:,:,:,:), intent(in) :: X     !! reference to the value array.
    real(dp)                                 :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    n3    = min( size(W,DIM=3), size(X,DIM=3) )
    n4    = min( size(W,DIM=4), size(X,DIM=4) )
    total = 0.0d0

    !$omp parallel private(i1,i2,i3,i4) shared(W,X,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( W(i1,i2,i3,i4) * X(i1,i2,i3,i4) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_R8_4d




  !/ =====================================================================================
  function weighted_sum_C16_1d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(in) :: W     !! reference to the weight array.
    complex(dp), dimension(:), intent(in) :: X     !! reference to the value array.
    complex(dp)                           :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n     = min( size(W), size(X) )
    total = (0.0e0,0.0d0)

    !$omp parallel private(i) shared(W,X,n)
    !$omp do reduction (+:total)
    do i=1,n
       total = total + ( W(i) * X(i) )
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_C16_1d


  !/ =====================================================================================
  function weighted_sum_C16_2d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:), intent(in) :: W     !! reference to the weight array.
    complex(dp), dimension(:,:), intent(in) :: X     !! reference to the value array.
    complex(dp)                             :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    total = (0.0e0,0.0d0)

    !$omp parallel private(i1,i2) shared(W,X,n1,n2)
    !$omp do reduction (+:total)
    do i2=1,n2
       do i1=1,n1
          total = total + ( W(i1,i2) * X(i1,i2) )
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_C16_2d


  !/ =====================================================================================
  function weighted_sum_C16_3d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:), intent(in) :: W     !! reference to the weight array.
    complex(dp), dimension(:,:,:), intent(in) :: X     !! reference to the value array.
    complex(dp)                               :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    n3    = min( size(W,DIM=3), size(X,DIM=3) )
    total = (0.0e0,0.0d0)

    !$omp parallel private(i1,i2,i3) shared(W,X,n1,n2,n3)
    !$omp do reduction (+:total)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             total = total + ( W(i1,i2,i3) * X(i1,i2,i3) )
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_C16_3d


  !/ =====================================================================================
  function weighted_sum_C16_4d( W, X ) result( total )
    !/ -----------------------------------------------------------------------------------
    !! Perform a weighted sum: s = sum(i=1,n) W(i)*X(i)
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:,:), intent(in) :: W     !! reference to the weight array.
    complex(dp), dimension(:,:,:,:), intent(in) :: X     !! reference to the value array.
    complex(dp)                                 :: total !! weighted sum of the elements.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1    = min( size(W,DIM=1), size(X,DIM=1) )
    n2    = min( size(W,DIM=2), size(X,DIM=2) )
    n3    = min( size(W,DIM=3), size(X,DIM=3) )
    n4    = min( size(W,DIM=4), size(X,DIM=4) )
    total = (0.0e0,0.0d0)

    !$omp parallel private(i1,i2,i3,i4) shared(W,X,n1,n2,n3,n4)
    !$omp do reduction (+:total)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                total = total + ( W(i1,i2,i3,i4) * X(i1,i2,i3,i4) )
             end do
          end do
       end do
    end do
    !$omp end do
    !$omp end parallel

  end function weighted_sum_C16_4d


end module summation_mod


!/ =======================================================================================
!/ **                             S U M M A T I O N _ M O D                             **
!/ =========================================================================== END FILE ==
