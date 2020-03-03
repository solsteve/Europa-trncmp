!/ ====================================================================== BEGIN FILE =====
!/ **                                  Z E R O _ M O D                                  **
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
module zero_mod
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2015-10-23
  !! license: GPL
  !!
  !!##Zero and Set procedures.
  !!
  !! Zero/sets arrays arrays of multiple dimensions and types.
  !
  !/ -------------------------------------------------------------------------------------
  use constants_env
  implicit none

  !/ -------------------------------------------------------------------------------------

  public :: zero

  private :: zero_char_1d

  private :: zero_I4_1d
  private :: zero_R4_1d
  private :: zero_R8_1d
  private :: zero_C16_1d

  private :: zero_I4_2d
  private :: zero_R4_2d
  private :: zero_R8_2d
  private :: zero_C16_2d

  private :: zero_I4_3d
  private :: zero_R4_3d
  private :: zero_R8_3d
  private :: zero_C16_3d

  private :: zero_I4_4d
  private :: zero_R4_4d
  private :: zero_R8_4d
  private :: zero_C16_4d


  !/ -------------------------------------------------------------------------------------
  interface zero                                             ! zero the elements of arrays
     !/ ----------------------------------------------------------------------------------

     module procedure :: zero_char_1d

     ! ----- integer arrays ---------------------------

     module procedure :: zero_I4_1d
     module procedure :: zero_I4_2d
     module procedure :: zero_I4_3d
     module procedure :: zero_I4_4d

     ! ----- real arrays ------------------------------

     ! --- single precision

     module procedure :: zero_R4_1d
     module procedure :: zero_R4_2d
     module procedure :: zero_R4_3d
     module procedure :: zero_R4_4d

     ! --- double precision

     module procedure :: zero_R8_1d
     module procedure :: zero_R8_2d
     module procedure :: zero_R8_3d
     module procedure :: zero_R8_4d

     ! ----- complex arrays ---------------------------

     module procedure :: zero_C16_1d
     module procedure :: zero_C16_2d
     module procedure :: zero_C16_3d
     module procedure :: zero_C16_4d

  end interface zero




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine zero_char_1d( str, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element to null.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),           intent(inout) :: str !! reference to a character string.
    character(len=1), optional, intent(in)    :: SET  !! optional fill character.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    character(len=1) :: value
    !/ -----------------------------------------------------------------------------------

    value = ' '
    if ( present( SET ) ) value = set

    n = len(str)

    !$omp parallel do private(i) shared(str,value,n)
    do i=1,n
       str(i:i) = value
    end do
    !$omp end parallel do

  end subroutine zero_char_1d




  !/ =====================================================================================
  subroutine zero_I4_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:), intent(inout) :: array !! reference to the array to be zeroed. 
    integer, optional    , intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    integer :: value
    !/ -----------------------------------------------------------------------------------

    value = 0
    if ( present( SET ) ) value = SET

    n = size(array,DIM=1)

    !$omp parallel do private(i) shared(array,value,n)
    do i=1,n
       array(i) = value
    end do
    !$omp end parallel do

  end subroutine zero_I4_1d


  !/ =====================================================================================
  subroutine zero_I4_2d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:), intent(inout) :: array !! reference to the array to be zeroed. 
    integer, optional,       intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, n1, n2
    integer :: value
    !/ -----------------------------------------------------------------------------------

    value = 0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)

    !$omp parallel do private(i1,i2) shared(array,value,n1,n2)
    do i2=1,n2
       do i1=1,n1
          array(i1,i2) = value
       end do
    end do
    !$omp end parallel do

  end subroutine zero_I4_2d


  !/ =====================================================================================
  subroutine zero_I4_3d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:), intent(inout) :: array !! reference to the array to be zeroed. 
    integer, optional,         intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, n1, n2, n3
    integer :: value
    !/ -----------------------------------------------------------------------------------

    value = 0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(array,value,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             array(i1,i2,i3) = value
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_I4_3d


  !/ =====================================================================================
  subroutine zero_I4_4d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    integer, optional,           intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    integer :: value
    !/ -----------------------------------------------------------------------------------

    value = 0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)
    n4 = size(array,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(array,value,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                array(i1,i2,i3,i4) = value
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_I4_4d




  !/ =====================================================================================
  subroutine zero_R4_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:), intent(inout) :: array !! reference to the array to be zeroed.
    real(sp), optional,     intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(sp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0e0
    if ( present( SET ) ) value = SET

    n = size(array,DIM=1)

    !$omp parallel do private(i) shared(array,value,n)
    do i=1,n
       array(i) = value
    end do
    !$omp end parallel do

  end subroutine zero_R4_1d


  !/ =====================================================================================
  subroutine zero_R4_2d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:), intent(inout) :: array !! reference to the array to be zeroed.
    real(sp), optional,       intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, n1, n2
    real(sp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0e0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)

    !$omp parallel do private(i1,i2) shared(array,value,n1,n2)
    do i2=1,n2
       do i1=1,n1
          array(i1,i2) = value
       end do
    end do
    !$omp end parallel do

  end subroutine zero_R4_2d


  !/ =====================================================================================
  subroutine zero_R4_3d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    real(sp), optional,         intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, i3, n1, n2, n3
    real(sp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0e0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(array,value,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             array(i1,i2,i3) = value
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_R4_3d


  !/ =====================================================================================
  subroutine zero_R4_4d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    real(sp), optional,           intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, i3, i4, n1, n2, n3, n4
    real(sp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0e0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)
    n4 = size(array,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(array,value,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                array(i1,i2,i3,i4) = value
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_R4_4d




  !/ =====================================================================================
  subroutine zero_R8_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(inout) :: array !! reference to the array to be zeroed.
    real(dp), optional,     intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0d0
    if ( present( SET ) ) value = SET

    n = size(array,DIM=1)

    !$omp parallel do private(i) shared(array,value,n)
    do i=1,n
       array(i) = value
    end do
    !$omp end parallel do

  end subroutine zero_R8_1d


  !/ =====================================================================================
  subroutine zero_R8_2d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:), intent(inout) :: array !! reference to the array to be zeroed.
    real(dp), optional,       intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, n1, n2
    real(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0d0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)

    !$omp parallel do private(i1,i2) shared(array,value,n1,n2)
    do i2=1,n2
       do i1=1,n1
          array(i1,i2) = value
       end do
    end do
    !$omp end parallel do

  end subroutine zero_R8_2d


  !/ =====================================================================================
  subroutine zero_R8_3d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    real(dp), optional,         intent(in)    :: SET   !! optional value to be used to zero
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, i3, n1, n2, n3
    real(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0d0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(array,value,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             array(i1,i2,i3) = value
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_R8_3d


  !/ =====================================================================================
  subroutine zero_R8_4d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    real(dp), optional,           intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, i3, i4, n1, n2, n3, n4
    real(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = 0.0d0
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)
    n4 = size(array,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(array,value,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                array(i1,i2,i3,i4) = value
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_R8_4d




  !/ =====================================================================================
  subroutine zero_C16_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(inout) :: array !! reference to the array to be zeroed.
    complex(dp), optional,     intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    complex(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = (0.0d0, 0.0d0)
    if ( present( SET ) ) value = SET

    n = size(array,DIM=1)

    !$omp parallel do private(i) shared(array,value,n)
    do i=1,n
       array(i) = value
    end do
    !$omp end parallel do

  end subroutine zero_C16_1d


  !/ =====================================================================================
  subroutine zero_C16_2d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:), intent(inout) :: array !! reference to the array to be zeroed.
    complex(dp), optional,       intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, n1, n2
    complex(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = (0.0d0, 0.0d0)
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)

    !$omp parallel do private(i1,i2) shared(array,value,n1,n2)
    do i2=1,n2
       do i1=1,n1
          array(i1,i2) = value
       end do
    end do
    !$omp end parallel do

  end subroutine zero_C16_2d


  !/ =====================================================================================
  subroutine zero_C16_3d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    complex(dp), optional,         intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, i3, n1, n2, n3
    complex(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = (0.0d0, 0.0d0)
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(array,value,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             array(i1,i2,i3) = value
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_C16_3d


  !/ =====================================================================================
  subroutine zero_C16_4d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:,:), intent(inout) :: array !! reference to the array to be zeroed.
    complex(dp), optional,           intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i1, i2, i3, i4, n1, n2, n3, n4
    complex(dp) :: value
    !/ -----------------------------------------------------------------------------------

    value = (0.0d0, 0.0d0)
    if ( present( SET ) ) value = SET

    n1 = size(array,DIM=1)
    n2 = size(array,DIM=2)
    n3 = size(array,DIM=3)
    n4 = size(array,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(array,value,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                array(i1,i2,i3,i4) = value
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine zero_C16_4d


end module zero_mod


!/ =======================================================================================
!/ **                                  Z E R O _ M O D                                  **
!/ =========================================================================== END FILE ==
