!/ ====================================================================== BEGIN FILE =====
!/ **                                  Z E R O _ M O D                                  **
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


  public :: zero




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  pure subroutine zero_char_1d( str, c )
    !/ -----------------------------------------------------------------------------------
    !! Set each element to null.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),           intent(inout) :: str !! reference to a character string.
    character(len=1), optional, intent(in)    :: c   !! optional fill character.
    !/ -----------------------------------------------------------------------------------
    integer          :: n, i
    !/ -----------------------------------------------------------------------------------

    n = len(str)

    if (present(c)) then
       do concurrent (i=1:n)
          str(i:i) = c
       end do
    else
       do concurrent (i=1:n)
          str(i:i) = ' '
       end do
    end if

  end subroutine zero_char_1d






  !/ =====================================================================================
  pure subroutine zero_I4_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:), intent(inout) :: array !! reference to the array to be zeroed. 
    integer, optional    , intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=1)

    if ( present( SET ) ) then
       do concurrent (i=1:n)
          array(i) = SET
       end do
    else
       do concurrent (i=1:n)
          array(i) = 0
       end do
    end if

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=2)

    do concurrent (i=1:n)
       call zero_I4_1d( array(:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=3)

    do i=1,n
       call zero_I4_2d( array(:,:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=4)

    do i=1,n
       call zero_I4_3d( array(:,:,:,i), SET )
    end do

  end subroutine zero_I4_4d






  !/ =====================================================================================
  pure subroutine zero_R4_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:), intent(inout) :: array !! reference to the array to be zeroed.
    real(sp), optional,     intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=1)

    if ( present( SET ) ) then
       do concurrent (i=1:n)
          array(i) = SET
       end do
    else
       do concurrent (i=1:n)
          array(i) = 0.0e0
       end do
    end if

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=2)

    do i=1,n
       call zero_R4_1d( array(:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=3)

    do i=1,n
       call zero_R4_2d( array(:,:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=4)

    do i=1,n
       call zero_R4_3d( array(:,:,:,i), SET )
    end do

  end subroutine zero_R4_4d






  !/ =====================================================================================
  pure subroutine zero_R8_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(inout) :: array !! reference to the array to be zeroed.
    real(dp), optional,     intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=1)

    if ( present( SET ) ) then
       do concurrent (i=1:n)
          array(i) = SET
       end do
    else
       do concurrent (i=1:n)
          array(i) = 0.0d0
       end do
    end if

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=2)

    do i=1,n
       call zero_R8_1d( array(:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=3)

    do i=1,n
       call zero_R8_2d( array(:,:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=4)

    do i=1,n
       call zero_R8_3d( array(:,:,:,i), SET )
    end do

  end subroutine zero_R8_4d






  !/ =====================================================================================
  pure subroutine zero_C16_1d( array, SET )
    !/ -----------------------------------------------------------------------------------
    !! Set each element of the array to the ZERO value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(inout) :: array !! reference to the array to be zeroed.
    complex(dp), optional,     intent(in)    :: SET   !! optional value to be used to zero.
    !/ -----------------------------------------------------------------------------------
    integer     :: i, n
    complex(dp), parameter :: czero = (0.0d0,0.0d0)
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=1)

    if ( present( SET ) ) then
       do concurrent (i=1:n)
          array(i) = SET
       end do
    else
       do concurrent (i=1:n)
          array(i) = czero
       end do
    end if

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=2)

    do i=1,n
       call zero_C16_1d( array(:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=3)

    do i=1,n
       call zero_C16_2d( array(:,:,i), SET )
    end do

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
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(array,DIM=4)

    do i=1,n
       call zero_C16_3d( array(:,:,:,i), SET )
    end do

  end subroutine zero_C16_4d


end module zero_mod


!/ =======================================================================================
!/ **                                  Z E R O _ M O D                                  **
!/ =========================================================================== END FILE ==
