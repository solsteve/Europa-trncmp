!/ ====================================================================== BEGIN FILE =====
!/ **                                  C O P Y _ M O D                                  **
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
module copy_mod
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2015-10-23
  !! license: GPL
  !!
  !!##Copy procedures.
  !!
  !! Copies arrays of multiple dimensions and types.
  !
  !/ -------------------------------------------------------------------------------------
  use constants_env
  implicit none

  !/ -------------------------------------------------------------------------------------

  private :: copy_char_1d
  private :: copy_I4_1d
  private :: copy_R4_1d
  private :: copy_R8_1d
  private :: copy_C16_1d


  !/ -------------------------------------------------------------------------------------
  interface copy                                             ! copy the elements of arrays
     !/ ----------------------------------------------------------------------------------

     module procedure :: copy_char_1d

     ! ----- integer arrays ---------------------------

     module procedure :: copy_I4_1d
     module procedure :: copy_I4_2d
     module procedure :: copy_I4_3d
     module procedure :: copy_I4_4d

     ! ----- real arrays ------------------------------

     ! --- single precision

     module procedure :: copy_R4_1d
     module procedure :: copy_R4_2d
     module procedure :: copy_R4_3d
     module procedure :: copy_R4_4d

     ! --- double precision

     module procedure :: copy_R8_1d
     module procedure :: copy_R8_2d
     module procedure :: copy_R8_3d
     module procedure :: copy_R8_4d

     ! ----- complex arrays ---------------------------

     module procedure :: copy_C16_1d
     module procedure :: copy_C16_2d
     module procedure :: copy_C16_3d
     module procedure :: copy_C16_4d

  end interface copy


  public :: copy




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  pure subroutine copy_char_1d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy all the characters from the src array to the dst array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*), intent(inout) :: dst !! reference to a destination character string.
    character(len=*), intent(in)    :: src !! reference to a source character string.
    !/ -----------------------------------------------------------------------------------

    dst = src

  end subroutine copy_char_1d






  !/ =====================================================================================
  pure subroutine copy_I4_1d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:), intent(inout) :: dst !! reference to the destination array.
    integer, dimension(:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=1)

    do concurrent (i=1:n)
       dst(i) = src(i)
    end do

  end subroutine copy_I4_1d


  !/ =====================================================================================
  subroutine copy_I4_2d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:), intent(inout) :: dst !! reference to the destination array.
    integer, dimension(:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=2)

    do i=1,n
       call copy_I4_1d( dst(:,i), src(:,i) )
    end do

  end subroutine copy_I4_2d


  !/ =====================================================================================
  subroutine copy_I4_3d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:), intent(inout) :: dst !! reference to the destination array.
    integer, dimension(:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=3)

    do i=1,n
       call copy_I4_2d( dst(:,:,i), src(:,:,i) )
    end do

  end subroutine copy_I4_3d


  !/ =====================================================================================
  subroutine copy_I4_4d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, dimension(:,:,:,:), intent(inout) :: dst !! reference to the destination array.
    integer, dimension(:,:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=4)

    do i=1,n
       call copy_I4_3d( dst(:,:,:,i), src(:,:,:,i) )
    end do

  end subroutine copy_I4_4d






  !/ =====================================================================================
  pure subroutine copy_R4_1d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:), intent(inout) :: dst !! reference to the destination array.
    real(sp), dimension(:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=1)

    do concurrent (i=1:n)
       dst(i) = src(i)
    end do

  end subroutine copy_R4_1d


  !/ =====================================================================================
  subroutine copy_R4_2d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:), intent(inout) :: dst !! reference to the destination array.
    real(sp), dimension(:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=2)

    do i=1,n
       call copy_R4_1d( dst(:,i), src(:,i) )
    end do

  end subroutine copy_R4_2d


  !/ =====================================================================================
  subroutine copy_R4_3d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:), intent(inout) :: dst !! reference to the destination array.
    real(sp), dimension(:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=3)

    do i=1,n
       call copy_R4_2d( dst(:,:,i), src(:,:,i) )
    end do

  end subroutine copy_R4_3d


  !/ =====================================================================================
  subroutine copy_R4_4d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), dimension(:,:,:,:), intent(inout) :: dst !! reference to the destination array.
    real(sp), dimension(:,:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=4)

    do i=1,n
       call copy_R4_3d( dst(:,:,:,i), src(:,:,:,i) )
    end do

  end subroutine copy_R4_4d






  !/ =====================================================================================
  pure subroutine copy_R8_1d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(inout) :: dst !! reference to the destination array.
    real(dp), dimension(:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=1)

    do concurrent (i=1:n)
       dst(i) = src(i)
    end do

  end subroutine copy_R8_1d


  !/ =====================================================================================
  subroutine copy_R8_2d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:), intent(inout) :: dst !! reference to the destination array.
    real(dp), dimension(:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=2)

    do i=1,n
       call copy_R8_1d( dst(:,i), src(:,i) )
    end do

  end subroutine copy_R8_2d


  !/ =====================================================================================
  subroutine copy_R8_3d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:), intent(inout) :: dst !! reference to the destination array.
    real(dp), dimension(:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=3)

    do i=1,n
       call copy_R8_2d( dst(:,:,i), src(:,:,i) )
    end do

  end subroutine copy_R8_3d


  !/ =====================================================================================
  subroutine copy_R8_4d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:,:,:), intent(inout) :: dst !! reference to the destination array.
    real(dp), dimension(:,:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=4)

    do i=1,n
       call copy_R8_3d( dst(:,:,:,i), src(:,:,:,i) )
    end do

  end subroutine copy_R8_4d






  !/ =====================================================================================
  pure subroutine copy_C16_1d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(inout) :: dst !! reference to the destination array.
    complex(dp), dimension(:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=1)

    do concurrent (i=1:n)
       dst(i) = src(i)
    end do

  end subroutine copy_C16_1d


  !/ =====================================================================================
  subroutine copy_C16_2d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:), intent(inout) :: dst !! reference to the destination array.
    complex(dp), dimension(:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=2)

    do i=1,n
       call copy_C16_1d( dst(:,i), src(:,i) )
    end do

  end subroutine copy_C16_2d


  !/ =====================================================================================
  subroutine copy_C16_3d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:), intent(inout) :: dst !! reference to the destination array.
    complex(dp), dimension(:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=3)

    do i=1,n
       call copy_C16_2d( dst(:,:,i), src(:,:,i) )
    end do

  end subroutine copy_C16_3d


  !/ =====================================================================================
  subroutine copy_C16_4d( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy each element of the source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:,:,:,:), intent(inout) :: dst !! reference to the destination array.
    complex(dp), dimension(:,:,:,:), intent(in)    :: src !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(src,DIM=4)

    do i=1,n
       call copy_C16_3d( dst(:,:,:,i), src(:,:,:,i) )
    end do

  end subroutine copy_C16_4d


end module copy_mod


!/ =======================================================================================
!/ **                                  C O P Y _ M O D                                  **
!/ =========================================================================== END FILE ==
