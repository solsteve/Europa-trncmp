!/ ====================================================================== BEGIN FILE =====
!/ **                                  C O P Y _ M O D                                  **
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
module copy_mod
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2015-10-23
  !! license: GPL
  !!
  !!##Copy Procedures
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
  subroutine copy_char_1d( dst, src )
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
  subroutine copy_I4_1d( dst, src )
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

    !$omp parallel do private(i) shared(dst,src,n)
    do i=1,n
       dst(i) = src(i)
    end do
    !$omp end parallel do

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
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)

    !$omp parallel do private(i1,i2) shared(dst,src,n1,n2)
    do i2=1,n2
       do i1=1,n1
          dst(i1,i2) = src(i1,i2)       
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(dst,src,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             dst(i1,i2,i3) = src(i1,i2,i3)       
          end do
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)
    n4 = size(src,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(dst,src,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)       
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine copy_I4_4d




  !/ =====================================================================================
  subroutine copy_R4_1d( dst, src )
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

    !$omp parallel do private(i) shared(dst,src,n)
    do i=1,n
       dst(i) = src(i)
    end do
    !$omp end parallel do

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
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)

    !$omp parallel do private(i1,i2) shared(dst,src,n1,n2)
    do i2=1,n2
       do i1=1,n1
          dst(i1,i2) = src(i1,i2)       
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(dst,src,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             dst(i1,i2,i3) = src(i1,i2,i3)       
          end do
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)
    n4 = size(src,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(dst,src,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)       
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine copy_R4_4d




  !/ =====================================================================================
  subroutine copy_R8_1d( dst, src )
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

    !$omp parallel do private(i) shared(dst,src,n)
    do i=1,n
       dst(i) = src(i)
    end do
    !$omp end parallel do

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
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)

    !$omp parallel do private(i1,i2) shared(dst,src,n1,n2)
    do i2=1,n2
       do i1=1,n1
          dst(i1,i2) = src(i1,i2)       
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(dst,src,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             dst(i1,i2,i3) = src(i1,i2,i3)       
          end do
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)
    n4 = size(src,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(dst,src,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)       
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine copy_R8_4d




  !/ =====================================================================================
  subroutine copy_C16_1d( dst, src )
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

    !$omp parallel do private(i) shared(dst,src,n)
    do i=1,n
       dst(i) = src(i)
    end do
    !$omp end parallel do

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
    integer :: i1, i2, n1, n2
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)

    !$omp parallel do private(i1,i2) shared(dst,src,n1,n2)
    do i2=1,n2
       do i1=1,n1
          dst(i1,i2) = src(i1,i2)       
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, n1, n2, n3
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)

    !$omp parallel do private(i1,i2,i3) shared(dst,src,n1,n2,n3)
    do i3=1,n3
       do i2=1,n2
          do i1=1,n1
             dst(i1,i2,i3) = src(i1,i2,i3)       
          end do
       end do
    end do
    !$omp end parallel do

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
    integer :: i1, i2, i3, i4, n1, n2, n3, n4
    !/ -----------------------------------------------------------------------------------

    n1 = size(src,DIM=1)
    n2 = size(src,DIM=2)
    n3 = size(src,DIM=3)
    n4 = size(src,DIM=4)

    !$omp parallel do private(i1,i2,i3,i4) shared(dst,src,n1,n2,n3,n4)
    do i4=1,n4
       do i3=1,n3
          do i2=1,n2
             do i1=1,n1
                dst(i1,i2,i3,i4) = src(i1,i2,i3,i4)       
             end do
          end do
       end do
    end do
    !$omp end parallel do

  end subroutine copy_C16_4d


end module copy_mod


!/ =======================================================================================
!/ **                                  C O P Y _ M O D                                  **
!/ =========================================================================== END FILE ==
