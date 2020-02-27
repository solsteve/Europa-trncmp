!/ ====================================================================== BEGIN FILE =====
!/ **                              V E C T O R 3 D _ M O D                              **
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
module matrix3d_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-02-03
  !! license: GPL
  !!
  !!## 3D Matrix Routines
  !!
  !! Collection of procedures for manipulating 3D matrices.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger


  !/ -------------------------------------------------------------------------------------
  type Matrix3D
     !/ ----------------------------------------------------------------------------------
     real(dp) :: m(3,3)
   contains
     procedure :: zero   => m3_zero_all
     procedure :: ident  => m3_ident
     procedure :: copy   => m3_copy_internal
     procedure :: equals => m3_compare

     procedure :: numpy  => m3_to_numpy
     procedure :: matlab => m3_to_matlab
     procedure :: sage   => m3_to_sage

     procedure :: det    => m3_determinant

     procedure :: diagonal    => m3_diagonal
     

     procedure, private :: m3_set_all
     procedure, private :: m3_set_array
     procedure, private :: m3_set_mem

     procedure, private :: m3_add_m_internal
     procedure, private :: m3_add_s_internal
     procedure, private :: m3_add_mm_internal
     procedure, private :: m3_add_ms_internal
     procedure, private :: m3_add_sm_internal

     procedure, private :: m3_sub_m_internal
     procedure, private :: m3_sub_s_internal
     procedure, private :: m3_sub_mm_internal
     procedure, private :: m3_sub_ms_internal
     procedure, private :: m3_sub_sm_internal

     procedure, private :: m3_mul_m_internal
     procedure, private :: m3_mul_s_internal
     procedure, private :: m3_mul_mm_internal
     procedure, private :: m3_mul_ms_internal
     procedure, private :: m3_mul_sm_internal

     procedure, private :: m3_div_m_internal
     procedure, private :: m3_div_s_internal
     procedure, private :: m3_div_mm_internal
     procedure, private :: m3_div_ms_internal
     procedure, private :: m3_div_sm_internal

     procedure, private :: m3_dot_self
     procedure, private :: m3_dot_pair

     procedure, private :: m3_transpose
     procedure, private :: m3_transpose_inplace
     
     procedure, private :: m3_inverse
     procedure, private :: m3_inverse_inplace

     generic :: set => m3_set_all, m3_set_array, m3_set_mem

     generic :: add => m3_add_m_internal, m3_add_s_internal,  &
          &            m3_add_mm_internal, m3_add_ms_internal, m3_add_sm_internal

     generic :: sub => m3_sub_m_internal, m3_sub_s_internal,  &
          &            m3_sub_mm_internal, m3_sub_ms_internal, m3_sub_sm_internal

     generic :: mul => m3_mul_m_internal, m3_mul_s_internal,  &
          &            m3_mul_mm_internal, m3_mul_ms_internal, m3_mul_sm_internal

     generic :: div => m3_div_m_internal, m3_div_s_internal,  &
          &            m3_div_mm_internal, m3_div_ms_internal, m3_div_sm_internal

     generic :: dot => m3_dot_self, m3_dot_pair

     generic :: transpose => m3_transpose, m3_transpose_inplace
     generic :: inverse   => m3_inverse,   m3_inverse_inplace

  end type Matrix3D


  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: m3_size_external
  end interface size

  !/ -------------------------------------------------------------------------------------
  interface zero
     !/ ----------------------------------------------------------------------------------
     module procedure :: m3_zero_external
  end interface zero


  !/ -------------------------------------------------------------------------------------
  interface copy
     !/ ----------------------------------------------------------------------------------
     module procedure :: m3_copy_mm_external
     module procedure :: m3_copy_ma_external
     module procedure :: m3_copy_am_external
  end interface copy

















  !/ -------------------------------------------------------------------------------------
  interface toString
     !/ ----------------------------------------------------------------------------------
     module procedure :: m3_to_string
  end interface toString



  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine m3_zero_all( dts )
    !/ -----------------------------------------------------------------------------------
    !! Zero the elements of this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = D_ZERO
    end do
  end subroutine m3_zero_all


  !/ =====================================================================================
  subroutine m3_ident( dts )
    !/ -----------------------------------------------------------------------------------
    !! Zero the elements of this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    dts%m(1,1) = D_ONE
    dts%m(2,1) = D_ZERO
    dts%m(3,1) = D_ZERO
    dts%m(1,2) = D_ZERO
    dts%m(2,2) = D_ONE
    dts%m(3,2) = D_ZERO
    dts%m(1,3) = D_ZERO
    dts%m(2,3) = D_ZERO
    dts%m(3,3) = D_ONE
  end subroutine m3_ident


  !/ =====================================================================================
  subroutine m3_copy_internal( dts, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy the elements of a source Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D),  intent(in)    :: src !! reference to a source Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = src%m(i,j)
    end do
  end subroutine m3_copy_internal


  !/ =====================================================================================
  function m3_compare( dts, M ) result( cmp )
    !/ -----------------------------------------------------------------------------------
    !! Perform a element by element comparison of two matrices.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(in) :: dts !! reference to this Matrix3D.
    type(Matrix3D),  intent(in) :: M   !! reference to another Matrix3D.
    logical                     :: cmp
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    cmp = .true.
    do j=1,3
       do i=1,3
          if ( dts%m(i,j).lt.M%m(i,j) ) then
             cmp = .false.
             goto 999
          end if
          if ( dts%m(i,j).gt.M%m(i,j) ) then
             cmp = .false.
             goto 999
          end if
       end do
    end do
999 continue
  end function m3_compare


  !/ =====================================================================================
  function m3_to_numpy( dts, FMT, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Format this Matrix3D to be read as a numpy array
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D),        intent(in)  :: dts   !! reference to a Matrix3D.
    character(*), optional, intent(in)  :: FMT   !! edit descriptor (defualt: G0).
    logical, optional,      intent(in)  :: TRANS !! transpose       (defualt: .false.).
    character(len=:),       allocatable :: str   !! string formated as a matrix
    !/ -----------------------------------------------------------------------------------
    str = 'np.array( ' // toString( dts, FMT=FMT, RDEL='],[', TRANS=TRANS ) // ' )'
  end function m3_to_numpy


  !/ =====================================================================================
  function m3_to_matlab( dts, FMT, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Format this Matrix3D to be read as a numpy array
    !/ -----------------------------------------------------------------------------------
    use string_tools, only : toString
    implicit none
    class(Matrix3D),        intent(in)  :: dts   !! reference to a Matrix3D.
    character(*), optional, intent(in)  :: FMT   !! edit descriptor (defualt: G0).
    logical, optional,      intent(in)  :: TRANS !! transpose       (defualt: .false.).
    character(len=:),       allocatable :: str   !! string formated as a matrix
    !/ -----------------------------------------------------------------------------------
    str = '[' // toString( dts%m, FMT=FMT, CDEL=' ', RDEL=' ; ', TRANS=TRANS ) // ']'
  end function m3_to_matlab


  !/ =====================================================================================
  function m3_to_sage( dts, FMT, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Format this Matrix3D to be read as a numpy array
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D),        intent(in)  :: dts   !! reference to a Matrix3D.
    character(*), optional, intent(in)  :: FMT   !! edit descriptor (defualt: G0).
    logical, optional,      intent(in)  :: TRANS !! transpose       (defualt: .false.).
    character(len=:),       allocatable :: str   !! string formated as a matrix
    !/ -----------------------------------------------------------------------------------
    str = 'Matrix(SR, ' // toString( dts, FMT=FMT, RDEL='],[', TRANS=TRANS ) // ' )'
  end function m3_to_sage






















  !/ =====================================================================================
  subroutine m3_set_all( dts, val )
    !/ -----------------------------------------------------------------------------------
    !! Set all the elements of this Matrix3D to val.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),        intent(in)    :: val !! value to set all
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = val
    end do
  end subroutine m3_set_all


  !/ =====================================================================================
  subroutine m3_set_array( dts, ary )
    !/ -----------------------------------------------------------------------------------
    !! Set all the elements of this Matrix3D from a 3x3 array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts      !! reference to this Matrix3D.
    real(dp),        intent(in)    :: ary(:,:) !! array to set from
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = ary(i,j)
    end do
  end subroutine m3_set_array


  !/ =====================================================================================
  subroutine m3_set_mem( dts, ary, ORDER )
    !/ -----------------------------------------------------------------------------------
    !! Set all the elements of this Matrix3D from a 9 element array.
    !! Order: 'C' is fortran fill down each column.
    !!        'R' is C/C++   fill across each row.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D),       intent(inout) :: dts    !! reference to this Matrix3D.
    real(dp),              intent(in)    :: ary(:) !! array to set from
    character(1), optional, intent(in)    :: ORDER  !! order: 'C' or 'R'
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, otype
    !/ -----------------------------------------------------------------------------------
    otype = 0
    if ( present( ORDER ) ) then
       if ( 'R'.eq.ORDER ) otype = 1
       if ( 'r'.eq.ORDER ) otype = 1
    end if

    if ( 0.eq.otype ) then ! Fill Fortran style
       do concurrent( i=1:3, j=1:3 )
          dts%m(i,j) = ary(3*j + i - 3)
       end do
    else
       do concurrent( i=1:3, j=1:3 )
          dts%m(i,j) = ary(3*i + j - 3)
       end do
    end if
  end subroutine m3_set_mem









  !/ =====================================================================================
  subroutine m3_add_m_internal( dts, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Add each element of a right Matrix3D to the coresponding element of this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts  !! reference to this Matrix3D.
    class(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) + rmat%m(i,j)
    end do
  end subroutine m3_add_m_internal


  !/ =====================================================================================
  subroutine m3_add_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Add a right scalar to each element of this Martix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),        intent(in)    :: rs  !! right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) + rs
    end do
  end subroutine m3_add_s_internal


  !/ =====================================================================================
  subroutine m3_add_mm_internal( dts, lmat, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Add each element of a left Matrix3D to the coresponding element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    type(Matrix3D), intent(in)    :: rmat !! reference to a left Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) + rmat%m(i,j)
    end do
  end subroutine m3_add_mm_internal


  !/ =====================================================================================
  subroutine m3_add_sm_internal( dts, lscl, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Add a left scalar to each element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),       intent(in)    :: lscl !! reference to a left scalar.
    type(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lscl + rmat%m(i,j)
    end do
  end subroutine m3_add_sm_internal


  !/ =====================================================================================
  subroutine m3_add_ms_internal( dts, lmat, rscl )
    !/ -----------------------------------------------------------------------------------
    !! Add each element of a left Matrix3D to a right scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    real(dp),       intent(in)    :: rscl !! reference to a right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) + rscl
    end do
  end subroutine m3_add_ms_internal











  !/ =====================================================================================
  subroutine m3_sub_m_internal( dts, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Subtract each element of a right Matrix3D from the coresponding element of this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts  !! reference to this Matrix3D.
    class(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) - rmat%m(i,j)
    end do
  end subroutine m3_sub_m_internal


  !/ =====================================================================================
  subroutine m3_sub_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Subtract a right scalar from each element of this Martix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),        intent(in)    :: rs  !! right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) - rs
    end do
  end subroutine m3_sub_s_internal


  !/ =====================================================================================
  subroutine m3_sub_mm_internal( dts, lmat, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Subtract each element of a right Matrix3D from the coresponding element of a left Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    type(Matrix3D), intent(in)    :: rmat !! reference to a left Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) - rmat%m(i,j)
    end do
  end subroutine m3_sub_mm_internal


  !/ =====================================================================================
  subroutine m3_sub_sm_internal( dts, lscl, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Subtract each element of a right Matrix3D from a left scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),       intent(in)    :: lscl !! reference to a left scalar.
    type(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lscl - rmat%m(i,j)
    end do
  end subroutine m3_sub_sm_internal


  !/ =====================================================================================
  subroutine m3_sub_ms_internal( dts, lmat, rscl )
    !/ -----------------------------------------------------------------------------------
    !! Subtract a right scalar from each element of a left Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    real(dp),       intent(in)    :: rscl !! reference to a right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) - rscl
    end do
  end subroutine m3_sub_ms_internal











  !/ =====================================================================================
  subroutine m3_mul_m_internal( dts, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of this Matrix3D by a corresponding element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts  !! reference to this Matrix3D.
    class(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) * rmat%m(i,j)
    end do
  end subroutine m3_mul_m_internal


  !/ =====================================================================================
  subroutine m3_mul_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of this Matrix3D by a right scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),        intent(in)    :: rs  !! right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) * rs
    end do
  end subroutine m3_mul_s_internal


  !/ =====================================================================================
  subroutine m3_mul_mm_internal( dts, lmat, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of a left Matrix3D with the corresponding element a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    type(Matrix3D), intent(in)    :: rmat !! reference to a left Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) * rmat%m(i,j)
    end do
  end subroutine m3_mul_mm_internal


  !/ =====================================================================================
  subroutine m3_mul_sm_internal( dts, lscl, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Multiply a scalar with each element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),       intent(in)    :: lscl !! reference to a left scalar.
    type(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lscl * rmat%m(i,j)
    end do
  end subroutine m3_mul_sm_internal


  !/ =====================================================================================
  subroutine m3_mul_ms_internal( dts, lmat, rscl )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of a left Matrix3D with a right scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    real(dp),       intent(in)    :: rscl !! reference to a right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) * rscl
    end do
  end subroutine m3_mul_ms_internal








  !/ =====================================================================================
  subroutine m3_div_m_internal( dts, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of this Matrix3D by a corresponding element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts  !! reference to this Matrix3D.
    class(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) / rmat%m(i,j)
    end do
  end subroutine m3_div_m_internal


  !/ =====================================================================================
  subroutine m3_div_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of this Matrix3D by a right scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),        intent(in)    :: rs  !! right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = dts%m(i,j) / rs
    end do
  end subroutine m3_div_s_internal


  !/ =====================================================================================
  subroutine m3_div_mm_internal( dts, lmat, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of a left Matrix3D with the coresponding element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    type(Matrix3D), intent(in)    :: rmat !! reference to a left Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) / rmat%m(i,j)
    end do
  end subroutine m3_div_mm_internal


  !/ =====================================================================================
  subroutine m3_div_sm_internal( dts, lscl, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Divide a left scalar by each element of a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    real(dp),       intent(in)    :: lscl !! reference to a left scalar.
    type(Matrix3D), intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lscl / rmat%m(i,j)
    end do
  end subroutine m3_div_sm_internal


  !/ =====================================================================================
  subroutine m3_div_ms_internal( dts, lmat, rscl )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of a left Matrix3D by a right scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D), intent(in)    :: lmat !! reference to a left Matrix3D.
    real(dp),       intent(in)    :: rscl !! reference to a right scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = lmat%m(i,j) / rscl
    end do
  end subroutine m3_div_ms_internal











  !/ =====================================================================================
  function m3_size_external( mat, DIM ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Size of this square matrix.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D),    intent(in) :: mat
    integer, optional, intent(in) :: DIM
    integer                       :: n
    !/ -----------------------------------------------------------------------------------
    n = 3

    ! I know this is useless but it prevents warning about unused variables
    if ( present( DIM ) ) then
       if ( 0.eq.DIM ) then
          n = 3
       else
          n = 3
       end if
    end if
  end function m3_size_external


  !/ =====================================================================================
  subroutine m3_zero_external( mat )
    !/ -----------------------------------------------------------------------------------
    !! Zero the elements of this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: mat !! reference to this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       mat%m(i,j) = D_ZERO
    end do
  end subroutine m3_zero_external


  !/ =====================================================================================
  subroutine m3_copy_mm_external( dmat, smat )
    !/ -----------------------------------------------------------------------------------
    !! Copy 
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: dmat !! reference to the destination Matrix3D.
    type(Matrix3D), intent(in)    :: smat !! reference to the source      Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dmat%m(i,j) = smat%m(i,j)
    end do
  end subroutine m3_copy_mm_external


  !/ =====================================================================================
  subroutine m3_copy_ma_external( dmat, sary )
    !/ -----------------------------------------------------------------------------------
    !! Copy 
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: dmat      !! reference to the destination Matrix3D.
    real(dp),       intent(in)    :: sary(:,:) !! reference to the source      array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dmat%m(i,j) = sary(i,j)
    end do
  end subroutine m3_copy_ma_external


  !/ =====================================================================================
  subroutine m3_copy_am_external( dary, smat )
    !/ -----------------------------------------------------------------------------------
    !! Copy 
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),       intent(inout) :: dary(:,:) !! reference to the destination array.
    type(Matrix3D), intent(in)    :: smat      !! reference to the source      Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dary(i,j) = smat%m(i,j)
    end do
  end subroutine m3_copy_am_external


  !/ =====================================================================================
  function m3_to_string( mat, FMT, CDEL, RDEL, PRE, POST, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert a Matrix3D into a formatted string.
    !/ -----------------------------------------------------------------------------------
    use string_tools, only : toString
    implicit none
    type(Matrix3D),         intent(in) :: mat   !! reference to a Matrix3D.
    character(*), optional, intent(in) :: FMT   !! edit descriptor  (defualt: G0).
    character(*), optional, intent(in) :: CDEL  !! column delimeter (defualt: ,).
    character(*), optional, intent(in) :: RDEL  !! row    delimeter (defualt: ;).
    character(*), optional, intent(in) :: PRE   !! prefix           (defualt: [).
    character(*), optional, intent(in) :: POST  !! postfix          (defualt: ]).
    logical,      optional, intent(in) :: TRANS !! transpose        (defualt: .false.).
    character(len=:), allocatable      :: str   !! string formated as a matrix
    !/ -----------------------------------------------------------------------------------
    character(len=:), allocatable :: scdel, srdel, spre, spost
    !/ -----------------------------------------------------------------------------------

    scdel = ','
    srdel = '],' // NEW_LINE('A') // ' ['

    if ( present( CDEL ) ) scdel = CDEL
    if ( present( RDEL ) ) srdel = RDEL

    spre  = '[['
    spost = ']]'

    if ( present( PRE ) )  spre  = PRE
    if ( present( POST ) ) spost = POST

    str = spre // toString( mat%m, FMT=FMT, CDEL=scdel, RDEL=srdel, TRANS=TRANS ) // spost
  end function m3_to_string




  !/ =====================================================================================
  subroutine m3_dot_self( dts, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Inplace matrix multiply.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts  !! reference to this Matrix3D.
    type(Matrix3D),  intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, k
    real(dp) :: lmat(3,3), t
    !/ -----------------------------------------------------------------------------------

    do concurrent( i=1:3, j=1:3 )
       lmat(i,j) = dts%m(i,j)
       dts%m(i,j) = D_ZERO
    end do

    do concurrent( j=1:3, k=1:3 )
       t = rmat%m(k,j)
       do i=1,3
          dts%m(i,j) = dts%m(i,j) + t*lmat(i,k)
       end do
    end do

  end subroutine m3_dot_self


  !/ =====================================================================================
  subroutine m3_dot_pair( dts, lmat, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Matrix multiply.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts  !! reference to this Matrix3D.
    type(Matrix3D),  intent(in)    :: lmat !! reference to a left  Matrix3D.
    type(Matrix3D),  intent(in)    :: rmat !! reference to a right Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, k
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------

    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = D_ZERO
    end do

    do concurrent( j=1:3, k=1:3 )
       t = rmat%m(k,j)
       do i=1,3
          dts%m(i,j) = dts%m(i,j) + t*lmat%m(i,k)
       end do
    end do

  end subroutine m3_dot_pair



  !/ =====================================================================================
  subroutine m3_transpose( dts, mat )
    !/ -----------------------------------------------------------------------------------
    !! Make this Matrix3D the Transpose of the argument Matrix3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D),  intent(in)    :: mat !! reference to a Matrix3D.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       dts%m(i,j) = mat%m(j,i)
    end do
  end subroutine m3_transpose


  !/ =====================================================================================
  subroutine m3_transpose_inplace( dts )
    !/ -----------------------------------------------------------------------------------
    !! Transpose this Matrix3D inplace.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(inout) :: dts !! reference to this Matrix3D.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: t21, t31, t32
    !/ -----------------------------------------------------------------------------------

    t21 = dts%m(2,1)
    t31 = dts%m(3,1)
    t32 = dts%m(3,2)

    dts%m(2,1) = dts%m(1,2)
    dts%m(3,1) = dts%m(1,3)
    dts%m(3,2) = dts%m(2,3)

    dts%m(1,2) = t21
    dts%m(1,3) = t31
    dts%m(2,3) = t32
    
  end subroutine m3_transpose_inplace


  !/ =====================================================================================
  function m3_determinant( dts ) result( d )
    !/ -----------------------------------------------------------------------------------
    !! Determinant
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D), intent(in) :: dts !! reference to this Matrix3D.
    real(dp)                    :: d   !! determinant.
    !/ -----------------------------------------------------------------------------------
    d =      dts%m(1,1)*(dts%m(2,2)*dts%m(3,3) - dts%m(2,3)*dts%m(3,2)) + &
         &   dts%m(1,2)*(dts%m(2,3)*dts%m(3,1) - dts%m(2,1)*dts%m(3,3)) + &
         &   dts%m(1,3)*(dts%m(2,1)*dts%m(3,2) - dts%m(2,2)*dts%m(3,1))
  end function m3_determinant




  !/ =====================================================================================
  subroutine m3_diagonal( dts, GET, SET, ZERO )
    !/ -----------------------------------------------------------------------------------
    !! Get the diagonal
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D),    intent(inout) :: dts    !! reference to this Matrix3D.
    real(dp), optional, intent(out)   :: GET(:) !! diagonal
    real(dp), optional, intent(in)    :: SET(:) !! diagonal
    logical,  optional, intent(in)    :: ZERO   !! flag to zero off diagonal elements
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( present( GET ) ) then
       do concurrent( i=1:3 )
          GET(i) = dts%m(i,i)
       end do
    end if

    if ( present( SET ) ) then
       do concurrent( i=1:3 )
          dts%m(i,i) = SET(i)
       end do
    end if

    if ( present( ZERO ) ) then
       if ( ZERO ) then
          dts%m(2,1) = D_ZERO
          dts%m(3,1) = D_ZERO
          dts%m(1,2) = D_ZERO
          dts%m(3,2) = D_ZERO
          dts%m(1,3) = D_ZERO
          dts%m(2,3) = D_ZERO
       end if
    end if
    
  end subroutine m3_diagonal


  !/ =====================================================================================
  subroutine m3_inverse( dts, mat, DET, IERR )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D),    intent(inout) :: dts !! reference to this Matrix3D.
    type(Matrix3D),     intent(in)    :: mat !! reference to a Matrix3D.
    real(dp), optional, intent(out)   :: DET !! return determinant.
    integer,  optional, intent(out)   :: IERR !! return error 0=successful, 1=singular
    !/ -----------------------------------------------------------------------------------
    integer  :: ret
    real(dp) :: md
    !/ -----------------------------------------------------------------------------------
    ret = 0
    md  = mat%det()
    if (( -D_EPSILON.gt.md ).or.(  D_EPSILON.lt.md )) then

       dts%m(1,1) = ( mat%m(2,2)*mat%m(3,3) - mat%m(2,3)*mat%m(3,2) ) / md
       dts%m(1,2) = ( mat%m(1,3)*mat%m(3,2) - mat%m(1,2)*mat%m(3,3) ) / md
       dts%m(1,3) = ( mat%m(1,2)*mat%m(2,3) - mat%m(1,3)*mat%m(2,2) ) / md

       dts%m(2,1) = ( mat%m(2,3)*mat%m(3,1) - mat%m(2,1)*mat%m(3,3) ) / md
       dts%m(2,2) = ( mat%m(1,1)*mat%m(3,3) - mat%m(1,3)*mat%m(3,1) ) / md
       dts%m(2,3) = ( mat%m(1,3)*mat%m(2,1) - mat%m(1,1)*mat%m(2,3) ) / md

       dts%m(3,1) = ( mat%m(2,1)*mat%m(3,2) - mat%m(2,2)*mat%m(3,1) ) / md
       dts%m(3,2) = ( mat%m(1,2)*mat%m(3,1) - mat%m(1,1)*mat%m(3,2) ) / md
       dts%m(3,3) = ( mat%m(1,1)*mat%m(2,2) - mat%m(1,2)*mat%m(2,1) ) / md

    else
       md  = D_ZERO
       ret = 1
    end if

    if ( present(DET) )  DET  = md
    if ( present(IERR) ) IERR = ret
  end subroutine m3_inverse


  !/ =====================================================================================
  subroutine m3_inverse_inplace( dts, DET, IERR )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Matrix3D),    intent(inout) :: dts !! reference to this Matrix3D.
    real(dp), optional, intent(out)   :: DET !! return determinant.
    integer,  optional, intent(out)   :: IERR !! return error 0=successful, 1=singular
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp(3,3), md
    integer  :: i, j, ret
    !/ -----------------------------------------------------------------------------------
    ret = 0
    md  = dts%det()
    if (( -D_EPSILON.gt.md ).or.(  D_EPSILON.lt.md )) then

       do concurrent( i=1:3, j=1:3 )
          temp(i,j) = dts%m(i,j )
       end do

       dts%m(1,1) = ( temp(2,2)*temp(3,3) - temp(2,3)*temp(3,2) ) / md
       dts%m(1,2) = ( temp(1,3)*temp(3,2) - temp(1,2)*temp(3,3) ) / md
       dts%m(1,3) = ( temp(1,2)*temp(2,3) - temp(1,3)*temp(2,2) ) / md

       dts%m(2,1) = ( temp(2,3)*temp(3,1) - temp(2,1)*temp(3,3) ) / md
       dts%m(2,2) = ( temp(1,1)*temp(3,3) - temp(1,3)*temp(3,1) ) / md
       dts%m(2,3) = ( temp(1,3)*temp(2,1) - temp(1,1)*temp(2,3) ) / md

       dts%m(3,1) = ( temp(2,1)*temp(3,2) - temp(2,2)*temp(3,1) ) / md
       dts%m(3,2) = ( temp(1,2)*temp(3,1) - temp(1,1)*temp(3,2) ) / md
       dts%m(3,3) = ( temp(1,1)*temp(2,2) - temp(1,2)*temp(2,1) ) / md

    else
       md  = D_ZERO
       ret = 1
    end if

    if ( present(DET) )  DET  = md
    if ( present(IERR) ) IERR = ret
  end subroutine m3_inverse_inplace


end module matrix3d_mod


!/ =======================================================================================
!/ **                              M A T R I X 3 D _ M O D                              **
!/ =========================================================================== END FILE ==
