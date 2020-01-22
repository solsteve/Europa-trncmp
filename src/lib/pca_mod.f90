!/ ====================================================================== BEGIN FILE =====
!/ **                                   P C A _ M O D                                   **
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
module pca_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-01-21
  !! license: GPL
  !!
  !!## Principle Component Analysis.
  !!
  !! Collection of procedures for using PCA to transform high dimensional data.
  !
  !/ -------------------------------------------------------------------------------------
  use tc_lapack
  use matrix_mod
  use tlogger
  implicit none


  !/ =====================================================================================
  type :: PCA
     !/ -----------------------------------------------------------------------------------

     real(dp), allocatable :: U(:,:)  !! Left Singular Vectors.
     real(dp), allocatable :: S(:,:)  !! Singular Values.
     real(dp), allocatable :: VT(:,:) !! Right Singular Vectors.

     
   contains

     
  end type PCA


  interface MeanShift
     module procedure :: mean_shift_vec_R8
     module procedure :: mean_shift_vec_R8_inplace
     module procedure :: mean_shift_mat_R8
     module procedure :: mean_shift_mat_R8_inplace
  end interface MeanShift



  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =======================================================================================
  subroutine mean_shift_vec_R8( MS, V, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Vector.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: MS(:)  !! Mean Shifted Vector.
    real(dp),           intent(in)    :: V(:)   !! Source Vector.
    real(dp), optional, intent(out)   :: MEAN   !! return the mean value.
    !/ -------------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: mu
    !/ -------------------------------------------------------------------------------------
    mu = D_ZERO
    n = size(V)
    if ( size(MS).lt.n ) then
       call log_error( 'Mean Shift Vector: return array is smaller than', I4=n )
       goto 999
    end if

    do i=1,n
       mu = mu + V(i)
    end do

    mu = mu / real(n,dp)

    do i=1,n
       MS(i) = V(i) - mu
    end do

999 continue

    if ( present( MEAN ) ) MEAN = mu

  end subroutine mean_shift_vec_R8


  !/ =======================================================================================
  subroutine mean_shift_vec_R8_inplace( V, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Vector inplace.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: V(:)   !! Source Vector.
    real(dp), optional, intent(out)   :: MEAN   !! return the mean value.
    !/ -------------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: mu
    !/ -------------------------------------------------------------------------------------
    mu = D_ZERO
    n  = size(V)

    do i=1,n
       mu = mu + V(i)
    end do

    mu = mu / real(n,dp)

    do i=1,n
       V(i) = V(i) - mu
    end do

    if ( present( MEAN ) ) MEAN = mu

  end subroutine mean_shift_vec_R8_inplace


  !/ =======================================================================================
  subroutine mean_shift_col_R8( MS, M, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Column Matrix. A row of means will be used. One mean for each column.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: MS(:,:)  !! Mean Shifted Matrix.
    real(dp),           intent(in)    :: M(:,:)   !! Source Matrix.
    real(dp), optional, intent(out)   :: MEAN(:)  !! return the mean values.
    !/ -------------------------------------------------------------------------------------
    integer  :: i, j, nr, nc
    real(dp), allocatable :: mu(:)
    real(dp) :: s, f
    !/ -------------------------------------------------------------------------------------
    nr = size(M, DIM=1)
    nc = size(M, DIM=2)

    allocate( mu(nc) )
    do i=1,nc
       mu(i) = D_ZERO
    end do

    if ( size(MS,DIM=1).lt.nr ) then
       call log_error( 'Mean Shift Matrix: return number of rows smaller than', I4=nr )
       goto 999
    end if

    if ( size(MS,DIM=2).lt.nc ) then
       call log_error( 'Mean Shift Matrix: return number of columns smaller than', I4=nc )
       goto 999
    end if

    f = real(nr,dp)
    do j=1,nc
       s = D_ZERO
       do i=1,nr
          s = s + M(i,j)
       end do
       mu(j) = s / f
    end do

    do j=1,nc
       do i=1,nr
          MS(i,j) = M(i,j) - mu(j)
       end do
    end do

999 continue

    if ( present( MEAN ) ) then
       do i=1,nc
          MEAN(i) = mu(i)
       end do
    end if

    deallocate( mu )

  end subroutine mean_shift_col_R8


  !/ =======================================================================================
  subroutine mean_shift_col_R8_inplace( M, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Column Matrix inplace.
    !! A row of means will be used. One mean for each column.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: M(:,:)   !! Source Matrix.
    real(dp), optional, intent(out)   :: MEAN(:)  !! return the mean values.
    !/ -------------------------------------------------------------------------------------
    integer  :: i, j, nr, nc
    real(dp), allocatable :: mu(:)
    real(dp) :: s, f
    !/ -------------------------------------------------------------------------------------
    nr = size(M, DIM=1)
    nc = size(M, DIM=2)

    allocate( mu(nc) )
    do i=1,nc
       mu(i) = D_ZERO
    end do

    f = real(nr,dp)
    do j=1,nc
       s = D_ZERO
       do i=1,nr
          s = s + M(i,j)
       end do
       mu(j) = s / f
    end do

    do j=1,nc
       do i=1,nr
          M(i,j) = M(i,j) - mu(j)
       end do
    end do

    if ( present( MEAN ) ) then
       do i=1,nc
          MEAN(i) = mu(i)
       end do
    end if

    deallocate( mu )

  end subroutine mean_shift_col_R8_inplace


  !/ =======================================================================================
  subroutine mean_shift_row_R8( MS, M, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Column Matrix inplace.
    !! A column of means will be used. One mean for each row.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: MS(:,:)  !! Mean Shifted Matrix.
    real(dp),           intent(in)    :: M(:,:)   !! Source Matrix.
    real(dp), optional, intent(out)   :: MEAN(:)  !! return the mean values.
    !/ -------------------------------------------------------------------------------------
    integer               :: i, j, nr, nc
    real(dp), allocatable :: mu(:)
    real(dp)              :: s, f
    !/ -------------------------------------------------------------------------------------
    nr = size(M, DIM=1)
    nc = size(M, DIM=2)

    allocate( mu(nr) )
    do i=1,nr
       mu(i) = D_ZERO
    end do

    if ( size(MS,DIM=1).lt.nr ) then
       call log_error( 'Mean Shift Matrix: return number of rows smaller than', I4=nr )
       goto 999
    end if

    if ( size(MS,DIM=2).lt.nc ) then
       call log_error( 'Mean Shift Matrix: return number of columns smaller than', I4=nc )
       goto 999
    end if

    f = real(nc,dp)
    do i=1,nr
       s = D_ZERO
       do j=1,nc
          s = s + M(i,j)
       end do
       mu(i) = s / f
    end do

    do i=1,nr
       do j=1,nc
          MS(i,j) = M(i,j) - mu(i)
       end do
    end do

999 continue

    if ( present( MEAN ) ) then
       do i=1,nr
          MEAN(i) = mu(i)
       end do
    end if

    deallocate( mu )

  end subroutine mean_shift_row_R8


  !/ =======================================================================================
  subroutine mean_shift_row_R8_inplace( M, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Row Matrix inplace.
    !! A column of means will be used. One mean for each row.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: M(:,:)   !! Source Matrix.
    real(dp), optional, intent(out)   :: MEAN(:)  !! return the mean values.
    !/ -------------------------------------------------------------------------------------
    integer               :: i, j, nr, nc
    real(dp), allocatable :: mu(:)
    real(dp)              :: s, f
    !/ -------------------------------------------------------------------------------------
    nr = size(M, DIM=1)
    nc = size(M, DIM=2)

    allocate( mu(nr) )
    do i=1,nr
       mu(i) = D_ZERO
    end do

    f = real(nc,dp)
    do i=1,nr
       s = D_ZERO
       do j=1,nc
          s = s + M(i,j)
       end do
       mu(i) = s / f
    end do

    do i=1,nr
       do j=1,nc
          M(i,j) = M(i,j) - mu(i)
       end do
    end do

999 continue

    if ( present( MEAN ) ) then
       do i=1,nr
          MEAN(i) = mu(i)
       end do
    end if

    deallocate( mu )

  end subroutine mean_shift_row_R8_inplace


  !/ =======================================================================================
  subroutine mean_shift_mat_R8( MS, M, AXIS, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Matrix.
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: MS(:,:)  !! Mean Shifted Matrix.
    real(dp),           intent(in)    :: M(:,:)   !! Source Matrix.
    integer,  optional, intent(in)    :: AXIS     !! Axis.
    real(dp), optional, intent(out)   :: MEAN(:)  !! return the mean values.
    !/ -------------------------------------------------------------------------------------
    integer :: ax
    !/ -------------------------------------------------------------------------------------
    if ( size(M, DIM=1).gt.size(M, DIM=2) ) then
       ax = 1
    else
       ax = 2
    end if

    if ( present( AXIS ) ) ax = AXIS

    if ( 1.eq.ax ) then
       call mean_shift_col_R8( MS, M, MEAN )
    else
       call mean_shift_row_R8( MS, M, MEAN )
    end if

  end subroutine mean_shift_mat_R8


  !/ =======================================================================================
  subroutine mean_shift_mat_R8_inplace( M, AXIS, MEAN )
    !/ -------------------------------------------------------------------------------------
    !! Mean shift a Matrix inplace.
    !/ -------------------------------------------------------------------------------------
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: M(:,:)   !! Source Matrix.
    integer,  optional, intent(in)    :: AXIS     !! Axis.
    real(dp), optional, intent(out)   :: MEAN(:)  !! return the mean values.
    !/ -------------------------------------------------------------------------------------
    integer :: ax
    !/ -------------------------------------------------------------------------------------
    if ( size(M, DIM=1).gt.size(M, DIM=2) ) then
       ax = 1
    else
       ax = 2
    end if

    if ( present( AXIS ) ) ax = AXIS

    if ( 1.eq.ax ) then
       call mean_shift_col_R8_inplace( M, MEAN )
    else
       call mean_shift_row_R8_inplace( M, MEAN )
    end if

  end subroutine mean_shift_mat_R8_inplace


end module pca_mod


!/ =======================================================================================
!/ **                                   P C A _ M O D                                   **
!/ =========================================================================== END FILE ==
