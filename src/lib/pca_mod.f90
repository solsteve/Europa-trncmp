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
     !/ ----------------------------------------------------------------------------------

     integer               :: max_var  = 0  !! Number of data dimensions.
     integer               :: n_sample = 0  !! Number of samples.
     real(dp), allocatable :: mu(:)         !! Sample means.
     real(dp), allocatable :: U(:,:)        !! Left Singular Vectors.
     real(dp), allocatable :: S(:)          !! Singular Values.
     real(dp), allocatable :: VT(:,:)       !! Right Singular Vectors transposed.
     real(dp), allocatable :: V(:,:)        !! Right Singular Vectors.

   contains

     procedure :: compile => pca_compile_table
     procedure :: delete  => pca_delete

     procedure, private :: pca_vector_transform
     procedure, private :: pca_table_transform
     procedure, private :: pca_vector_recover
     procedure, private :: pca_table_recover

     generic :: transform => pca_vector_transform, pca_table_transform
     generic :: recover   => pca_vector_recover, pca_table_recover

     procedure :: eigenvalue => pca_eigenvalue_index

     final :: pca_destroy

  end type PCA




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function pca_eigenvalue_index( dts, idx ) result( w )
    !/ -----------------------------------------------------------------------------------
    !! Compute an eigenvalue from a singular value $$W_i = \frac{1}{n-1} S_i^2$$.
    !! Where $W$ are the eigenvalues, $S$ are the singular values, and $n$ is the
    !! number of samples used to compute the singular value decomposition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PCA), intent(inout) :: dts !! reference to this PCA
    integer, intent(in) :: idx
    real(dp) :: w
    !/ -----------------------------------------------------------------------------------
    w = dts%S(idx)*dts%S(idx)/real(dts%n_sample-1,dp)
  end function pca_eigenvalue_index






  !/ =====================================================================================
  subroutine pca_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Deconstructor
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PCA), intent(inout) :: dts !! reference to a PCA
    !/ -----------------------------------------------------------------------------------
    call dts%delete
  end subroutine pca_destroy


  !/ =====================================================================================
  subroutine pca_delete( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destroy the allocation for this PCA object.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PCA), intent(inout) :: dts !! reference to this PCA
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%mu ) ) deallocate( dts%mu )
    if ( allocated( dts%U  ) ) deallocate( dts%U  )
    if ( allocated( dts%S  ) ) deallocate( dts%S  )
    if ( allocated( dts%V  ) ) deallocate( dts%V  )
    if ( allocated( dts%VT ) ) deallocate( dts%VT )
    dts%max_var  = 0
    dts%n_sample = 0
  end subroutine pca_delete


  !/ =====================================================================================
  subroutine pca_compile_table( dts, table, MEAN_CENTERED )
    !/ -----------------------------------------------------------------------------------
    !! Compile the internal matrices for PCA.
    !! After a call to this procedure mu will contain that sample means of the table
    !! V & VT with contain the forward and inverse eigenvectors used to
    !! Transform data. S will contain the singular values, and U will contain the
    !! unit covariance transformation of the sampled data.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PCA),        intent(inout) :: dts           !! reference to this PCA object.
    real(dp),          intent(in)    :: table(:,:)    !! input data
    logical, optional, intent(in)    :: MEAN_CENTERED !! table is mean centered (default: FALSE)
    !/ -----------------------------------------------------------------------------------
    integer :: ierr, i, j
    logical :: centered
    !/ -----------------------------------------------------------------------------------
    centered = .false.
    if ( present( MEAN_CENTERED ) ) centered = MEAN_CENTERED

    call dts%delete

    dts%n_sample = size( table, DIM=1 )
    dts%max_var  = size( table, DIM=2 )

    allocate( dts%mu( dts%max_var ) )
    allocate( dts%U( dts%n_sample, dts%max_var ) )
    allocate( dts%S( dts%max_var ) )
    allocate( dts%VT( dts%max_var, dts%max_var ) )
    allocate( dts%V( dts%max_var, dts%max_var ) )

    if ( .not.centered ) then
       !print *, 'Data requires mean shifting'
       call MeanShift( dts%U, table, AXIS=1, MEAN=dts%mu )
    else
       !print *, 'Data has already been mean shifted'
       do concurrent( j=1:dts%max_var, i=1:dts%n_sample )
          dts%U(i,j) = table(i,j)
       end do
    end if

    call tc_dgesdd( dts%U, dts%S, VT=dts%VT, JOB='U', INFO=ierr )

    !/ ----- transpose VT --------------------------------------
    do concurrent( i=1:dts%max_var, j=1:dts%max_var )
       dts%V(j,i) = dts%VT(i,j)
    end do

  end subroutine pca_compile_table




  !/ =====================================================================================
  subroutine pca_vector_transform( dts, vecout, vecin, LEVEL )
    !/ -----------------------------------------------------------------------------------
    !! Perform a forward PCA transform on a single vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PCA),        intent(inout) :: dts       !! reference to this PCA object.
    real(dp),          intent(inout) :: vecout(:) !! ouput vector.
    real(dp),          intent(in)    :: vecin(:)  !! input vector.
    integer, optional, intent(in)    :: LEVEL     !! number of principle componets
    !!                                               None or Zero == All
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, m, n
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    m = dts%max_var
    n = m
    if ( present( LEVEL ) ) then
       if ( 0.lt.LEVEL ) then
          if ( LEVEL.le.m ) then
             m = LEVEL
          end if
       end if
    end if

    do j=1,m
       x = D_ZERO
       do i=1,n
          x = x + vecin(i) * dts%V(i,j)
       end do
       vecout(j) = x
    end do

  end subroutine pca_vector_transform


  !/ =====================================================================================
  subroutine pca_table_transform( dts, matout, matin, LEVEL )
    !/ -----------------------------------------------------------------------------------
    !! Perform a forward PCA transform on a matrix.
    !/ -----------------------------------------------------------------------------------
    implicit none 
    class(PCA),        intent(inout) :: dts         !! reference to this PCA object.
    real(dp),          intent(inout) :: matout(:,:) !! ouput matrix.
    real(dp),          intent(in)    :: matin(:,:)  !! input matrix.
    integer, optional, intent(in)    :: LEVEL       !! number of principle componets
    !!                                                 None or Zero == All
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, k, m, n, s
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    s = dts%n_sample
    m = dts%max_var
    n = m
    if ( present( LEVEL ) ) then
       if ( 0.lt.LEVEL ) then
          if ( LEVEL.le.m ) then
             m = LEVEL
          end if
       end if
    end if

    do i=1,s
       do j=1,m
          x = D_ZERO
          do k=1,n
             x = x + matin(i,k) * dts%V(k,j)
          end do
          matout(i,j) = x
       end do
    end do

  end subroutine pca_table_transform


  !/ =====================================================================================
  subroutine pca_vector_recover( dts, vecout, vecin, LEVEL )
    !/ -----------------------------------------------------------------------------------
    !! Perform a reverse PCA transform on a single vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PCA),        intent(inout) :: dts     !! reference to this PCA object.
    real(dp),          intent(inout) :: vecout(:) !! output vector.
    real(dp),          intent(in)    :: vecin(:)  !! input vector.
    integer, optional, intent(in)    :: LEVEL   !! number of principle componets
    !!                                             None or Zero == All
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, m, n
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    m = dts%max_var
    n = m
    if ( present( LEVEL ) ) then
       if ( 0.lt.LEVEL ) then
          if ( LEVEL.le.m ) then
             m = LEVEL
          end if
       end if
    end if

    do j=1,m
       x = D_ZERO
       do i=1,n
          x = x + vecin(i) * dts%VT(i,j)
       end do
       vecout(j) = x
    end do

  end subroutine pca_vector_recover


  !/ =====================================================================================
  subroutine pca_table_recover( dts, matout, matin, LEVEL )
    !/ -----------------------------------------------------------------------------------
    !! Perform a reverse PCA transform on a matrix.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PCA),        intent(inout) :: dts         !! reference to this PCA object.
    real(dp),          intent(inout) :: matout(:,:) !! output matrix.
    real(dp),          intent(in)    :: matin(:,:)  !! input matrix.
    integer, optional, intent(in)    :: LEVEL       !! number of principle componets
    !!                                                 None or Zero == All
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, k, m, n, s
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    s = dts%n_sample
    m = dts%max_var
    n = m
    if ( present( LEVEL ) ) then
       if ( 0.lt.LEVEL ) then
          if ( LEVEL.le.m ) then
             m = LEVEL
          end if
       end if
    end if

    do i=1,s
       do j=1,m
          x = D_ZERO
          do k=1,n
             x = x + matin(i,k) * dts%VT(k,j)
          end do
          matout(i,j) = x
       end do
    end do

  end subroutine pca_table_recover


end module pca_mod


!/ =======================================================================================
!/ **                                   P C A _ M O D                                   **
!/ =========================================================================== END FILE ==
