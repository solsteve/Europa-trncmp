!/ ====================================================================== BEGIN FILE =====
!/ **                                M A T R I X _ M O D                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module matrix_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-11-22
  !! license: GPL
  !!
  !!## Matrix procedures.
  !!
  !! Collection of procedures for manipulating matrices.
  !
  !/ -------------------------------------------------------------------------------------
  use vector_mod
  use tlogger
  use tc_lapack
  implicit none

  integer, parameter :: MIN_M_PAR = 64 !! minimum matrix dimension for parallization with openmp


  !/ -------------------------------------------------------------------------------------
  interface add
     !/ ----------------------------------------------------------------------------------
     module procedure :: add_ms_R8
     module procedure :: add_sm_R8
     module procedure :: add_ms_R8_inplace
     module procedure :: add_mm_R8_inplace
     module procedure :: add_mm_R8
  end interface add


  !/ -------------------------------------------------------------------------------------
  interface sub
     !/ ----------------------------------------------------------------------------------
     module procedure :: sub_ms_R8
     module procedure :: sub_sm_R8
     module procedure :: sub_ms_R8_inplace
     module procedure :: sub_mm_R8_inplace
     module procedure :: sub_mm_R8
  end interface sub


  !/ -------------------------------------------------------------------------------------
  interface mul
     !/ ----------------------------------------------------------------------------------
     module procedure :: mul_ms_R8
     module procedure :: mul_sm_R8
     module procedure :: mul_ms_R8_inplace
     module procedure :: mul_mm_R8_inplace
     module procedure :: mul_mm_R8
  end interface mul


  !/ -------------------------------------------------------------------------------------
  interface DiagMul
     !/ ----------------------------------------------------------------------------------
     module procedure :: mul_dm_R8
  end interface DiagMul


  !/ -------------------------------------------------------------------------------------
  interface div
     !/ ----------------------------------------------------------------------------------
     module procedure :: div_ms_R8
     module procedure :: div_sm_R8
     module procedure :: div_ms_R8_inplace
     module procedure :: div_mm_R8_inplace
     module procedure :: div_mm_R8
  end interface div


  !/ -------------------------------------------------------------------------------------
  interface dot
     !/ ----------------------------------------------------------------------------------
     module procedure :: dot_mm_R8
     module procedure :: dot_mv_R8
     module procedure :: dot_vm_R8
  end interface dot


  !/ -------------------------------------------------------------------------------------
  interface swap
     !/ ----------------------------------------------------------------------------------
     module procedure :: swap_mm_R8
  end interface swap


  !/ -------------------------------------------------------------------------------------
  interface diagonal
     !/ -----------------------------------------------------------------------------------
     module procedure :: diagonal_R8
  end interface diagonal


  !/ -------------------------------------------------------------------------------------
  interface identity
     !/ ----------------------------------------------------------------------------------
     module procedure :: identity_R8
  end interface identity


  !/ -------------------------------------------------------------------------------------
  interface equals
     !/ ----------------------------------------------------------------------------------
     module procedure :: matrix_equals_R8
  end interface equals


  !/ -------------------------------------------------------------------------------------
  interface det
     !/ ----------------------------------------------------------------------------------
     module procedure :: det_n_R8
  end interface det


  !/ -------------------------------------------------------------------------------------
  interface inverse
     !/ ----------------------------------------------------------------------------------
     module procedure :: inverse_n_R8
  end interface inverse


  !/ -------------------------------------------------------------------------------------
  interface MeanShift
     !/ ----------------------------------------------------------------------------------
     module procedure :: mean_shift_vec_R8
     module procedure :: mean_shift_vec_R8_inplace
     module procedure :: mean_shift_mat_R8
     module procedure :: mean_shift_mat_R8_inplace
  end interface MeanShift


  !/ -------------------------------------------------------------------------------------
  interface ATA
     !/ ----------------------------------------------------------------------------------
     module procedure :: dot_AT_A_R8
  end interface ATA

  !/ -------------------------------------------------------------------------------------
  interface ATB
     !/ ----------------------------------------------------------------------------------
     module procedure :: dot_AT_B_R8
  end interface ATB

  !/ -------------------------------------------------------------------------------------
  interface AAT
     !/ ----------------------------------------------------------------------------------
     module procedure :: dot_A_AT_R8
  end interface AAT


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine diagonal_R8( mat, SET, GET )
    !/ -----------------------------------------------------------------------------------
    !! Diagonal Matrix.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout)           :: mat(:,:)  !!  matrix
    real(dp), optional, intent(in)    :: SET(:)    !!  input  diagonal vector
    real(dp), optional, intent(inout) :: GET(:)    !!  output diagonal vector
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = min( size(mat,DIM=1), size(mat,DIM=2) )

    if ( present( GET ) ) then
       do concurrent(i=1:n)
          GET(i) = mat(i,i)
       end do
    end if

    if ( present( SET ) ) then
       do concurrent(i=1:n)
          mat(i,i) = SET(i)
       end do
    end if

  end subroutine diagonal_R8


  !/ =====================================================================================
  subroutine identity_R8( mat )
    !/ -----------------------------------------------------------------------------------
    !! Identity Matrix.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mat(:,:)  !!  matrix
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(mat,DIM=1)
    nc = size(mat,DIM=2)
    do concurrent(r=1:nr,c=1:nc)
       if ( r.eq.c ) then
          mat(r,c) = D_ONE
       else
          mat(r,c) = D_ZERO
       end if
    end do
  end subroutine identity_R8








  !/ =====================================================================================
  function matrix_equals_R8( ml, mr ) result( eq )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: ml(:,:)  !!  matrix
    real(dp), intent(in) :: mr(:,:)  !!  matrix
    logical              :: eq
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(mr,DIM=2)
    if ( nr.ne.size(mr,DIM=1) ) goto 800
    if ( nc.ne.size(mr,DIM=2) ) goto 800
    do c=1,nc
       do r=1,nr
          if ( ml(r,c).lt.mr(r,c) ) goto 800
          if ( ml(r,c).gt.mr(r,c) ) goto 800
       end do
    end do
    eq=.true.
    goto 999
800 continue
    eq=.false.
999 continue
  end function matrix_equals_R8








  !/ =====================================================================================
  subroutine add_ms_R8( mc, ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise addition. $M_c = M_l + s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) + sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) + sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine add_ms_R8


  !/ =====================================================================================
  subroutine add_sm_R8( mc, sl, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise addition. $M_c = s_r + M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: sl      !! left  hand side input scalar.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(mr,DIM=1)
    nc = size(mr,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = sl + mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,sl,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = sl + mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine add_sm_R8


  !/ =====================================================================================
  subroutine add_ms_R8_inplace( ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise inplace addition. $M_l = M_l + s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) + sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) + sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine add_ms_R8_inplace


  !/ =====================================================================================
  subroutine add_mm_R8_inplace( ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace addition. $M_l = M_l + M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) + mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,mr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) + mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine add_mm_R8_inplace


  !/ =====================================================================================
  subroutine add_mm_R8( mc, ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace addition. $M_c = M_l + M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) + mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,mc,ml,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) + mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine add_mm_R8








  !/ =====================================================================================
  subroutine sub_ms_R8( mc, ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise subtraction. $M_c = M_l - s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) - sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) - sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine sub_ms_R8


  !/ =====================================================================================
  subroutine sub_sm_R8( mc, sl, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise subtraction. $M_c = s_r - M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: sl      !! left  hand side input scalar.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(mr,DIM=1)
    nc = size(mr,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = sl - mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,sl,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = sl - mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine sub_sm_R8


  !/ =====================================================================================
  subroutine sub_ms_R8_inplace( ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise inplace subtraction. $M_l = M_l - s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) - sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) - sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine sub_ms_R8_inplace


  !/ =====================================================================================
  subroutine sub_mm_R8_inplace( ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace subtraction. $M_l = M_l - M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) - mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,mr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) - mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine sub_mm_R8_inplace


  !/ =====================================================================================
  subroutine sub_mm_R8( mc, ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace subtraction. $M_c = M_l - M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) - mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,mc,ml,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) - mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine sub_mm_R8








  !/ =====================================================================================
  subroutine mul_ms_R8( mc, ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise multiplication. $M_c = M_l * s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) * sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) * sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine mul_ms_R8


  !/ =====================================================================================
  subroutine mul_sm_R8( mc, sl, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise multiplication. $M_c = s_r * M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: sl      !! left  hand side input scalar.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(mr,DIM=1)
    nc = size(mr,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = sl * mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,sl,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = sl * mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine mul_sm_R8


  !/ =====================================================================================
  subroutine mul_ms_R8_inplace( ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise inplace multiplication. $M_l = M_l * s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) * sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) * sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine mul_ms_R8_inplace


  !/ =====================================================================================
  subroutine mul_mm_R8_inplace( ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace multiplication. $M_l = M_l * M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) * mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,mr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) * mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine mul_mm_R8_inplace


  !/ =====================================================================================
  subroutine mul_mm_R8( mc, ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace multiplication. $M_c = M_l * M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) * mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,mc,ml,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) * mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine mul_mm_R8


  !/ =====================================================================================
  subroutine mul_dm_R8( mat, D, M )
    !/ -----------------------------------------------------------------------------------
    !! Diagonal and Matrix multiplication. ${mat} = D \cdot M$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: mat(:,:) !! Resulting Matrix.
    real(dp), intent(in)  :: D(:)     !! Diagonal elements.
    real(dp), intent(in)  :: M(:,:)   !! Matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, n
    !/ -----------------------------------------------------------------------------------
    n = size(D)

    if ( n.lt.MIN_M_PAR ) then
       do concurrent (r=1:n, c=1:n)
          mat(r,c) = D(r) * M(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(n,mat,D,M)
       do c=1,n
          do r=1,n
             mat(r,c) = D(r) * M(r,c)
          end do
       end do
       !$omp end parallel do
    end if

  end subroutine mul_dm_R8

  !/ =====================================================================================
  subroutine div_ms_R8( mc, ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise division. $M_c = M_l / s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) / sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) / sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine div_ms_R8


  !/ =====================================================================================
  subroutine div_sm_R8( mc, sl, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise division. $M_c = s_r / M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: sl      !! left  hand side input scalar.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(mr,DIM=1)
    nc = size(mr,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = sl / mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,sl,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = sl / mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine div_sm_R8


  !/ =====================================================================================
  subroutine div_ms_R8_inplace( ml, sr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Scalar element-wise inplace division. $M_l = M_l / s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: sr      !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) / sr
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,sr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) / sr
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine div_ms_R8_inplace


  !/ =====================================================================================
  subroutine div_mm_R8_inplace( ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace division. $M_l = M_l / M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          ml(r,c) = ml(r,c) / mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,ml,mr)
       do c=1,nc
          do r=1,nr
             ml(r,c) = ml(r,c) / mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine div_mm_R8_inplace


  !/ =====================================================================================
  subroutine div_mm_R8( mc, ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Matrix element-wise inplace division. $M_c = M_l / M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:) !! output matrix.
    real(dp), intent(in)    :: ml(:,:) !! left  hand side input matrix.
    real(dp), intent(in)    :: mr(:,:) !! right hand side input matrix.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------
    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)
    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr, c=1:nc)
          mc(r,c) = ml(r,c) / mr(r,c)
       end do
    else
       !$omp parallel do private(r,c) shared(nr,nc,mc,ml,mr)
       do c=1,nc
          do r=1,nr
             mc(r,c) = ml(r,c) / mr(r,c)
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine div_mm_R8








  !/ =====================================================================================
  subroutine swap_mm_R8( a, b )
    !/ -----------------------------------------------------------------------------------
    !! Swap vectors.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: a(:,:) !! matrix.
    real(dp), intent(inout) :: b(:,:) !! matrix.
    !/ -----------------------------------------------------------------------------------
    integer  :: r,c,nr,nc
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------
    nr = size(a,DIM=1)
    nc = size(a,DIM=2)

    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr,c=1:nc)
          t      = a(r,c)
          a(r,c) = b(r,c)
          b(r,c) = t
       end do
    else
       !$omp parallel do private(r,c,t) shared(nr,nc,a,b)
       do c=1,nc
          do r=1,nr
             t      = a(r,c)
             a(r,c) = b(r,c)
             b(r,c) = t
          end do
       end do
       !$omp end parallel do
    end if
  end subroutine swap_mm_R8








  !/ =====================================================================================
  subroutine dot_mm_R8( mc, ml, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix multiplication. $M_c = M_l \cdot M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: mc(:,:)
    real(dp), intent(in)    :: ml(:,:)
    real(dp), intent(in)    :: mr(:,:)
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, k, nr, nc, nk
    !/ -----------------------------------------------------------------------------------

    nr = size(ml,DIM=1)
    nk = size(ml,DIM=2)
    nc = size(mr,DIM=2)

    if ( nk.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr,c=1:nc)
          mc(r,c) = D_ZERO
          do k = 1, nk
             mc(r,c) = mc(r,c) + ( ml(r,k) * mr(k,c) )
          end do
       end do
    else
       !$omp parallel shared ( mc, ml, mr, nr, nc, nk ) private ( r, c, k )
       !$omp do
       do r = 1, nr
          do c = 1, nc
             mc(r,c) = D_ZERO
             do k = 1, nk
                mc(r,c) = mc(r,c) + ( ml(r,k) * mr(k,c) )
             end do
          end do
       end do
       !$omp end do
       !$omp end parallel
    end if
  end subroutine dot_mm_R8


  !/ =====================================================================================
  subroutine dot_mv_R8( vc, ml, vr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Vector multiplication. $M_c = M_l \cdot \hat{v_l}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:)
    real(dp), intent(in)    :: ml(:,:)
    real(dp), intent(in)    :: vr(:)
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------

    nr = size(ml,DIM=1)
    nc = size(ml,DIM=2)

    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(r=1:nr)
          vc(r) = D_ZERO
          do c = 1, nc
             vc(r) = vc(r) + ( ml(r,c) * vr(c) )
          end do
       end do
    else
       !$omp parallel shared ( vc, ml, vr, nr, nc ) private ( r, c )
       !$omp do
       do r=1,nr
          vc(r) = D_ZERO
          do c = 1, nc
             vc(r) = vc(r) + ( ml(r,c) * vr(c) )
          end do
       end do
       !$omp end do
       !$omp end parallel
    end if
  end subroutine dot_mv_R8


  !/ =====================================================================================
  subroutine dot_vm_R8( vc, vl, mr )
    !/ -----------------------------------------------------------------------------------
    !! Matrix and Vector multiplication. $M_c = \hat{v_l} \cdot M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:)
    real(dp), intent(in)    :: vl(:)
    real(dp), intent(in)    :: mr(:,:)
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nr, nc
    !/ -----------------------------------------------------------------------------------

    nr = size(mr,DIM=1)
    nc = size(mr,DIM=2)

    if ( nr.lt.MIN_M_PAR ) then
       do concurrent(c=1:nc)
          vc(c) = D_ZERO
          do r = 1, nr
             vc(c) = vc(c) + ( vl(r) * mr(r,c) )
          end do
       end do
    else
       !$omp parallel shared ( vc, mr, vl, nr, nc ) private ( r, c )
       !$omp do
       do c=1,nc
          vc(c) = D_ZERO
          do r = 1, nr
             vc(c) = vc(c) + ( vl(r) * mr(r,c) )
          end do
       end do
       !$omp end do
       !$omp end parallel
    end if
  end subroutine dot_vm_R8








  !/ =====================================================================================
  function det_2_R8( mat ) result( d )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the determinant of a 2x2 Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: mat(:,:)
    real(dp)             :: d
    !/ -----------------------------------------------------------------------------------
    d = (mat(1,1)*mat(2,2)) - (mat(1,2)*mat(2,1))
  end function det_2_R8


  !/ =====================================================================================
  function det_3_R8( mat ) result( d )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the determinant of a 3x3 Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: mat(:,:)    
    real(dp)             :: d
    !/ -----------------------------------------------------------------------------------
    d =      mat(1,1)*(mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2)) + &
         &   mat(1,2)*(mat(2,3)*mat(3,1) - mat(2,1)*mat(3,3)) + &
         &   mat(1,3)*(mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1))
  end function det_3_R8


  !/ =====================================================================================
  function det_4_R8( mat ) result( d )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the determinant of a 4x4 Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: mat(:,:)    
    real(dp)             :: d
    !/ -----------------------------------------------------------------------------------
    d =     ((mat(1,4)*mat(2,3) - mat(1,3)*mat(2,4))*mat(3,2) +            &
         &   (mat(1,2)*mat(2,4) - mat(1,4)*mat(2,2))*mat(3,3) +            &
         &   (mat(1,3)*mat(2,2) - mat(1,2)*mat(2,3))*mat(3,4))*mat(4,1) +  &
         &  ((mat(1,3)*mat(2,4) - mat(1,4)*mat(2,3))*mat(3,1) +            &
         &   (mat(1,4)*mat(2,1) - mat(1,1)*mat(2,4))*mat(3,3) +            &
         &   (mat(1,1)*mat(2,3) - mat(1,3)*mat(2,1))*mat(3,4))*mat(4,2) +  &
         &  ((mat(1,4)*mat(2,2) - mat(1,2)*mat(2,4))*mat(3,1) +            &
         &   (mat(1,1)*mat(2,4) - mat(1,4)*mat(2,1))*mat(3,2) +            &
         &   (mat(1,2)*mat(2,1) - mat(1,1)*mat(2,2))*mat(3,4))*mat(4,3) +  &
         &  ((mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2))*mat(3,1) +            &
         &   (mat(1,3)*mat(2,1) - mat(1,1)*mat(2,3))*mat(3,2) +            &
         &   (mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1))*mat(3,3))*mat(4,4)
  end function det_4_R8


  !/ =====================================================================================
  function det_n_R8( mat ) result( d )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the determinant of a NxN Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: mat(:,:)    
    real(dp)             :: d
    !/ -----------------------------------------------------------------------------------
    integer :: n
    !/ -----------------------------------------------------------------------------------
    n = size(mat,DIM=1)
    if ( 1.eq.n ) then
       d = 0.0d0
    elseif ( 2.eq.n ) then
       d = det_2_R8( mat )
    elseif ( 3.eq.n ) then
       d = det_3_R8( mat )
    elseif ( 4.eq.n ) then
       d = det_4_R8( mat )
    else
       write( ERROR_UNIT, * ) 'Determinant: rank > 4 not yet implemented'
       stop
    end if

  end function det_n_R8








  !/ =====================================================================================
  subroutine inverse_2_R8( inv, mat, D )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the inverse of a 2x2 Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: inv(:,:)  !! inverse of mat.
    real(dp),           intent(in)    :: mat(:,:)  !! matrix.
    real(dp), optional, intent(out)   :: D         !! determinant of mat.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: md
    !/ -----------------------------------------------------------------------------------
    md = det_2_R8( mat )

    if ( isZero( md ) ) goto 900

    inv(1,1) =  mat(2,2) / md
    inv(2,1) = -mat(2,1) / md
    inv(1,2) = -mat(1,2) / md
    inv(2,2) =  mat(1,1) / md

    goto 999

900 continue

    md = 0.0d0

999 continue

    if ( present( D ) ) D = md

  end subroutine inverse_2_R8


  !/ =====================================================================================
  subroutine inverse_3_R8( inv, mat, D )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the inverse of a 3x3 Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: inv(:,:)  !! inverse of mat.
    real(dp),           intent(in)    :: mat(:,:)  !! matrix.
    real(dp), optional, intent(out)   :: D         !! determinant of mat.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: md
    !/ -----------------------------------------------------------------------------------
    md = det_3_R8( mat )

    if ( isZero( md ) ) goto 900

    inv(1,1) = ( mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2) ) / md
    inv(1,2) = ( mat(1,3)*mat(3,2) - mat(1,2)*mat(3,3) ) / md
    inv(1,3) = ( mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2) ) / md

    inv(2,1) = ( mat(2,3)*mat(3,1) - mat(2,1)*mat(3,3) ) / md
    inv(2,2) = ( mat(1,1)*mat(3,3) - mat(1,3)*mat(3,1) ) / md
    inv(2,3) = ( mat(1,3)*mat(2,1) - mat(1,1)*mat(2,3) ) / md

    inv(3,1) = ( mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1) ) / md
    inv(3,2) = ( mat(1,2)*mat(3,1) - mat(1,1)*mat(3,2) ) / md
    inv(3,3) = ( mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1) ) / md

    goto 999

900 continue

    md = 0.0d0

999 continue

    if ( present( D ) ) D = md

  end subroutine inverse_3_R8


  !/ =====================================================================================
  subroutine inverse_4_R8( inv, mat, D )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the inverse of a 4x4 Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: inv(:,:)  !! inverse of mat.
    real(dp),           intent(in)    :: mat(:,:)  !! matrix.
    real(dp), optional, intent(out)   :: D         !! determinant of mat.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: md
    !/ -----------------------------------------------------------------------------------
    md = det_4_R8( mat )

    if ( isZero( md ) ) goto 900

    inv(1,1) = ( (mat(2,3)*mat(3,4) - mat(2,4)*mat(3,3))*mat(4,2) +        &
         &       (mat(2,4)*mat(3,2) - mat(2,2)*mat(3,4))*mat(4,3) +        &
         &       (mat(2,2)*mat(3,3) - mat(2,3)*mat(3,2))*mat(4,4) ) / md

    inv(1,2) = ( (mat(1,4)*mat(3,3) - mat(1,3)*mat(3,4))*mat(4,2) +        &
         &       (mat(1,2)*mat(3,4) - mat(1,4)*mat(3,2))*mat(4,3) +        &
         &       (mat(1,3)*mat(3,2) - mat(1,2)*mat(3,3))*mat(4,4) ) / md

    inv(1,3) = ( (mat(1,3)*mat(2,4) - mat(1,4)*mat(2,3))*mat(4,2) +        &
         &       (mat(1,4)*mat(2,2) - mat(1,2)*mat(2,4))*mat(4,3) +        &
         &       (mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2))*mat(4,4) ) / md

    inv(1,4) = ( (mat(1,4)*mat(2,3) - mat(1,3)*mat(2,4))*mat(3,2) +        &
         &       (mat(1,2)*mat(2,4) - mat(1,4)*mat(2,2))*mat(3,3) +        &
         &       (mat(1,3)*mat(2,2) - mat(1,2)*mat(2,3))*mat(3,4) ) / md


    inv(2,1) = ( (mat(2,4)*mat(3,3) - mat(2,3)*mat(3,4))*mat(4,1) +        &
         &       (mat(2,1)*mat(3,4) - mat(2,4)*mat(3,1))*mat(4,3) +        &
         &       (mat(2,3)*mat(3,1) - mat(2,1)*mat(3,3))*mat(4,4) ) / md

    inv(2,2) = ( (mat(1,3)*mat(3,4) - mat(1,4)*mat(3,3))*mat(4,1) +        &
         &       (mat(1,4)*mat(3,1) - mat(1,1)*mat(3,4))*mat(4,3) +        &
         &       (mat(1,1)*mat(3,3) - mat(1,3)*mat(3,1))*mat(4,4) ) / md

    inv(2,3) = ( (mat(1,4)*mat(2,3) - mat(1,3)*mat(2,4))*mat(4,1) +        &
         &       (mat(1,1)*mat(2,4) - mat(1,4)*mat(2,1))*mat(4,3) +        &
         &       (mat(1,3)*mat(2,1) - mat(1,1)*mat(2,3))*mat(4,4) ) / md

    inv(2,4) = ( (mat(1,3)*mat(2,4) - mat(1,4)*mat(2,3))*mat(3,1) +        &
         &       (mat(1,4)*mat(2,1) - mat(1,1)*mat(2,4))*mat(3,3) +        &
         &       (mat(1,1)*mat(2,3) - mat(1,3)*mat(2,1))*mat(3,4) ) / md


    inv(3,1) = ( (mat(2,2)*mat(3,4) - mat(2,4)*mat(3,2))*mat(4,1) +        &
         &       (mat(2,4)*mat(3,1) - mat(2,1)*mat(3,4))*mat(4,2) +        &
         &       (mat(2,1)*mat(3,2) - mat(2,2)*mat(3,1))*mat(4,4) ) / md

    inv(3,2) = ( (mat(1,4)*mat(3,2) - mat(1,2)*mat(3,4))*mat(4,1) +        &
         &       (mat(1,1)*mat(3,4) - mat(1,4)*mat(3,1))*mat(4,2) +        &
         &       (mat(1,2)*mat(3,1) - mat(1,1)*mat(3,2))*mat(4,4) ) / md

    inv(3,3) = ( (mat(1,2)*mat(2,4) - mat(1,4)*mat(2,2))*mat(4,1) +        &
         &       (mat(1,4)*mat(2,1) - mat(1,1)*mat(2,4))*mat(4,2) +        &
         &       (mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1))*mat(4,4) ) / md

    inv(3,4) = ( (mat(1,4)*mat(2,2) - mat(1,2)*mat(2,4))*mat(3,1) +        &
         &       (mat(1,1)*mat(2,4) - mat(1,4)*mat(2,1))*mat(3,2) +        &
         &       (mat(1,2)*mat(2,1) - mat(1,1)*mat(2,2))*mat(3,4) ) / md


    inv(4,1) = ( (mat(2,3)*mat(3,2) - mat(2,2)*mat(3,3))*mat(4,1) +        &
         &       (mat(2,1)*mat(3,3) - mat(2,3)*mat(3,1))*mat(4,2) +        &
         &       (mat(2,2)*mat(3,1) - mat(2,1)*mat(3,2))*mat(4,3) ) / md

    inv(4,2) = ( (mat(1,2)*mat(3,3) - mat(1,3)*mat(3,2))*mat(4,1) +        &
         &       (mat(1,3)*mat(3,1) - mat(1,1)*mat(3,3))*mat(4,2) +        &
         &       (mat(1,1)*mat(3,2) - mat(1,2)*mat(3,1))*mat(4,3) ) / md

    inv(4,3) = ( (mat(1,3)*mat(2,2) - mat(1,2)*mat(2,3))*mat(4,1) +        &
         &       (mat(1,1)*mat(2,3) - mat(1,3)*mat(2,1))*mat(4,2) +        &
         &       (mat(1,2)*mat(2,1) - mat(1,1)*mat(2,2))*mat(4,3) ) / md

    inv(4,4) = ( (mat(1,2)*mat(2,3) - mat(1,3)*mat(2,2))*mat(3,1) +        &
         &       (mat(1,3)*mat(2,1) - mat(1,1)*mat(2,3))*mat(3,2) +        &
         &       (mat(1,1)*mat(2,2) - mat(1,2)*mat(2,1))*mat(3,3) ) / md

    goto 999

900 continue

    md = 0.0d0

999 continue

    if ( present( D ) ) D = md

  end subroutine inverse_4_R8


  !/ =====================================================================================
  subroutine inverse_n_R8( inv, mat, D )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the inverse of a NxN Matrix directly.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(inout) :: inv(:,:)  !! inverse of mat.
    real(dp),           intent(in)    :: mat(:,:)  !! matrix.
    real(dp), optional, intent(out)   :: D         !! determinant of mat.
    !/ -----------------------------------------------------------------------------------
    integer               :: r, c, n, ier
    integer,  allocatable :: ip(:)
    !/ -----------------------------------------------------------------------------------
    n = size(mat,DIM=1)
    if ( 1.eq.n ) then
       d = 1.0d0 / mat(1,1)
    elseif ( 2.eq.n ) then
       call inverse_2_R8( inv, mat, D )
    elseif ( 3.eq.n ) then
       call inverse_3_R8( inv, mat, D )
    elseif ( 4.eq.n ) then
       call inverse_4_R8( inv, mat, D )
    else
       !/ --------------------------------------------------------------------------------
       allocate( ip(n) )

       if ( present( D ) ) D = D_ONE

       
       if ( n.lt.MIN_M_PAR ) then
          do concurrent( c=1:n, r=1:n )
             inv(r,c) = mat(r,c)
          end do
       else
          !$omp parallel shared ( inv, mat, n ) private ( r, c )
          !$omp do
          do c=1,n
             do r=1,n
                inv(r,c) = mat(r,c)
             end do
          end do
          !$omp end do
          !$omp end parallel
       end if

       
       call TC_DGETRF( inv, IPIV=ip, INFO=ier )
       if ( 0.ne.ier ) then
          if ( 0.lt.ier ) then
             call log_warn( 'Matrix is singular' )
             if ( present( D ) ) D = D_ZERO
             goto 999
          end if
       end if

       call tc_dgetri( inv, ip, INFO=ier )
       if ( 0.ne.ier ) then
          if ( 0.lt.ier ) then
             call log_warn( 'Matrix is singular' )
             if ( present( D ) ) D = D_ZERO
             goto 999
          end if
       end if

       deallocate( ip )

       !/ --------------------------------------------------------------------------------
    end if

999 continue

  end subroutine inverse_n_R8



  subroutine eigen_value_2_R8( eval, ieval, mat )
    implicit none
    real(dp), intent(out) ::  eval(2)
    real(dp), intent(out) :: ieval(2)
    real(dp), intent(in)  :: mat(:,:)

    real(dp) :: q, a, b

    q = (mat(1,1) - mat(2,2))**2

    a = (mat(1,1) + mat(2,2)) / D_TWO

    if ( q.lt.D_ZERO ) then
       b = sqrt(-q) / D_TWO
       eval(1)  =  a
       eval(2)  =  a
       ieval(1) = -b
       ieval(2) =  b
    else
       b = sqrt(q) / D_TWO
       eval(1)  = a-b
       eval(2)  = a+b
       ieval(1) = D_ZERO
       ieval(2) = D_ZERO
    end if
  end subroutine eigen_value_2_R8


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
       !print *,'returning col mean'
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





  !/ =======================================================================================
  subroutine dot_AT_A_R8( B, A )
    !/ -------------------------------------------------------------------------------------
    !! Inner product of a Transpose of a Matrix with itself.  $$B = A^T \cdot A$$
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: B(:,:)  !! Resulting Matrix.
    real(dp), intent(in)    :: A(:,:)  !! source Matrix.
    !/ -------------------------------------------------------------------------------------
    integer :: r,c,k,nr,nc
    !/ -------------------------------------------------------------------------------------
    nr = size( A, DIM=1 )
    nc = size( A, DIM=2 )

    do r=1,nc
       do c=1,nc
          B(r,c) = D_ZERO
          do k=1,nr
             B(r,c) = B(r,c) + ( A(k,r) * A(k,c) )
          end do
       end do
    end do

  end subroutine dot_AT_A_R8


  !/ =======================================================================================
  subroutine dot_AT_B_R8( D, A, B )
    !/ -------------------------------------------------------------------------------------
    !! Inner product of a Transpose of a Matrix with itself.  $$D = A^T \cdot B$$
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: D(:,:)  !! Resulting Matrix.
    real(dp), intent(in)    :: A(:,:)  !! source Matrix to transpose.
    real(dp), intent(in)    :: B(:,:)  !! source Matrix.
    !/ -------------------------------------------------------------------------------------
    integer :: r,c,k,ar,ac,bc
    !/ -------------------------------------------------------------------------------------
    ar = size( A, DIM=1 )
    ac = size( A, DIM=2 )
    bc = size( B, DIM=2 )

    do r=1,ac
       do c=1,bc
          D(r,c) = D_ZERO
          do k=1,ar
             D(r,c) = D(r,c) + ( A(k,r) * B(k,c) )
          end do
       end do
    end do

  end subroutine dot_AT_B_R8


  !/ =======================================================================================
  subroutine dot_A_AT_R8( B, A )
    !/ -------------------------------------------------------------------------------------
    !! inner product of a Matrix with its own transpose. $$C = B \cdot A^T$$
    !/ -------------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: B(:,:)  !! Resulting Matrix.
    real(dp), intent(in)    :: A(:,:)  !! source Matrix.
    !/ -------------------------------------------------------------------------------------
    integer :: r,c,k,nr,nc
    !/ -------------------------------------------------------------------------------------
    nr = size( A, DIM=1 )
    nc = size( A, DIM=2 )

    do r=1,nr
       do c=1,nr
          B(r,c) = D_ZERO
          do k=1,nc
             B(r,c) = B(r,c) + ( A(r,k) * A(c,k) )
          end do
       end do
    end do

  end subroutine dot_A_AT_R8




end module matrix_mod


!/ =======================================================================================
!/ **                                M A T R I X _ M O D                                **
!/ =========================================================================== END FILE ==
