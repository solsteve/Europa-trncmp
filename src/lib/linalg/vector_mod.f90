!/ ====================================================================== BEGIN FILE =====
!/ **                                V E C T O R _ M O D                                **
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
module vector_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-11-22
  !! license: GPL
  !!
  !!## Vector procedures.
  !!
  !! Collection of procedures for manipulating vectors.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env


  integer, parameter :: MIN_V_PAR = 256 !! minimum vector length for parallization with openmp


  !/ -------------------------------------------------------------------------------------
  interface add
     !/ -------------------------------------------------------------------------------------
     module procedure :: add_vs_R8
     module procedure :: add_sv_R8
     module procedure :: add_vs_R8_inplace
     module procedure :: add_vv_R8_inplace
     module procedure :: add_vv_R8
  end interface add


  !/ -------------------------------------------------------------------------------------
  interface sub
     !/ -------------------------------------------------------------------------------------
     module procedure :: sub_vs_R8
     module procedure :: sub_sv_R8
     module procedure :: sub_vs_R8_inplace
     module procedure :: sub_vv_R8_inplace
     module procedure :: sub_vv_R8
  end interface sub


  !/ -------------------------------------------------------------------------------------
  interface mul
     !/ -------------------------------------------------------------------------------------
     module procedure :: mul_vs_R8
     module procedure :: mul_sv_R8
     module procedure :: mul_vs_R8_inplace
     module procedure :: mul_vv_R8_inplace
     module procedure :: mul_vv_R8
  end interface mul


  !/ -------------------------------------------------------------------------------------
  interface div
     !/ -------------------------------------------------------------------------------------
     module procedure :: div_vs_R8
     module procedure :: div_sv_R8
     module procedure :: div_vs_R8_inplace
     module procedure :: div_vv_R8_inplace
     module procedure :: div_vv_R8
  end interface div


  !/ -------------------------------------------------------------------------------------
  interface inner
     !/ -------------------------------------------------------------------------------------
     module procedure :: inner_product_vv_R8
  end interface inner


  !/ -------------------------------------------------------------------------------------
  interface swap
     !/ -------------------------------------------------------------------------------------
     module procedure :: swap_vv_R8
  end interface swap

  !/ -------------------------------------------------------------------------------------
  interface L1Norm
     !/ -------------------------------------------------------------------------------------
     module procedure :: L1_norm_R8
     module procedure :: L1_diff_norm_R8
  end interface L1Norm

  !/ -------------------------------------------------------------------------------------
  interface L2Norm
     !/ -------------------------------------------------------------------------------------
     module procedure :: L2_norm_R8
     module procedure :: L2_diff_norm_R8
  end interface L2Norm

  !/ -------------------------------------------------------------------------------------
  interface PNorm
     !/ -------------------------------------------------------------------------------------
     module procedure ::P_norm_R8
  end interface PNorm

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine add_vs_R8( vc, vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar element-wise addition. $\hat{v_c} = \hat{v_l} + s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) + sr
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,sr)
       do i=1,n
          vc(i) = vl(i) + sr
       end do
       !$omp end parallel do
    end if
  end subroutine add_vs_R8


  !/ =====================================================================================
  subroutine add_sv_R8( vc, sl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Scalar plus Vector element-wise addition. $\hat{v_c} = s_l + \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: sl    !! left  hand side input scalar.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vr)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = sl + vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,sl,vr)
       do i=1,n
          vc(i) = sl + vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine add_sv_R8


  !/ =====================================================================================
  subroutine add_vs_R8_inplace( vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar inplace element-wise addition. $\hat{v_l} = \hat{v_l} + s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) + sr
       end do
    else
       !$omp parallel do private(i) shared(n,vl,sr)
       do i=1,n
          vl(i) = vl(i) + sr
       end do
       !$omp end parallel do
    end if
  end subroutine add_vs_R8_inplace


  !/ =====================================================================================
  subroutine add_vv_R8_inplace( vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector inplace element-wise addition. $\hat{v_l} = \hat{v_l} + \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) + vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vl,vr)
       do i=1,n
          vl(i) = vl(i) + vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine add_vv_R8_inplace


  !/ =====================================================================================
  subroutine add_vv_R8( vc, vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector element-wise addition. $\hat{v} = \hat{v_l} + \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) + vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,vr)
       do i=1,n
          vc(i) = vl(i) + vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine add_vv_R8








  !/ =====================================================================================
  subroutine sub_vs_R8( vc, vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar element-wise subtraction. $\hat{v_c} = \hat{v_l} - s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) - sr
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,sr)
       do i=1,n
          vc(i) = vl(i) - sr
       end do
       !$omp end parallel do
    end if
  end subroutine sub_vs_R8


  !/ =====================================================================================
  subroutine sub_sv_R8( vc, sl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Scalar plus Vector element-wise subtraction. $\hat{v_c} = s_l - \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: sl    !! left  hand side input scalar.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vr)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = sl - vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,sl,vr)
       do i=1,n
          vc(i) = sl - vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine sub_sv_R8


  !/ =====================================================================================
  subroutine sub_vs_R8_inplace( vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar inplace element-wise subtraction. $\hat{v_l} = \hat{v_l} - s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) - sr
       end do
    else
       !$omp parallel do private(i) shared(n,vl,sr)
       do i=1,n
          vl(i) = vl(i) - sr
       end do
       !$omp end parallel do
    end if
  end subroutine sub_vs_R8_inplace


  !/ =====================================================================================
  subroutine sub_vv_R8_inplace( vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector inplace element-wise subtraction. $\hat{v_l} = \hat{v_l} - \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) - vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vl,vr)
       do i=1,n
          vl(i) = vl(i) - vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine sub_vv_R8_inplace


  !/ =====================================================================================
  subroutine sub_vv_R8( vc, vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector element-wise subtraction. $\hat{v} = \hat{v_l} - \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) - vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,vr)
       do i=1,n
          vc(i) = vl(i) - vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine sub_vv_R8








  !/ =====================================================================================
  subroutine mul_vs_R8( vc, vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar element-wise multiplication. $\hat{v_c} = \hat{v_l} * s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) * sr
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,sr)
       do i=1,n
          vc(i) = vl(i) * sr
       end do
       !$omp end parallel do
    end if
  end subroutine mul_vs_R8


  !/ =====================================================================================
  subroutine mul_sv_R8( vc, sl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Scalar plus Vector element-wise multiplication. $\hat{v_c} = s_l * \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: sl    !! left  hand side input scalar.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vr)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = sl * vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,sl,vr)
       do i=1,n
          vc(i) = sl * vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine mul_sv_R8


  !/ =====================================================================================
  subroutine mul_vs_R8_inplace( vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar inplace element-wise multiplication. $\hat{v_l} = \hat{v_l} * s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) * sr
       end do
    else
       !$omp parallel do private(i) shared(n,vl,sr)
       do i=1,n
          vl(i) = vl(i) * sr
       end do
       !$omp end parallel do
    end if
  end subroutine mul_vs_R8_inplace


  !/ =====================================================================================
  subroutine mul_vv_R8_inplace( vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector inplace element-wise multiplication. $\hat{v_l} = \hat{v_l} * \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) * vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vl,vr)
       do i=1,n
          vl(i) = vl(i) * vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine mul_vv_R8_inplace


  !/ =====================================================================================
  subroutine mul_vv_R8( vc, vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector element-wise multiplication. $\hat{v} = \hat{v_l} * \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) * vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,vr)
       do i=1,n
          vc(i) = vl(i) * vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine mul_vv_R8








  !/ =====================================================================================
  subroutine div_vs_R8( vc, vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar element-wise division. $\hat{v_c} = \hat{v_l} / s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) / sr
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,sr)
       do i=1,n
          vc(i) = vl(i) / sr
       end do
       !$omp end parallel do
    end if
  end subroutine div_vs_R8


  !/ =====================================================================================
  subroutine div_sv_R8( vc, sl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Scalar plus Vector element-wise division. $\hat{v_c} = s_l / \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: sl    !! left  hand side input scalar.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vr)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = sl / vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,sl,vr)
       do i=1,n
          vc(i) = sl / vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine div_sv_R8


  !/ =====================================================================================
  subroutine div_vs_R8_inplace( vl, sr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Scalar inplace element-wise division. $\hat{v_l} = \hat{v_l} / s_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: sr    !! right hand side input scalar.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) / sr
       end do
    else
       !$omp parallel do private(i) shared(n,vl,sr)
       do i=1,n
          vl(i) = vl(i) / sr
       end do
       !$omp end parallel do
    end if
  end subroutine div_vs_R8_inplace


  !/ =====================================================================================
  subroutine div_vv_R8_inplace( vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector inplace element-wise division. $\hat{v_l} = \hat{v_l} / \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vl(i) = vl(i) / vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vl,vr)
       do i=1,n
          vl(i) = vl(i) / vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine div_vv_R8_inplace


  !/ =====================================================================================
  subroutine div_vv_R8( vc, vl, vr )
    !/ -----------------------------------------------------------------------------------
    !! Vector plus Vector element-wise division. $\hat{v} = \hat{v_l} / \hat{v_r}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: vc(:) !! output vector.
    real(dp), intent(in)    :: vl(:) !! left  hand side input vector.
    real(dp), intent(in)    :: vr(:) !! right hand side input vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(vl)
    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          vc(i) = vl(i) / vr(i)
       end do
    else
       !$omp parallel do private(i) shared(n,vc,vl,vr)
       do i=1,n
          vc(i) = vl(i) / vr(i)
       end do
       !$omp end parallel do
    end if
  end subroutine div_vv_R8








  !/ =====================================================================================
  function inner_product_vv_R8( vl, VR ) result( p )
    !/ -----------------------------------------------------------------------------------
    !! Inner product. $p = \sum^N_{i=1} v^{(L)}_i \cdot v^{(R)}_i$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(in) :: vl(:) !! left  hand side input vector.
    real(dp), optional, intent(in) :: VR(:) !! right hand side input vector.
    real(dp)                       :: p
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: s
    !/ -----------------------------------------------------------------------------------

    n = size(vl)
    s = D_ZERO

    if ( present( VR ) ) then
       if ( n.lt.MIN_V_PAR ) then
          do concurrent(i=1:n)
             s = s + (vl(i) * vr(i))
          end do
       else
          !$omp parallel private(i) shared(n,vl,vr)
          !$omp  do reduction(+:s)
          do i=1,n
             s = s + (vl(i) * vr(i))
          end do
          !$omp end do
          !$omp end parallel
       end if
    else
       if ( n.lt.MIN_V_PAR ) then
          do concurrent(i=1:n)
             s = s + (vl(i) * vl(i))
          end do
       else
          !$omp parallel private(i) shared(n,vl,vr)
          !$omp  do reduction(+:s)
          do i=1,n
             s = s + (vl(i) * vl(i))
          end do
          !$omp end do
          !$omp end parallel
       end if
    end if

    p = s

  end function inner_product_vv_R8








  !/ =====================================================================================
  subroutine swap_vv_R8( a, b )
    !/ -----------------------------------------------------------------------------------
    !! Swap vectors.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: a(:) !! vector.
    real(dp), intent(inout) :: b(:) !! vector.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------
    n = size(a)

    if ( n.lt.MIN_V_PAR ) then
       do concurrent(i=1:n)
          t    = a(i)
          a(i) = b(i)
          b(i) = t
       end do
    else
       !$omp parallel do private(i,t) shared(n,a,b)
       do i=1,n
          t    = a(i)
          a(i) = b(i)
          b(i) = t
       end do
       !$omp end parallel do
    end if
  end subroutine swap_vv_R8








  !/ =====================================================================================
  function L1_norm_R8( x ) result( L1 )
    !/ -----------------------------------------------------------------------------------
    !! L1 Norm = $\sum^N_{i=1} \left|{x_i}\right|$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: x(:) !! input vector.
    real(dp)             :: L1   !! L1 norm.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, total
    integer  :: i, n

    total = D_ZERO
    n     = size(x)

    if ( n.lt.MIN_V_PAR ) then
       do i=1,n
          d = x(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d
       end do
    else
       !$omp parallel private(i,d) shared(x,n)
       !$omp do reduction (+:total)
       do i=1,n
          d = x(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d
       end do
       !$omp end do
       !$omp end parallel
    end if

    L1 = total
  end function L1_norm_R8


  !/ =====================================================================================
  function L1_diff_norm_R8( x, y ) result( L1 )
    !/ -----------------------------------------------------------------------------------
    !! L1 Norm = $\sum^N_{i=1} \left|{x_i} - {y_i}\right|$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: x(:) !! input vector 1.
    real(dp), intent(in) :: y(:) !! input vector 2.
    real(dp)             :: L1   !! L1 norm.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, total
    integer  :: i, n

    total = D_ZERO
    n     = size(x)

    if ( n.lt.MIN_V_PAR ) then
       do i=1,n
          d = x(i) - y(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d
       end do
    else
       !$omp parallel private(i,d) shared(x,y,n)
       !$omp do reduction (+:total)
       do i=1,n
          d = x(i) - y(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d
       end do
       !$omp end do
       !$omp end parallel
    end if

    L1 = total
  end function L1_diff_norm_R8


  !/ =====================================================================================
  function L2_norm_R8( x ) result( L2 )
    !/ -----------------------------------------------------------------------------------
    !! L2 Norm = ${\left[{\sum^N_{i=1} {x_i}^2}\right]}^{1/2}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: x(:) !! input vector.
    real(dp)             :: L2   !! L2 norm.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, total
    integer  :: i, n

    total = D_ZERO
    n     = size(x)

    if ( n.lt.MIN_V_PAR ) then
       do i=1,n
          d = x(i)
          total = total + (d*d)
       end do
    else
       !$omp parallel private(i,d) shared(x,n)
       !$omp do reduction (+:total)
       do i=1,n
          d = x(i)
          total = total + (d*d)
       end do
       !$omp end do
       !$omp end parallel
    end if

    L2 = sqrt(total)
  end function L2_norm_R8


  !/ =====================================================================================
  function L2_diff_norm_R8( x, y ) result( L2 )
    !/ -----------------------------------------------------------------------------------
    !! L2 Norm = ${\left[{\sum^N_{i=1} {\left({x_i}-{x_i}\right)}^2}\right]}^{1/2}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: x(:) !! input vector 1.
    real(dp), intent(in) :: y(:) !! input vector 2.
    real(dp)             :: L2   !! L2 norm.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, total
    integer  :: i, n

    total = D_ZERO
    n     = size(x)

    if ( n.lt.MIN_V_PAR ) then
       do i=1,n
          d = x(i) - y(i)
          total = total + (d*d)
       end do
    else
       !$omp parallel private(i,d) shared(x,y,n)
       !$omp do reduction (+:total)
       do i=1,n
          d = x(i) - y(i)
          total = total + (d*d)
       end do
       !$omp end do
       !$omp end parallel
    end if

    L2 = sqrt(total)
  end function L2_diff_norm_R8


  !/ =====================================================================================
  function Lp_norm_R8( p, x ) result( Lp )
    !/ -----------------------------------------------------------------------------------
    !! Lp Norm = ${\left[{\sum^N_{i=1} \{x_i}^p}\right]}^{1/p}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: p    !! p level.
    real(dp), intent(in) :: x(:) !! input vector.
    real(dp)             :: Lp   !! Lp norm.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, total
    integer  :: i, n

    total = D_ZERO
    n     = size(x)

    if ( n.lt.MIN_V_PAR ) then
       do i=1,n
          d = x(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d**p
       end do
    else
       !$omp parallel private(i,d) shared(x,p,n)
       !$omp do reduction (+:total)
       do i=1,n
          d = x(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d**p
       end do
       !$omp end do
       !$omp end parallel
    end if

    Lp = exp(log(total)/real(p,dp))
  end function Lp_norm_R8


  !/ =====================================================================================
  function Lp_diff_norm_R8( p, x, y ) result( Lp )
    !/ -----------------------------------------------------------------------------------
    !! Lp Norm = ${\left[{\sum^N_{i=1} {\left({x_i}-{x_i}\right)}^p}\right]}^{1/p}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: p    !! p level.
    real(dp), intent(in) :: x(:) !! input vector 1.
    real(dp), intent(in) :: y(:) !! input vector 2.
    real(dp)             :: Lp   !! Lp norm.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, total
    integer  :: i, n

    total = D_ZERO
    n     = size(x)

    if ( n.lt.MIN_V_PAR ) then
       do i=1,n
          d = x(i) - y(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d**p
       end do
    else
       !$omp parallel private(i,d) shared(x,y,p,n)
       !$omp do reduction (+:total)
       do i=1,n
          d = x(i) - y(i)
          if ( d.lt.D_ZERO ) d = -d
          total = total + d**p
       end do
       !$omp end do
       !$omp end parallel
    end if

    Lp = exp(log(total)/real(p,dp))
  end function Lp_diff_norm_R8




  !/ =====================================================================================
  function P_norm_R8( p, x, Y ) result( Lp )
    !/ -----------------------------------------------------------------------------------
    !! Lp Norm = ${\left[{\sum^N_{i=1} {\left({x_i}-{x_i}\right)}^p}\right]}^{1/p}$
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,            intent(in) :: p    !! p level.
    real(dp),           intent(in) :: x(:) !! input vector 1.
    real(dp), optional, intent(in) :: Y(:) !! input vector 2.
    real(dp)                       :: Lp   !! Lp norm.
    !/ -----------------------------------------------------------------------------------

    if ( present( Y ) ) then
       if ( 1.eq.p ) then
          Lp = L1_diff_norm_R8( x, Y )
       else if ( 2.eq.p ) then
          Lp = L2_diff_norm_R8( x, Y )
       else
          Lp = Lp_diff_norm_R8( p, x, Y )
       end if
    else
       if ( 1.eq.p ) then
          Lp = L1_norm_R8( x )
       else if ( 2.eq.p ) then
          Lp = L2_norm_R8( x )
       else
          Lp = Lp_norm_R8( p, x )
       end if
    end if

  end function P_norm_R8


end module vector_mod


!/ =======================================================================================
!/ **                                V E C T O R _ M O D                                **
!/ =========================================================================== END FILE ==
