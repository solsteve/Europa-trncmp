!/ ====================================================================== BEGIN FILE =====
!/ **                               T E S T _ M O D E L S                               **
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
module test_models
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use pso_model_mod
  implicit none


  !/ =====================================================================================
  type, extends(PSO_Model) :: Sphere
     !/ ----------------------------------------------------------------------------------
     !! $$f_0\left(\bar{x}\right) = \sum^n_{i=1}x_i^2$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => sphere_build

     procedure, pass(dts) :: evaluate     => sphere_eval
     procedure, nopass    :: isLeftBetter => sphere_better

  end type Sphere


  !/ =====================================================================================
  type, extends(PSO_Model) :: Rosenbrock
     !/ ----------------------------------------------------------------------------------
     !! $$f_1\left(\bar{x}\right) = \sum_{i=1}^{n-1}
     !!   \left[{100{\left({x_{i+1}-x_i^2}\right)}^2 +
     !!   {\left({1 - x_i}\right)}^2}\right]$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => rb_build

     procedure, pass(dts) :: evaluate     => rb_eval
     procedure, nopass    :: isLeftBetter => rb_better

  end type Rosenbrock




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine sphere_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Sphere), intent(inout) :: dts !! reference to this PSO_Model.
    integer,          intent(in)    :: n   !! number of variables to optimize.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=n, MINV=-1.0D2, MAXV=1.0D2 )
  end subroutine sphere_build


  !/ =====================================================================================
  subroutine sphere_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate sphere test function. [-100,100]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Sphere), intent(inout) :: dts       !! reference to this PSO_Model.
    real(dp),         intent(inout) :: metric(:) !! return metric.
    real(dp),         intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: sum
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable
    sum = D_ZERO

    do i=1,n
       sum = sum + (1.0d4*param(i)*param(i))
    end do

    metric(1) = sum

  end subroutine sphere_eval


  !/ =====================================================================================
  function sphere_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)   !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)   !! right hand side metric.
    logical              :: truth !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function sphere_better






  !/ =====================================================================================
  subroutine rb_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Rosenbrock), intent(inout) :: dts !! reference to this PSO_Model.
    integer,          intent(in)    :: n   !! number of variables to optimize.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=n )
  end subroutine rb_build


  !/ =====================================================================================
  subroutine rb_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate sphere test function. [-100,100]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Rosenbrock), intent(inout) :: dts       !! reference to this PSO_Model.
    real(dp),         intent(inout) :: metric(:) !! return metric.
    real(dp),         intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: sum, a, b, k
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable-1
    sum = D_ZERO

    do i=1,n
       k = param(i)
       a = param(i+1) - k*k
       b = D_ONE - k
       sum = sum + (1.0D2*a*a - b*b)
    end do

    metric(1) = sum

  end subroutine rb_eval


  !/ =====================================================================================
  function rb_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)   !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)   !! right hand side metric.
    logical              :: truth !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function rb_better


end module test_models


!/ =======================================================================================
!/ **                               T E S T _ M O D E L S                               **
!/ ======================================================================== END FILE =====
