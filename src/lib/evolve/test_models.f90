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
  use real_model_mod
  implicit none


  !/ =====================================================================================
  type, extends(RealModel) :: Sphere
     !/ ----------------------------------------------------------------------------------
     !! $$f_0\left(\bar{x}\right) = \sum^n_{i=1}x_i^2$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => f1_build

     procedure, pass(dts) :: evaluate     => f1_eval
     procedure, nopass    :: isLeftBetter => f1_better

  end type Sphere


  !/ =====================================================================================
  type, extends(RealModel) :: Rosenbrock
     !/ ----------------------------------------------------------------------------------
     !! $$f_1\left(\bar{x}\right) = \sum_{i=1}^{n-1}
     !!   \left[{100{\left({x_{i+1}-x_i^2}\right)}^2 +
     !!   {\left({1 - x_i}\right)}^2}\right]$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => f2_build

     procedure, pass(dts) :: evaluate     => f2_eval
     procedure, nopass    :: isLeftBetter => f2_better

  end type Rosenbrock


  !/ =====================================================================================
  type, extends(RealModel) :: Rastrigin
     !/ ----------------------------------------------------------------------------------
     !! $$f_2\left(\bar{x}\right) = 10 n + \sum_{i=0}^n \left[{x_i^2 -
     !!  10 \cos\left({2 \pi x_i}\right)}\right]$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => f3_build

     procedure, pass(dts) :: evaluate     => f3_eval
     procedure, nopass    :: isLeftBetter => f3_better

  end type Rastrigin


  !/ =====================================================================================
  type, extends(RealModel) :: Griewank
     !/ ----------------------------------------------------------------------------------
     !! $$f_3\left(\bar{x}\right) = 1 + \frac{1}{4000}\sum^n_{i=1}x_i^2 - 
     !!   \prod^n_{i=1} \cos\left(\frac{x_i}{\sqrt{i}}\right)$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => f4_build

     procedure, pass(dts) :: evaluate     => f4_eval
     procedure, nopass    :: isLeftBetter => f4_better

  end type Griewank


  !/ =====================================================================================
  type, extends(RealModel) :: Schaffer
     !/ ----------------------------------------------------------------------------------
     !! $$f_4\left(x,y\right) = \frac{1}{2} + \frac{
     !!   \sin^2\left(x^2 - y^2\right) - \frac{1}{2}
     !!   }{
     !!   {\left[{1 + \frac{1}{1000}\left(x^2+y^2\right)}\right]}^2
     !!   }$$
     !/ ----------------------------------------------------------------------------------

   contains

     procedure            :: build        => f5_build

     procedure, pass(dts) :: evaluate     => f5_eval
     procedure, nopass    :: isLeftBetter => f5_better

  end type Schaffer




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine f1_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Sphere), intent(inout) :: dts !! reference to this RealModel.
    integer,       intent(in)    :: n   !! number of variables to optimize.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=n, MINV=-1.0D2, MAXV=1.0D2 )
  end subroutine f1_build


  !/ =====================================================================================
  subroutine f1_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate sphere test function. [-100,100]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Sphere), intent(inout) :: dts       !! reference to this RealModel.
    real(dp),      intent(inout) :: metric(:) !! return metric.
    real(dp),      intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: sum, k
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable
    sum = D_ZERO

    do i=1,n
       k = dts%scale(param(i))
       sum = sum + (k*k)
    end do

    metric(1) = sum

  end subroutine f1_eval


  !/ =====================================================================================
  function f1_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)  !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)  !! right hand side metric.
    logical              :: truth   !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function f1_better








  !/ =====================================================================================
  subroutine f2_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Rosenbrock), intent(inout) :: dts !! reference to this RealModel.
    integer,           intent(in)    :: n   !! number of variables to optimize.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=n, MINV=-1.0d1, MAXV=1.0d1 )
  end subroutine f2_build


  !/ =====================================================================================
  subroutine f2_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate Rosenbrock test function. [-10,10]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Rosenbrock), intent(inout) :: dts       !! reference to this RealModel.
    real(dp),          intent(inout) :: metric(:) !! return metric.
    real(dp),          intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: sum, a, b, k, k1
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable-1
    sum = D_ZERO

    do i=1,n
       k  = dts%scale(param(i))
       k1 = dts%scale(param(i+1))
       a  = k1 - k*k
       b  = D_ONE - k
       sum = sum + (1.0D2*a*a - b*b)
    end do

    metric(1) = sum

  end subroutine f2_eval


  !/ =====================================================================================
  function f2_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)  !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)  !! right hand side metric.
    logical              :: truth   !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function f2_better








  !/ =====================================================================================
  subroutine f3_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Rastrigin), intent(inout) :: dts !! reference to this RealModel.
    integer,          intent(in)    :: n   !! number of variables to optimize.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=n, MINV=-5.12D0, MAXV=5.12D0 )
  end subroutine f3_build


  !/ =====================================================================================
  subroutine f3_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate Rastrigin test function. [-5.12,5.12]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Rastrigin), intent(inout) :: dts       !! reference to this RealModel.
    real(dp),         intent(inout) :: metric(:) !! return metric.
    real(dp),         intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: sum, k
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable
    sum = D_ZERO

    do i=1,n
       k = dts%scale(param(i))
       sum = sum + (k*k - 1.0d1*cos(D_2PI*k))
    end do

    metric(1) = 1.0d1*real(n,dp) + sum

  end subroutine f3_eval


  !/ =====================================================================================
  function f3_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)   !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)   !! right hand side metric.
    logical              :: truth !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function f3_better








  !/ =====================================================================================
  subroutine f4_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Griewank), intent(inout) :: dts !! reference to this RealModel.
    integer,         intent(in)    :: n   !! number of variables to optimize.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=n, MINV=-1.0d1, MAXV=1.0d1 )
  end subroutine f4_build


  !/ =====================================================================================
  subroutine f4_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate Griewank test function. [-200,200]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Griewank), intent(inout) :: dts       !! reference to this RealModel.
    real(dp),        intent(inout) :: metric(:) !! return metric.
    real(dp),        intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: P, S, k
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable
    S = D_ZERO
    P = D_ONE

    do i=1,n
       k = dts%scale(param(i))
       S = S + (k*k)
       P = P * cos(k / sqrt(real(i,dp)))
    end do

    metric(1) = D_ONE + (S/4.0d3) - P

  end subroutine f4_eval


  !/ =====================================================================================
  function f4_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)  !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)  !! right hand side metric.
    logical              :: truth   !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function f4_better










  !/ =====================================================================================
  subroutine f5_build( dts )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Schaffer), intent(inout) :: dts !! reference to this RealModel.
    !/ -----------------------------------------------------------------------------------
    call dts%super( MET=1, VAR=2, MINV=-1.0d2, MAXV=1.0d2 )
  end subroutine f5_build


  !/ =====================================================================================
  subroutine f5_eval( dts, metric, param )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate Schaffer's test function. [-100,100]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Schaffer), intent(inout) :: dts       !! reference to this RealModel.
    real(dp),        intent(inout) :: metric(:) !! return metric.
    real(dp),        intent(in)    :: param(:)  !! input parameters.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x, y, x2, y2, k1, k2
    !/ -----------------------------------------------------------------------------------
    x = dts%scale( param(1) )
    y = dts%scale( param(2) )

    x2 = x*x
    y2 = y*y

    k1 = sin( x2 - y2 )
    k2 = D_ONE + (x2 + y2)*1.0d-3

    metric(1) = D_HALF + (k1*k1 - D_HALF)/(k2*k2)
    

  end subroutine f5_eval


  !/ =====================================================================================
  function f5_better( lhs, rhs ) result ( truth )
    !/ -----------------------------------------------------------------------------------
    !! Minimize single metric function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)  !! left  hand side metric.
    real(dp), intent(in) :: rhs(:)  !! right hand side metric.
    logical              :: truth   !! true if lhs is better than rhs.
    !/ -----------------------------------------------------------------------------------
    truth = ( lhs(1).lt.rhs(1) )
  end function f5_better


end module test_models


!/ =======================================================================================
!/ **                               T E S T _ M O D E L S                               **
!/ ======================================================================== END FILE =====
