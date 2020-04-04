!/ ====================================================================== BEGIN FILE =====
!/ **                       F F N N _ A C T I V A T I O N _ M O D                       **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 1994-2020, Stephen W. Soliday                                      **
!/ **                           stephen.soliday@trncmp.org                              **
!/ **                           http://research.trncmp.org                              **
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
module FFNN_Activation_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides activation functions and thier first derivatives.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-02
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none


  !! -------------------------------------------------------------------------------------
  abstract interface
     pure function ActivateFunction( Z ) result( a )
       !! --------------------------------------------------------------------------------
       use trncmp_env, only : dp
       real(dp), intent(in) :: Z
       real(dp)             :: a
     end function ActivateFunction
  end interface


  !! -------------------------------------------------------------------------------------
  abstract interface
     pure function DActivateFunction( a, Z ) result( g )
       !! --------------------------------------------------------------------------------
       use trncmp_env, only : dp
       real(dp), intent(in) :: a
       real(dp), intent(in) :: Z
       real(dp)             :: g
     end function DActivateFunction
  end interface


  !! -------------------------------------------------------------------------------------
  interface getActivation
     !! --------------------------------------------------------------------------------
     module procedure :: get_activation_pointer
  end interface getActivation


  !! -------------------------------------------------------------------------------------
  interface getDActivation
     !! --------------------------------------------------------------------------------
     module procedure :: get_D_activation_pointer
  end interface getDActivation




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  pure function sigmoid_activation( Z ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Forward non-linear activation function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: Z  !! weighted sum of the inputs.
    real(dp)             :: a  !! non-linear activation of Z.
    !/ -----------------------------------------------------------------------------------
    a = 1.0d0 / ( 1.0d0 + exp( -Z ) )
  end function sigmoid_activation


  !/ =====================================================================================
  pure function D_sigmoid_activation( a, Z ) result( dadZ )
    !/ -----------------------------------------------------------------------------------
    !! First derivative of the non-linear activation function,
    !! with respect to the weighted sum of the inputs.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: a    !! non-linear activation of Z.
    real(dp), intent(in) :: Z    !! weighted sum of the inputs.
    real(dp)             :: dadZ !! first derivative of a with respect to Z.
    !/ -----------------------------------------------------------------------------------
    dadZ = a*(1.0d0-a)
  end function D_sigmoid_activation




  !/ =====================================================================================
  pure function tanh_activation( Z ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Forward non-linear activation function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: Z  !! weighted sum of the inputs.
    real(dp)             :: a  !! non-linear activation of Z.
    !/ -----------------------------------------------------------------------------------
    a = tanh( Z )
  end function tanh_activation


  !/ =====================================================================================
  pure function D_tanh_activation( a, Z ) result( dadZ )
    !/ -----------------------------------------------------------------------------------
    !! First derivative of the non-linear activation function,
    !! with respect to the weighted sum of the inputs.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: a    !! non-linear activation of Z.
    real(dp), intent(in) :: Z    !! weighted sum of the inputs.
    real(dp)             :: dadZ !! first derivative of a with respect to Z.
    !/ -----------------------------------------------------------------------------------
    dadZ = 1.0d0 - (a*a)
  end function D_tanh_activation





  !/ =====================================================================================
  pure function linear_activation( Z ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Forward non-linear activation function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: Z  !! weighted sum of the inputs.
    real(dp)             :: a  !! non-linear activation of Z.
    !/ -----------------------------------------------------------------------------------
    a = Z
  end function linear_activation


  !/ =====================================================================================
  pure function D_linear_activation( a, Z ) result( dadZ )
    !/ -----------------------------------------------------------------------------------
    !! First derivative of the non-linear activation function,
    !! with respect to the weighted sum of the inputs.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: a    !! non-linear activation of Z.
    real(dp), intent(in) :: Z    !! weighted sum of the inputs.
    real(dp)             :: dadZ !! first derivative of a with respect to Z.
    !/ -----------------------------------------------------------------------------------
    dadZ = 1.0d0
  end function D_linear_activation








  !/ =====================================================================================
  function get_activation_pointer( name ) result( f_ptr )
    !/ -----------------------------------------------------------------------------------
    !! Look up an activation function by name
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(1), intent(in)             :: name
    procedure(ActivateFunction), pointer :: f_ptr
    !/ -----------------------------------------------------------------------------------
    !! sigma, tanh, relu, linear
    !/ -----------------------------------------------------------------------------------
    f_ptr => null()

    if ( ( 'S'.eq.name ).or.( 's'.eq.name ) ) then
       f_ptr => sigmoid_activation
       goto 999
    end if

    if ( ( 'T'.eq.name ).or.( 't'.eq.name ) ) then
       f_ptr => tanh_activation
       goto 999
    end if

    if ( ( 'R'.eq.name ).or.( 'r'.eq.name ) ) then
       call log_warn('ReLU not yet implemented, falling back to sigmoid')
       f_ptr => sigmoid_activation
       goto 999
    end if

    if ( ( 'L'.eq.name ).or.( 'l'.eq.name ) ) then
       f_ptr => linear_activation
       goto 999
    end if

    call log_error( 'No activation was specified' )

999 continue
  end function get_activation_pointer


  !/ =====================================================================================
  function get_D_activation_pointer( name ) result( f_ptr )
    !/ -----------------------------------------------------------------------------------
    !! Look up a derivative of an activation function by name
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(1), intent(in)      :: name
    procedure(DActivateFunction), pointer :: f_ptr
    !/ -----------------------------------------------------------------------------------
    !! sigma, tanh, relu, linear
    !/ -----------------------------------------------------------------------------------
    f_ptr => null()

    if ( ( 'S'.eq.name ).or.( 's'.eq.name ) ) then
       f_ptr => D_sigmoid_activation
       goto 999
    end if

    if ( ( 'T'.eq.name ).or.( 't'.eq.name ) ) then
       f_ptr => D_tanh_activation
       goto 999
    end if

    if ( ( 'R'.eq.name ).or.( 'r'.eq.name ) ) then
       call log_warn('ReLU not yet implemented, falling back to sigmoid')
       f_ptr => D_sigmoid_activation
       goto 999
    end if

    if ( ( 'L'.eq.name ).or.( 'l'.eq.name ) ) then
       f_ptr => D_linear_activation
       goto 999
    end if

    call log_error( 'No derivative activation was specified' )

999 continue
  end function get_D_activation_pointer


end module FFNN_Activation_mod


!/ =======================================================================================
!/ **                       F F N N _ A C T I V A T I O N _ M O D                       **
!/ ======================================================================== END FILE =====
