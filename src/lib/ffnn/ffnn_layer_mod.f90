!/ ====================================================================== BEGIN FILE =====
!/ **                            F F N N _ L A Y E R _ M O D                            **
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
module FFNN_Layer_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides activation functions and their first derivatives.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-02
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use dice_mod
  use ffnn_activation_mod
  implicit none

  character(*), parameter :: DEFAULT_ACTIVATION_FUNCTION = 'sigma'
  real(dp),     parameter :: DEFAULT_TRAINING_RATE       = 1.0d-1

  type FFLayer

     integer  :: num_connect = 0      !! number of connections from the previous FFLayer.
     integer  :: num_node    = 0      !! number of nodes in this FFLayer.
     real(dp) :: alpha       = D_ZERO !! training rate

     real(dp), allocatable :: W(:,:)    !! weights
     real(dp), allocatable :: b(:)      !! bias
     real(dp), allocatable :: Z(:)      !! weighted sum of the inputs
     real(dp), allocatable :: a(:)      !! non-linear activation of the sum
     real(dp), allocatable :: E(:)      !! error
     real(dp), allocatable :: g(:)      !! gradient
     real(dp), allocatable :: dW(:,:)   !! delta weights
     real(dp), allocatable :: db(:)     !! delta bias
     real(dp), allocatable :: d(:)      !! difference applied to the previous layer

     !! non-linear activation function
     procedure(ActivateFunction),  pointer, nopass :: Sigma

     !! first derivative of the activation function
     procedure(DActivateFunction), pointer, nopass :: DSigma

     character(:), allocatable :: activate_name


     type(Dice) :: dd  !! instance of an entropy source
     
   contains

     procedure :: build               => L_build
     procedure :: delete              => L_delete
     procedure :: load                => L_load_weights
     procedure :: save                => L_save_weights
     procedure :: init                => L_initialize_weights
     procedure :: reset               => L_reset_deltas
     procedure :: propagate_forward   => L_execute_forward_pass
     procedure :: propagate_backward  => L_execute_backwards_pass
     procedure :: update              => L_update_weights

     procedure :: read                => L_read_weights
     procedure :: write               => L_write_weights
     

     final :: L_destroy_layer

  end type FFLayer

  


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  subroutine L_destroy_layer( layer )
    !/ -----------------------------------------------------------------------------------
    !! Free all of the allocation for an FFLayer object.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FFLayer), intent(inout) :: layer !! reference to an FFLayer
    !/ -----------------------------------------------------------------------------------

    if ( allocated( layer%Z ) ) then
       deallocate( layer%W )
       deallocate( layer%b )
       deallocate( layer%Z )
       deallocate( layer%a )
       deallocate( layer%E )
       deallocate( layer%g )
       deallocate( layer%dW )
       deallocate( layer%db )
       deallocate( layer%d )
    end if

    layer%num_connect = 0
    layer%num_node    = 0
    layer%alpha       = D_ZERO

    layer%Sigma  => null()
    layer%DSigma => null()

  end subroutine L_destroy_layer


  
  !/ =====================================================================================
  subroutine L_delete( dts )
    !/ -----------------------------------------------------------------------------------
    !! Build this FFLayer.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts !! reference to this FFLayer.
    !/ -----------------------------------------------------------------------------------
    call L_destroy_layer( dts )
  end subroutine L_delete

  
  !/ =====================================================================================
  subroutine L_build( dts, ncon, nnod, ALPHA, ACTIVATE )
    !/ -----------------------------------------------------------------------------------
    !! Build this FFLayer.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer),         intent(inout) :: dts      !! reference to this FFLayer.
    integer,                intent(in)    :: ncon     !! number of prior connections.
    integer,                intent(in)    :: nnod     !! number of nodes in this FFLayer.
    real(dp),     optional, intent(in)    :: ALPHA    !! training constant
    character(*), optional, intent(in)    :: ACTIVATE !! name of the activation function
    !/ -----------------------------------------------------------------------------------

    call dts%dd%seed_set

    if ( present( ALPHA ) ) then
       dts%alpha = ALPHA
    else
       dts%alpha = DEFAULT_TRAINING_RATE
    end if

    if ( present( ACTIVATE ) ) then
       dts%activate_name = trim( adjustl( ACTIVATE ) )
    else
       dts%activate_name = DEFAULT_ACTIVATION_FUNCTION
    end if
    
    if ( 0.lt.dts%num_node ) then
       call L_destroy_layer( dts )
    end if

    dts%num_connect = ncon
    dts%num_node    = nnod

    allocate( dts%W(ncon,nnod) )
    allocate( dts%b(nnod) )
    allocate( dts%Z(nnod) )
    allocate( dts%a(nnod) )
    allocate( dts%E(nnod) )
    allocate( dts%g(nnod) )
    allocate( dts%dW(ncon,nnod) )
    allocate( dts%db(nnod) )
    allocate( dts%d(nnod) )

    dts%Sigma  => getActivation(  dts%activate_name )
    dts%DSigma => getDActivation( dts%activate_name )

  end subroutine L_build

  !/ =====================================================================================
  subroutine L_load_weights( dts, src_W, src_b )
    !/ -----------------------------------------------------------------------------------
    !! Load the weights and bias from a source matrix and vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts !! reference to this FFLayer.
    real(dp),       intent(in)    :: src_W(:,:) !! source weight matrix.
    real(dp),       intent(in)    :: src_b(:) !! source bias vector.
    !/ -----------------------------------------------------------------------------------
    integer :: c,n,nc,nn
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    do concurrent( c=1:nc, n=1:nn )
       dts%W(c,n) = src_W(c,n)
    end do
    
    do concurrent( n=1:nn )
       dts%b(n) = src_b(n)
    end do
    
  end subroutine L_load_weights

  !/ =====================================================================================
  subroutine L_save_weights( dts, dst_W, dst_b )
    !/ -----------------------------------------------------------------------------------
    !! Store the weights and bias in a destination matrix and vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts      !! reference to this FFLayer.
    real(dp),       intent(out)   :: dst_W(:,:) !! destination weight matrix.
    real(dp),       intent(out)   :: dst_b(:) !! destination bias vector.
    !/ -----------------------------------------------------------------------------------
    integer :: c,n,nc,nn
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    do concurrent( c=1:nc, n=1:nn )
       dst_W(c,n) = dts%W(c,n)
    end do
    
    do concurrent( n=1:nn )
       dst_b(n) = dts%b(n)
    end do
    
  end subroutine L_save_weights

  !/ =====================================================================================
  subroutine L_initialize_weights( dts )
    !/ -----------------------------------------------------------------------------------
    !! Randomize the weights and bias.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts !! reference to this FFLayer.
    !/ -----------------------------------------------------------------------------------
    integer  :: c,n,nc,nn
    real(dp) :: s1, s2
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    s1 = sqrt( D_TWO / real(nn,dp) )
    s2 = sqrt( D_TWO / real(nn+nc,dp) )
    
    do n=1,nn
       dts%b(n) = s1 * dts%dd%normal()
       do c=1,nc
          dts%W(c,n) = s2 * dts%dd%normal()
       end do
    end do
    
  end subroutine L_initialize_weights

  !/ =====================================================================================
  subroutine L_reset_deltas( dts )
    !/ -----------------------------------------------------------------------------------
    !! Reset the delta weight and bias to zero.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts !! reference to this FFLayer.
    !/ -----------------------------------------------------------------------------------
    integer :: c,n,nc,nn
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    do concurrent( c=1:nc, n=1:nn )
       dts%dW(c,n) = D_ZERO
    end do
    
    do concurrent( n=1:nn )
       dts%db(n) = D_ZERO
       dts%d(n)  = D_ZERO
    end do

  end subroutine L_reset_deltas

  !/ =====================================================================================
  subroutine L_execute_forward_pass( dts, input )
    !/ -----------------------------------------------------------------------------------
    !! Execute a single forward predictive pass.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts        !! reference to this FFLayer.
    real(dp),       intent(in)    :: input(:)   !! sample input.
    !/ -----------------------------------------------------------------------------------
    integer  :: c,n,nc,nn
    real(dp) :: s
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    do n=1,nn
       s = dts%b(n)
       do c=1,nc
          s = s + ( input(c) * dts%W(c,n) )
       end do
       dts%Z(n) = s
       dts%a(n) = dts%Sigma( s )
    end do
    
  end subroutine L_execute_forward_pass


  !/ =====================================================================================
  subroutine L_execute_backwards_pass( dts, ain, DELTA )
    !/ -----------------------------------------------------------------------------------
    !! Execute a single backwards step. First execute a forward step.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout)   :: dts      !! reference to this FFLayer.
    real(dp), intent(in)            :: ain(:)   !! activated input from previous layer.
    real(dp), optional, intent(out) :: DELTA(:) !! difference applied to the previous layer.
    !/ -----------------------------------------------------------------------------------
    integer  :: c,n,nc,nn
    real(dp) :: s
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    do concurrent( n=1:nn )
       dts%g(n) = dts%DSigma( dts%a(n), dts%Z(n) )
       dts%E(n) = dts%d(n) * dts%g(n)
    end do

    if ( present( DELTA ) ) then
       do c=1,nc
          s = D_ZERO
          do n=1,nn
             s = s + ( dts%E(n) * dts%W(c,n) )
          end do
          DELTA(c) = s
       end do
    end if

    do concurrent( c=1:nc, n=1:nn )
       dts%dW(c,n) = dts%dW(c,n) + ( ain(c) * dts%E(n) )
    end do

    do concurrent( n=1:nn )
       dts%db(n) = dts%db(n) + dts%E(n)
    end do

  end subroutine L_execute_backwards_pass

  !/ =====================================================================================
  subroutine L_update_weights( dts )
    !/ -----------------------------------------------------------------------------------
    !! Update the weights with the weight deltas, then reset.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts !! reference to this FFLayer.
    !/ -----------------------------------------------------------------------------------
    integer :: c,n,nc,nn
    !/ -----------------------------------------------------------------------------------
    nc = size( dts%W, DIM=1 )
    nn = size( dts%W, DIM=2 )

    do concurrent( c=1:nc, n=1:nn )
       dts%W(c,n) = dts%W(c,n) - ( dts%alpha * dts%dW(c,n) )
    end do
    
    do concurrent( n=1:nn )
       dts%b(n) = dts%b(n) - ( dts%alpha * dts%db(n) )
    end do

  end subroutine L_update_weights
  

  !/ =====================================================================================
  subroutine L_read_weights( dts, unit )
    !/ -----------------------------------------------------------------------------------
    !! Read the weights and bias from an open file.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts  !! reference to this FFLayer.
    integer,        intent(in)    :: unit !! file unit number.
    !/ -----------------------------------------------------------------------------------
    integer :: c, n
    !/ -----------------------------------------------------------------------------------

    do n=1,dts%num_node
       read( unit, * ) dts%b(n)
       do c=1,dts%num_connect
          read( unit, * ) dts%W(c,n)
       end do
    end do

  end subroutine L_read_weights
  
    
  !/ =====================================================================================
    subroutine L_write_weights( dts, unit, fmt )
    !/ -----------------------------------------------------------------------------------
    !! Write the weights and bias to an open file.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFLayer), intent(inout) :: dts  !! reference to this FFLayer.
    integer,        intent(in)    :: unit !! file unit number.
    character(*),   intent(in)    :: fmt  !! edit descriptor for weight.
    !/ -----------------------------------------------------------------------------------
    integer :: c, n
    character(:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    sfmt = '(' // FMT // ')'
    
    do n=1,dts%num_node
       write( unit, sfmt ) dts%b(n)
       do c=1,dts%num_connect
          write( unit, sfmt ) dts%W(c,n)
       end do
    end do
    
  end subroutine L_write_weights
  
      
end module FFNN_Layer_mod


!/ =======================================================================================
!/ **                            F F N N _ L A Y E R _ M O D                            **
!/ ======================================================================== END FILE =====
