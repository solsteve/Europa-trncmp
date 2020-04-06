!/ ====================================================================== BEGIN FILE =====
!/ **                          F F N N _ N E T W O R K _ M O D                          **
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
module FFNN_Network_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a class for a feed forward neural network.
  !!   This NN has been evolving since grad school at NC A&T SU.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-02
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use dice_mod
  use tlogger
  use ffnn_layer_mod
  use ffnn_activation_mod
  implicit none


  !/ =====================================================================================
  type :: FFNN
     !/ ----------------------------------------------------------------------------------

     integer                    :: num_input           !! number of connections in the first layer
     integer                    :: num_output          !! number of nodes in the last layer
     integer                    :: num_layer           !! number of layers
     type(FFLayer), allocatable :: layer(:)            !! list of layer references

     real(dp)                   :: default_alpha       !! default training constant
     character(:),  allocatable :: default_activation  !! default activation function

   contains

     procedure :: build      => N_build
     procedure :: delete     => N_delete
     procedure :: setupLayer => N_setup_layer
     procedure :: init       => N_init_weights

     procedure :: predict    => N_execute_forward_pass
     procedure :: reset      => N_reset_deltas
     procedure :: train      => N_execute_reverse_pass
     procedure :: update     => N_apply_deltas

     final :: N_destroy_network
     
  end type FFNN



  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  subroutine N_destroy_network( net )
    !/ -----------------------------------------------------------------------------------
    !! Release allocations
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FFNN), intent(inout) :: net !! reference to an FFNN.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    do i=1,net%num_layer
       call net%layer(i)%delete
    end do
    
    if ( allocated( net%layer ) )              deallocate( net%layer )
    if ( allocated( net%default_activation ) ) deallocate( net%default_activation )

    net%num_input     = 0
    net%num_output    = 0
    net%num_layer     = 0
    net%default_alpha = D_ZERO
    
  end subroutine N_destroy_network
  
    

  !/ =====================================================================================
  subroutine N_delete( dts )
    !/ -----------------------------------------------------------------------------------
    !! Manual destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFNN), intent(inout) :: dts !! reference to this FFNN.
    !/ -----------------------------------------------------------------------------------
    call N_destroy_network( dts )
  end subroutine N_delete
    

  !/ =====================================================================================
  subroutine N_build( dts, num_input, num_layer, ALPHA, ACTIVATE )
    !/ -----------------------------------------------------------------------------------
    !! Setup the size of this FFNN.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFNN),            intent(inout) :: dts       !! reference to this FFNN.
    integer,                intent(in)    :: num_input !! number of inputs to this FFNN
    integer,                intent(in)    :: num_layer !! number of weight matrices in this FFNN
    real(dp),     optional, intent(in)    :: ALPHA     !! network wide default training rate
    character(*), optional, intent(in)    :: ACTIVATE  !!  network wide default activation function
    !/ -----------------------------------------------------------------------------------

    if ( present( ALPHA ) ) then
       dts%default_alpha = ALPHA
    else
       dts%default_alpha =  DEFAULT_TRAINING_RATE
    end if

    if ( present( ACTIVATE ) ) then
       dts%default_activation = trim(adjustl(ACTIVATE))
    else
        dts%default_activation = DEFAULT_ACTIVATION_FUNCTION
    end if

    dts%num_input = num_input
    dts%num_layer = num_layer
    allocate( dts%layer(dts%num_layer) )
    
  end subroutine N_build


  !/ =====================================================================================
  subroutine N_setup_layer( dts, index, nnode, ALPHA, ACTIVATE )
  !/ -----------------------------------------------------------------------------------
  !! Configure an indexed FFlayer.
  !/ -----------------------------------------------------------------------------------
  implicit none
  class(FFNN),            intent(inout) :: dts      !! reference to this FFNN.
  integer,                intent(in)    :: index    !! index of the 
  integer,                intent(in)    :: nnode    !! number of nodes in this layer
  real(dp),     optional, intent(in)    :: ALPHA    !! training rate for this node
  character(*), optional, intent(in)    :: ACTIVATE !! activation function
  !/ -----------------------------------------------------------------------------------
  integer                   :: ncon
  real(dp)                  :: this_alpha
  character(:), allocatable :: this_activate
  !/ -----------------------------------------------------------------------------------

  if ( 1.eq.index ) then
     ncon = dts%num_input
  else
     ncon = dts%layer(index-1)%num_node
  end if

  if ( 0.eq.ncon ) then
     call log_error( 'This layer is being built out of order. Layer', I4=index )
  else

     if ( present( ALPHA ) ) then
        this_alpha = ALPHA
     else
        this_alpha = dts%default_alpha
     end if
     
     if ( present( ACTIVATE ) ) then
        this_activate = ACTIVATE
     else
        this_activate = dts%default_activation
     end if

     call dts%layer(index)%build( ncon, nnode, this_alpha, this_activate )

  end if
  
end subroutine N_setup_layer


!/ =====================================================================================
subroutine N_init_weights( dts )
!/ -----------------------------------------------------------------------------------
!! initialize the weights and bias of all layers.
!/ -----------------------------------------------------------------------------------
implicit none
class(FFNN), intent(inout) :: dts !! reference to this FFNN.
!/ -----------------------------------------------------------------------------------
integer :: i
!/ -----------------------------------------------------------------------------------

do i=1,dts%num_layer
   call dts%layer(i)%init
end do

end subroutine N_init_weights


!/ =====================================================================================
subroutine N_execute_forward_pass( dts, input, OUTPUT )
  !/ -----------------------------------------------------------------------------------
  !! 
  !/ -----------------------------------------------------------------------------------
  implicit none
  class(FFNN),        intent(inout) :: dts       !! reference to this FFNN.
  real(dp),           intent(in)    :: input(:)  !! input vector.
  real(dp), optional, intent(out)   :: OUTPUT(:) !! output vector.
  !/ -----------------------------------------------------------------------------------
  integer :: i
  !/ -----------------------------------------------------------------------------------
  call dts%layer(1)%propagate_forward( input )
  if ( 1.lt.dts%num_layer ) then
     do i=2,dts%num_layer
        call dts%layer(i)%propagate_forward( dts%layer(i-1)%a )
     end do
  end if
  if ( present( OUTPUT ) ) then
     call copy( output, dts%layer(dts%num_layer)%a )
  end if
end subroutine N_execute_forward_pass


!/ =====================================================================================
subroutine N_reset_deltas( dts )
  !/ -----------------------------------------------------------------------------------
  !! 
  !/ -----------------------------------------------------------------------------------
  implicit none
  class(FFNN), intent(inout) :: dts !! reference to this FFNN.
  !/ -----------------------------------------------------------------------------------
  integer :: i
  !/ -----------------------------------------------------------------------------------
  do i=1,dts%num_layer
     call dts%layer(i)%reset
  end do
end subroutine N_reset_deltas


!/ =====================================================================================
subroutine N_execute_reverse_pass( dts, input, desired, MSE )
  !/ -----------------------------------------------------------------------------------
  !! 
  !/ -----------------------------------------------------------------------------------
  implicit none
  class(FFNN),        intent(inout) :: dts        !! reference to this FFNN.
  real(dp),           intent(in)    :: input(:)
  real(dp),           intent(in)    :: desired(:)
  real(dp), optional, intent(out)   :: MSE
  !/ -----------------------------------------------------------------------------------
  integer  :: i, nout
  real(dp) :: s, d
  !/ -----------------------------------------------------------------------------------

  nout = size(desired)
  
  call dts%predict( input ) ! no need for a copy of the output

  do concurrent ( i=1:nout )
     dts%layer(dts%num_layer)%d(i) = dts%layer(dts%num_layer)%a(i) - desired(i)
  end do

  if ( 1.lt.dts%num_layer ) then
     do i=dts%num_layer,2,-1
        call dts%layer(i)%propagate_backward( dts%layer(i-1)%a, DELTA=dts%layer(i-1)%d )
     end do
  end if

  call dts%layer(1)%propagate_backward( input )

  if ( present( MSE ) ) then
     s = D_ZERO
     do i=1,nout
        d = dts%layer(dts%num_layer)%a(i) - desired(i)
        s = s + (d*d)
     end do
     MSE = s / real(nout,dp)
  end if
  
end subroutine N_execute_reverse_pass


!/ =====================================================================================
subroutine N_apply_deltas( dts )
  !/ -----------------------------------------------------------------------------------
  !! 
  !/ -----------------------------------------------------------------------------------
  implicit none
  class(FFNN), intent(inout) :: dts !! reference to this FFNN.
  !/ -----------------------------------------------------------------------------------
  integer :: i
  !/ -----------------------------------------------------------------------------------
  do i=1,dts%num_layer
     call dts%layer(i)%update
  end do
end subroutine N_apply_deltas
 

end module FFNN_Network_mod


!/ =======================================================================================
!/ **                          F F N N _ N E T W O R K _ M O D                          **
!/ ======================================================================== END FILE =====
