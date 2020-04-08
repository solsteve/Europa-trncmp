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
  use file_tools
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

     procedure :: read       => N_read_weights
     procedure :: write      => N_write_weights

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
    !! Execute a forward pass through this network.
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
    !! Reset the weight and bias delta's to zero
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
    !! Exexuting a trainiong pass backwards through this network. Deltas will be adjusted.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFNN),        intent(inout) :: dts        !! reference to this FFNN.
    real(dp),           intent(in)    :: input(:)   !! input vector.
    real(dp),           intent(in)    :: desired(:) !! desired output vector.
    real(dp), optional, intent(out)   :: MSE        !! mean square error error between the
    !!                                                 the network output and the desired.
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
    !! Adjust the weights and bias using the deltas.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFNN), intent(inout) :: dts !! reference to this FFNN.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    do i=1,dts%num_layer
       call dts%layer(i)%update
    end do

    !call dts%reset

  end subroutine N_apply_deltas





  
  !/ =====================================================================================
     subroutine N_read_weights( dts, FILE, UNIT, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFNN),            intent(inout) :: dts    !! reference to this FFNN.
    character(*), optional, intent(in)    :: FILE   !! path to an new or existing file.
    integer,      optional, intent(in)    :: UNIT   !! file unit for an open unit
    integer,      optional, intent(out)   :: IOSTAT !! error return.
    !/ -----------------------------------------------------------------------------------
    integer       :: inf, ios, nin, nlyr, nn, i
    real(dp)      :: a
    logical       :: report
    character(32) :: sig_name
    !/ -----------------------------------------------------------------------------------

    ios = 0
    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    inf = ReadUnit( FILE=FILE, UNIT=UNIT, IOSTAT=ios )
    
    if ( 0.ne.ios ) then
       if ( report ) then
          call log_error( 'Cannot open file for reading', STR=file )
       end if
       goto 999
    end if

    !/ -----------------------------------------------------------------------------------

    read( inf, * ) nin, nlyr

    call dts%build( nin, nlyr )

    do i=1,nlyr
       read( inf, * ) nn, a, sig_name
       call dts%setupLayer( i, nn, ALPHA=a, ACTIVATE=trim(adjustl(sig_name)) )
       call dts%layer(i)%read( inf )
    end do
    
    !/ -----------------------------------------------------------------------------------

    close( inf )

999 continue

    if ( present( IOSTAT ) ) IOSTAT = ios

  end subroutine N_read_weights


  !/ =====================================================================================
       subroutine N_write_weights( dts, FILE, UNIT, IOSTAT, FMT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FFNN),            intent(inout) :: dts    !! reference to this FFNN.
    character(*), optional, intent(in)    :: FILE   !! path to an new or existing file.
    integer,      optional, intent(in)    :: UNIT   !! file unit for an open unit
    integer,      optional, intent(out)   :: IOSTAT !! error return.
    character(*), optional, intent(in)    :: FMT    !! edit descriptor for weight.
    !/ -----------------------------------------------------------------------------------
    integer                   :: outf, ios, i
    logical                   :: report
    character(:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------
    
    ios = 0
    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    sfmt = 'ES15.8'
    if ( present( FMT ) ) then
       sfmt = FMT
    end if
    
    outf = WriteUnit( FILE=FILE, UNIT=UNIT, IOSTAT=ios )
    
    if ( 0.ne.ios ) then
       if ( report ) then
          call log_error( 'Cannot open file for writting', STR=file )
       end if
       goto 999
    end if
    
    !/ -----------------------------------------------------------------------------------

    write( outf, 100 ) dts%num_input, dts%num_layer

    do i=1,dts%num_layer
       write( outf, 110 ) dts%layer(i)%num_node, dts%layer(i)%alpha, dts%layer(i)%activate_name
       call dts%layer(i)%write( outf, SFMT )
    end do

    !/ -----------------------------------------------------------------------------------

    close( outf )

999 continue

    if ( present( IOSTAT ) ) IOSTAT = ios

100 format( I0,1X,I0 )
110 format( I0,1X,F10.8,1X,A)

  end subroutine N_write_weights

     

end module FFNN_Network_mod


!/ =======================================================================================
!/ **                          F F N N _ N E T W O R K _ M O D                          **
!/ ======================================================================== END FILE =====
