!/ ====================================================================== BEGIN FILE =====
!/ **                                B P N 2 _ C L A S S                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2017, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  This program is free software: you can redistribute it and/or modify it under    **
!/ **  the terms of the GNU General Public License as published by the Free Software    **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
!/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
!/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
!/ **                                                                                   **
!/ ----- Modification History ------------------------------------------------------------
!
!! author:  Stephen W. Soliday
!! date:    2017-07-02
!! license: GPL
!!
!!## Feed Forward Neural Network.
!!
!! Provides the interface and procedures for implementing a fully connected
!! multi-layer feed forward neural network.
!
!/ =======================================================================================
module bpn2_class
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  
  !/ =====================================================================================
  type BPN2
     !/ ----------------------------------------------------------------------------------
     integer  :: num_inp           !! number of inputs.
     integer  :: num_hid           !! number of hidden elements.
     integer  :: num_out           !! number of output elements.

     real(dp), pointer :: B1(:)    => null()  !! first  layer bias
     real(dp), pointer :: W1(:,:)  => null()  !! first  layer weights
     real(dp), pointer :: z1(:)    => null()  !! first  layer sums
     real(dp), pointer :: a1(:)    => null()  !! first  layer outputs
     real(dp), pointer :: d1(:)    => null()  !! first  layer errors
     real(dp), pointer :: dB1(:)   => null()  !! first  layer bias differential
     real(dp), pointer :: dW1(:,:) => null()  !! first  layer weights differential
     real(dp), pointer :: aB1(:)   => null()  !! first  layer bias accumulator
     real(dp), pointer :: aW1(:,:) => null()  !! first  layer weights accumulator

     real(dp), pointer :: B2(:)    => null()  !! second layer bias
     real(dp), pointer :: W2(:,:)  => null()  !! second layer weights
     real(dp), pointer :: z2(:)    => null()  !! second layer sums
     real(dp), pointer :: a2(:)    => null()  !! second layer outputs
     real(dp), pointer :: d2(:)    => null()  !! second layer errors
     real(dp), pointer :: dB2(:)   => null()  !! second layer bias differential
     real(dp), pointer :: dW2(:,:) => null()  !! second layer weights differential
     real(dp), pointer :: aB2(:)   => null()  !! second layer bias accumulator
     real(dp), pointer :: aW2(:,:) => null()  !! second layer weights accumulator

   contains

     procedure, private :: bpn_weight_init
     procedure, private :: bpn_reset
     procedure, private :: bpn_forward
     procedure, private :: bpn_backwards
     procedure, private :: bpn_update
     procedure, private :: bpn_write_weights
     procedure, private :: bpn_read_weights

     procedure, private :: bpn_set_size
     procedure, private :: bpn_set_size1
     procedure, private :: bpn_set_size2

     generic :: init      => bpn_weight_init
     generic :: reset     => bpn_reset
     generic :: forward   => bpn_forward
     generic :: backwards => bpn_backwards
     generic :: update    => bpn_update
     generic :: write     => bpn_write_weights
     generic :: read      => bpn_read_weights
     
     !final :: bpn_delete

  end type BPN2

  private :: bpn_create_params
  private :: bpn_create_read_file
  private :: bpn_allocate_params
  private :: bpn_allocate_read_file

  !/ -------------------------------------------------------------------------------------
  interface create
     !/ ----------------------------------------------------------------------------------
     module procedure :: bpn_create_params
     module procedure :: bpn_create_read_file
  end interface create

  !/ -------------------------------------------------------------------------------------
  interface BPN2
     !/ ----------------------------------------------------------------------------------
     module procedure :: bpn_allocate_params
     module procedure :: bpn_allocate_read_file
  end interface BPN2




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_set_size1( self, inp, hid )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2), intent(inout) :: self   !! reference to this two layer network.
    integer,     intent(in)    :: inp    !! number of inputs.         
    integer,     intent(in)    :: hid    !! number of hidden elements.
    !/ -----------------------------------------------------------------------------------
    self%num_inp = inp
    self%num_hid = hid

    allocate( self%B1(  self%num_hid ) )
    allocate( self%W1(  self%num_inp, self%num_hid ) )
    allocate( self%z1(  self%num_hid ) )
    allocate( self%a1(  self%num_hid ) )
    allocate( self%d1(  self%num_hid ) )
    allocate( self%dB1( self%num_hid ) )
    allocate( self%dW1( self%num_inp, self%num_hid ) )
    allocate( self%aB1( self%num_hid ) )
    allocate( self%aW1( self%num_inp, self%num_hid ) )

  end subroutine bpn_set_size1
  

  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_set_size2( self, hid, out )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2), intent(inout) :: self   !! reference to this two layer network.
    integer,     intent(in)    :: hid    !! number of hidden elements.
    integer,     intent(in)    :: out    !! number of output elements.
    !/ -----------------------------------------------------------------------------------
    self%num_hid = hid
    self%num_out = out

    allocate( self%B2(  self%num_out ) )
    allocate( self%W2(  self%num_hid, self%num_out ) )
    allocate( self%z2(  self%num_out ) )
    allocate( self%a2(  self%num_out ) )
    allocate( self%d2(  self%num_out ) )
    allocate( self%dB2( self%num_out ) )
    allocate( self%dW2( self%num_hid, self%num_out ) )
    allocate( self%aB2( self%num_out ) )
    allocate( self%aW2( self%num_hid, self%num_out ) )

  end subroutine bpn_set_size2

  
  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_set_size( self, inp, hid, out )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2), intent(inout) :: self   !! reference to this two layer network.
    integer,     intent(in)    :: inp    !! number of inputs.         
    integer,     intent(in)    :: hid    !! number of hidden elements.
    integer,     intent(in)    :: out    !! number of output elements.
    !/ -----------------------------------------------------------------------------------

    call self%bpn_set_size1( inp, hid )
    call self%bpn_set_size2(      hid, out )
    
  end subroutine bpn_set_size

  
  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_create_params( net, inp, hid, out )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(BPN2), intent(inout) :: net    !! reference to a two layer network.
    integer,    intent(in)    :: inp    !! number of inputs.         
    integer,    intent(in)    :: hid    !! number of hidden elements.
    integer,    intent(in)    :: out    !! number of output elements.
    !/ -----------------------------------------------------------------------------------

    call net%bpn_set_size( inp, hid, out )
    
  end subroutine bpn_create_params
  
  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_create_read_file( net, file )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(BPN2),   intent(inout) :: net    !! reference to a two layer network.
    character(*), intent(in)    :: file   !! configuration file
    !/ -----------------------------------------------------------------------------------

    call net%bpn_read_weights( FILE=file )
    
  end subroutine bpn_create_read_file
  
  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  function bpn_allocate_params( inp, hid, out ) result( net )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2), pointer   :: net    !! pointer to a two layer network.
    integer,    intent(in) :: inp    !! number of inputs.         
    integer,    intent(in) :: hid    !! number of hidden elements.
    integer,    intent(in) :: out    !! number of output elements.
    !/ -----------------------------------------------------------------------------------

    allocate( net )
    call bpn_create_params( net, inp, hid, out )

  end function bpn_allocate_params

  
  !/ =====================================================================================
  !! Constructor.
  !/ -------------------------------------------------------------------------------------
  function bpn_allocate_read_file( file ) result( net )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2), pointer     :: net    !! pointer to a two layer network.
    character(*), intent(in) :: file   !! configuration
    !/ -----------------------------------------------------------------------------------

    allocate( net )
    call bpn_create_read_file( net, file )

  end function bpn_allocate_read_file

  !/ =====================================================================================
  function gauss() result( x )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp) :: x
    real(dp) :: a
    integer  :: i
    x = 0.0d0
    do i=1,4
       call RANDOM_NUMBER( a )
       x = x + (2.0d0 * a - 1.0d0)
    end do
    if ( x .lt. -1.0d0 ) x = -1.0d0
    if ( x .gt.  1.0d0 ) x =  1.0d0

  end function gauss
    
  
  !/ =====================================================================================
  !! Weight Initialization.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_weight_init( self, eta )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2),        intent(inout) :: self  !! reference to this net.
    real(dp), optional, intent(in)    :: eta   !! reference to this net.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: s
    integer  :: i, j
    !/ -----------------------------------------------------------------------------------

    s = 0.36
    if ( present( eta ) ) then
       s  = eta
    end if
    
    do j=1,self%num_hid
       self%B1(j)  = s*gauss()
       self%dB1(j) = 0.0d0
       do i=1,self%num_inp
          self%W1(i,j)  = s*gauss()
          self%dW1(i,j) = 0.0d0
       end do
    end do
    
    do j=1,self%num_out
       self%B2(j)  = s*gauss()
       self%dB2(j) = 0.0d0
       do i=1,self%num_hid
          self%W2(i,j)  = s*gauss()
          self%dW2(i,j) = 0.0d0
       end do
    end do
    
  end subroutine bpn_weight_init


  !/ =====================================================================================
  !! Reset Network.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_reset( self )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2), intent(inout) :: self !! reference to this net.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------

    do j=1,self%num_hid
       self%z1(j)  = 0.0d0
       self%a1(j)  = 0.0d0
       self%d1(j)  = 0.0d0
       self%dB1(j) = 0.0d0
       self%aB1(j) = 0.0d0
       do i=1,self%num_inp
          self%dW1(i,j) = 0.0d0
          self%aW1(i,j) = 0.0d0
       end do
    end do

    do j=1,self%num_out
       self%z2(j)  = 0.0d0
       self%a2(j)  = 0.0d0
       self%d2(j)  = 0.0d0
       self%dB2(j) = 0.0d0
       self%aB2(j) = 0.0d0
       do i=1,self%num_hid
          self%dW2(i,j) = 0.0d0
          self%aW2(i,j) = 0.0d0
       end do
    end do

  end subroutine bpn_reset


  !/ =====================================================================================
  !! Forward Pass.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_forward( self, X, y )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2),        intent(inout) :: self !! reference to this net.
    real(dp),           intent(inout) :: X(:) !! input vector.
    real(dp), optional, intent(inout) :: y(:) !! optional output vector.
    !/ -----------------------------------------------------------------------------------
    integer  :: n, c, inp, hid, out
    real(dp) :: s
    !/ -----------------------------------------------------------------------------------
    inp = self%num_inp
    hid = self%num_hid
    out = self%num_out

    do n=1,hid
       s = self%B1(n)
       do c=1,inp
          s = s + self%W1(c,n) * X(c)
       end do
       self%z1(n) = s
       self%a1(n) = 1.0d0 / (1.0d0 + exp(-s))
    end do

    do n=1,out
       s = self%B2(n)
       do c=1,hid
          s = s + self%W2(c,n) * self%a1(c)
       end do
       self%z2(n) = s
       self%a2(n) = 1.0d0 / (1.0d0 + exp(-s))
    end do

    if ( present( y ) ) then
       do n=1,out
          y(n) = self%a2(n)
       end do
    end if
    
  end subroutine bpn_forward


  !/ =====================================================================================
  !! Backwards Pass.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_backwards( self, X, T, CST )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2),        intent(inout) :: self !! reference to this net.
    real(dp),           intent(inout) :: X(:) !! input vector.
    real(dp),           intent(inout) :: T(:) !! desired output vector.
    real(dp), optional, intent(out)   :: CST  !! optional error output
    !/ -----------------------------------------------------------------------------------
    integer  :: n, c, j, inp, hid, out
    real(dp) :: s, fout, cost, dif, temp
    !/ -----------------------------------------------------------------------------------
    inp = self%num_inp
    hid = self%num_hid
    out = self%num_out
    fout = real( out, dp )

    cost = 0.0d0

    do n=1,out

       dif = self%a2(n) - T(n)

       cost = cost + (dif*dif)

       self%d2(n)  = dif * self%a2(n) * ( 1.0d0 - self%a2(n) ) / fout

       self%dB2(n) = self%d2(n)
       self%aB2(n) = self%aB2(n) + self%d2(n)
       do c=1,hid
          temp = self%d2(n) * self%a1(c)
          self%dW2(c,n) = temp
          self%aW2(c,n) = self%aW2(c,n) + temp
       end do
    end do

    cost = cost / fout

    do n=1,hid
       s = 0.0d0
       do j=1,out
          s = s + ( self%d2(j) * self%W2(n,j) )
       end do
       self%d1(n) = s * self%a1(n) * ( 1.0d0 - self%a1(n) )

       self%dB1(n) = self%d1(n)
       self%aB1(n) = self%aB1(n) + self%d1(n)
       do c=1,inp
          temp = self%d1(n) * X(c)
          self%dW1(c,n) = temp
          self%aW1(c,n) = self%aW1(c,n) + temp
       end do
    end do

    if ( present( CST ) ) CST = cost

  end subroutine bpn_backwards


  !/ =====================================================================================
  !! Update the Weights.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_update( self, eta )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BPN2),        intent(inout) :: self !! reference to this net.
    real(dp), optional, intent(in)    :: eta  !! optional learning parameter.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: alf
    integer  :: n, c, inp, hid, out
    !/ -----------------------------------------------------------------------------------
    inp = self%num_inp
    hid = self%num_hid
    out = self%num_out

    alf = 1.0d0
    if ( present( eta ) ) alf = eta
    
    do n=1,hid
       self%B1(n) = self%B1(n) - alf*self%aB1(n)
       do c=1,inp
          self%W1(c,n) = self%W1(c,n) - alf*self%aW1(c,n)
       end do
    end do

    do n=1,out
       self%B2(n) = self%B2(n) - alf*self%aB2(n)
       do c=1,hid
          self%W2(c,n) = self%W2(c,n) - alf*self%aW2(c,n)
       end do
    end do

    call self%bpn_reset

  end subroutine bpn_update


  !/ =====================================================================================
  !! Write Weights.
  !! Write the structure and weights of this two layer neural network.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_write_weights( self, FILE, UNIT, IOSTAT, FMT )
    !/ -----------------------------------------------------------------------------------
    use file_tools, only : WriteUnit
    use tlogger,    only : log_error
    implicit none
    class(BPN2),            intent(inout) :: self   !! reference to this net.
    character(*), optional, intent(in)    :: FILE   !! optional path to an new or existing file.
    integer,      optional, intent(in)    :: UNIT   !! file unit for an open unit.
    integer,      optional, intent(out)   :: IOSTAT !! optional error return.
    character(*), optional, intent(in)    :: FMT    !! optional output format ( default 'ES15.8' )
    !/ -----------------------------------------------------------------------------------
    integer   :: outf, ios, i, j
    logical   :: report
    character(:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    ios = 0
    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    sfmt = '(ES15.8)'
    if ( present( FMT ) ) then
       sfmt = '(' // FMT // ')'
    end if
    
    outf = WriteUnit( FILE=FILE, UNIT=UNIT, IOSTAT=ios )
    
    if ( 0.ne.ios ) then
       if ( report ) then
          call log_error( 'Cannot open file for writting', STR=file )
       end if
       goto 999
    end if

    !/ -----------------------------------------------------------------------------------

    write (outf, '(I0)')       2
    write (outf, '(I0,1X,I0)') self%num_inp, self%num_hid

    do j=1,self%num_hid
       write (outf, sfmt) self%B1(j)
    end do

    do i=1,self%num_inp
       do j=1,self%num_hid
          write (outf, sfmt) self%W1(i,j)
       end do
    end do

    !/ -----------------------------------------------------------------------------------

    write (outf, '(I0,1X,I0)') self%num_hid, self%num_out

    do j=1,self%num_out
       write (outf, sfmt) self%B2(j)
    end do

    do i=1,self%num_hid
       do j=1,self%num_out
          write (outf, sfmt) self%W2(i,j)
       end do
    end do

    !/ -----------------------------------------------------------------------------------

    close( outf )

999 continue

    if ( present( IOSTAT ) ) IOSTAT = ios

  end subroutine bpn_write_weights














  !/ =====================================================================================
  !! Write Weights.
  !! Write the structure and weights of this two layer neural network.
  !/ -------------------------------------------------------------------------------------
  subroutine bpn_read_weights( self, FILE, UNIT, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    use file_tools, only : ReadUnit
    use tlogger,    only : log_error
    implicit none
    class(BPN2),            intent(inout) :: self   !! reference to this net.
    character(*), optional, intent(in)    :: FILE   !! optional path to an new or existing file.
    integer,      optional, intent(in)    :: UNIT   !! file unit for an open unit.
    integer,      optional, intent(out)   :: IOSTAT !! optional error return.
    !/ -----------------------------------------------------------------------------------
    integer   :: inf, ios, i, j, inp, hid, out, temp
    logical   :: report
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

    read ( inf, * ) temp

    if ( 2.ne.temp ) then
       if ( report ) then
          call log_error( 'Only 2 layer networks' )
       end if
       ios = 1
       goto 888
    end if
    
    read ( inf, * ) inp, hid

    call self%bpn_set_size1( inp, hid )

    do j=1,hid
       read (inf, *) self%B1(j)
    end do

    do i=1,inp
       do j=1,hid
          read (inf, *) self%W1(i,j)
       end do
    end do

    read ( inf, * ) hid, out

    call self%bpn_set_size2( hid, out )
    
    do j=1,out
       read (inf, *) self%B2(j)
    end do

    do i=1,hid
       do j=1,out
          read (inf, *) self%W2(i,j)
       end do
    end do

    !/ -----------------------------------------------------------------------------------

888 continue
    
    close( inf )

999 continue

    if ( present( IOSTAT ) ) IOSTAT = ios

  end subroutine bpn_read_weights
  

end module bpn2_class

!/ =======================================================================================
!/ **                                B P N 2 _ C L A S S                                **
!/ =========================================================================== END FILE ==
