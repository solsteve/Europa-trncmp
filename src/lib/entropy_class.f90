!/ ====================================================================== BEGIN FILE =====
!/ **                             E N T R O P Y _ C L A S S                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
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
!/ =======================================================================================
module entropy_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-03-04
  !! license: GPL
  !!
  !! Provides a standardized source of entropy. This one is based on the built-in
  !! psuedo random number generator.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  
  public :: create
  public :: Entropy

  
   integer, parameter :: MAXINT_BUILTIN = 2147483647
   integer, parameter :: TABLE_SIZE     = 317

   
  !/ =====================================================================================
  type, public :: Entropy
     !/ ----------------------------------------------------------------------------------
     !! Instance state for the entropy engine. This engine wraps the (poor) internal
     !! random number generator that comes with GNU fortran.
     !!
     !! @todo
     !! Relace this engine with a Mersenne Twist 64 bit generator recovering at least
     !! 56 bits in order to create double precision numbers in the 0-1 range.
     !/ ----------------------------------------------------------------------------------

     real(dp), allocatable, dimension(:) :: table

   contains

     procedure, public :: reset      => entropy_reset
     procedure, public :: getInteger => entropy_get_I32
     procedure, public :: getReal    => entropy_get_R8
     
     procedure, public, nopass :: getMaxInteger => entropy_get_max_integer
     procedure, public, nopass :: seedBits      => entropy_seed_bits
     procedure, public, nopass :: seedSize      => entropy_seed_size

     procedure :: entropy_set_seed_void
     procedure :: entropy_set_seed_I08
     procedure :: entropy_set_seed_I16
     procedure :: entropy_set_seed_I32

     generic, public :: seedSet => entropy_set_seed_void, &
          &                        entropy_set_seed_I08,  &
          &                        entropy_set_seed_I16,  &
          &                        entropy_set_seed_I32

  end type Entropy

  
  interface create
     module procedure :: entropy_create
  end interface create

  
  interface Entropy
     module procedure :: entropy_alloc
  end interface Entropy


  

  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  subroutine entropy_create( ent )
    !/ -----------------------------------------------------------------------------------
    !! Initialize an Entropy class.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Entropy), intent(inout) :: ent !! reference to an allocated Entropy class.
    !/ -----------------------------------------------------------------------------------

    allocate( ent%table(TABLE_SIZE+1) )
    call ent%seedSet

  end subroutine entropy_create


  !/ =====================================================================================
  function entropy_alloc() result( ent )
    !/ -----------------------------------------------------------------------------------
    !! Create and initialize an Entropy class.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), pointer :: ent !! pointer to an newly allocated Entropy class.
    !/ -----------------------------------------------------------------------------------

    allocate( ent )
    call entropy_create( ent )

  end function entropy_alloc


  !/ =====================================================================================
  subroutine entropy_reset( self )
    !/ -----------------------------------------------------------------------------------
    !! Reset the internal tables.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: self !! self reference to this class.
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------

    do i=1,TABLE_SIZE+1
       call random_number(x)
       self%table(i) = x
     end do
       
  end subroutine entropy_reset

  
  !/ =====================================================================================
  function entropy_seed_bits( ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Return the number of seed bits per entropy element. 
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer :: n !! number of seed bits.
    !/ -----------------------------------------------------------------------------------
    integer :: example = 123
    !/ -----------------------------------------------------------------------------------

    n = bit_size(example)

  end function entropy_seed_bits

  
  !/ =====================================================================================
  function entropy_seed_size( ) result( n )
    !/ -----------------------------------------------------------------------------------
    !!  Return the number of integers required to establish a unique state.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer :: n !! number of seed elements.
    !/ -----------------------------------------------------------------------------------

    call random_seed( size=n )

  end function entropy_seed_size

  
  !/ =====================================================================================
  function entropy_get_max_integer( ) result( mx )
    !/ -----------------------------------------------------------------------------------
    !! Return the maximum integer generated by this psuedo random generator.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer :: mx !! the maximum integer.
    !/ -----------------------------------------------------------------------------------

    mx = MAXINT_BUILTIN

  end function entropy_get_max_integer

  

  !/ =====================================================================================
  subroutine entropy_set_seed_void( self )
    !/ -----------------------------------------------------------------------------------
    !! Set the seed from system entropy.
    !/ -----------------------------------------------------------------------------------
    use entropy_seed_source, only : urandom
    implicit none
    class(Entropy), intent(inout) :: self !! self reference to this class.
    !/ -----------------------------------------------------------------------------------
    integer, allocatable, dimension(:) :: seed
    !/ -----------------------------------------------------------------------------------
    
    allocate( seed(self%seedSize()) )

    call urandom( seed )

    call random_seed( put=seed )

    deallocate( seed )
    
    call self%reset
    
  end subroutine entropy_set_seed_void

  
  !/ =====================================================================================
  subroutine entropy_set_seed_I08( self, sm )
    !/ -----------------------------------------------------------------------------------
    !! Set the entropy state using the seed matter sm.
    !/ -----------------------------------------------------------------------------------
    use entropy_seed_source, only : mix_seed_matter
    use entropy_seed_source, only : bit_transfer
    implicit none
    class(Entropy),              intent(inout) :: self !! self reference to this class.
    integer(int8), dimension(:), intent(in)    :: sm   !! seed matter.
    !/ -----------------------------------------------------------------------------------
    integer(int32), allocatable, dimension(:) :: seed
    integer(int8),  allocatable, dimension(:) :: temp
    integer                                   :: n
    !/ -----------------------------------------------------------------------------------

    n = self%seedSize()
    
    allocate( temp(n*4) )
    allocate( seed(n) )

    call mix_seed_matter( temp, sm )
    call bit_transfer( seed, temp )
    
    call random_seed( put=seed  )
    
    deallocate( seed )
    deallocate( temp )

    call self%reset

  end subroutine entropy_set_seed_I08

  
  !/ =====================================================================================
  subroutine entropy_set_seed_I16( self, sm )
    !/ -----------------------------------------------------------------------------------
    !! Set the entropy state using the seed matter sm.
    !/ -----------------------------------------------------------------------------------
    use entropy_seed_source, only : mix_seed_matter
    use entropy_seed_source, only : bit_transfer
    implicit none
    class(Entropy),               intent(inout) :: self !! self reference to this class.
    integer(int16), dimension(:), intent(in)    :: sm   !! seed matter.
    !/ -----------------------------------------------------------------------------------
    integer(int32), allocatable, dimension(:) :: seed
    integer(int16), allocatable, dimension(:) :: temp
    integer                                   :: n
    !/ -----------------------------------------------------------------------------------

    n = self%seedSize()
    
    allocate( temp(n*2) )
    allocate( seed(n) )

    call mix_seed_matter( temp, sm )
    call bit_transfer( seed, temp )
    
    call random_seed( put=seed  )
    
    deallocate( seed )
    deallocate( temp )

    call self%reset

  end subroutine entropy_set_seed_I16

  
  !/ =====================================================================================
  subroutine entropy_set_seed_I32( self, sm )
    !/ -----------------------------------------------------------------------------------
    !! Set the entropy state using the seed matter sm.
    !/ -----------------------------------------------------------------------------------
    use entropy_seed_source, only : mix_seed_matter
    implicit none
    class(Entropy),               intent(inout) :: self !! self reference to this class.
    integer(int32), dimension(:), intent(in)    :: sm   !! seed matter.
    !/ -----------------------------------------------------------------------------------
    integer, allocatable, dimension(:) :: seed
    !/ -----------------------------------------------------------------------------------
    
    allocate( seed(self%seedSize()) )

    call mix_seed_matter( seed, sm )

    call random_seed( put=seed )
    
    deallocate( seed )

    call self%reset

  end subroutine entropy_set_seed_I32

  
  !/ =====================================================================================
  function entropy_get_I32( self ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return the next integer valued psuedo random number.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: self !! self reference to this class.
    integer                       :: x    !! psuedo random number.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: y
    !/ -----------------------------------------------------------------------------------

    y = self%getReal()

    x = int( y * real(MAXINT_BUILTIN, dp) )

  end function entropy_get_I32
  

  !/ =====================================================================================
  function entropy_get_R8( self ) result( x )
    !/ -----------------------------------------------------------------------------------
    !!  Return the next real valued psuedo random number 0 <= x < 1.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: self !! self reference to this class.
    real(dp)                      :: x    !! psuedo random number.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: y
    integer  :: i
    !/ -----------------------------------------------------------------------------------
    
    call random_number(y)
    i = int(y*TABLE_SIZE) + 1

    x = self%table(i)
    self%table(i) = y    

  end function entropy_get_R8

  
end module entropy_class


!/ =======================================================================================
!/ **                             E N T R O P Y _ C L A S S                             **
!/ =========================================================================== END FILE ==
