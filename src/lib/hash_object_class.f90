!/ ====================================================================== BEGIN FILE =====
!/ **                         H A S H _ O B J E C T _ C L A S S                         **
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
module hash_object_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-03
  !! license: GPL
  !!
  !! Provides a simple hash map of unlimited polymorphic objects.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use btree_object_class
  implicit none
  private

  
  !/ =====================================================================================
  type, public :: HashChain
     !/ ----------------------------------------------------------------------------------
     class(BTree), pointer :: chain => null()
  end type HashChain

  
  !/ =====================================================================================
  type, public :: HashMap
     !/ ----------------------------------------------------------------------------------

     type(BTree), allocatable, dimension(:) :: map
     integer :: num_chain = 0

   contains

     procedure, public :: set  => hashmap_set
     procedure, public :: get  => hashmap_get

     final :: hashmap_destroy
  end type HashMap


  !/ -------------------------------------------------------------------------------------
  interface hash
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_hash
  end interface hash

  public :: hash
  

  !/ -------------------------------------------------------------------------------------
  interface create
     !/ ----------------------------------------------------------------------------------
     module procedure :: hashmap_create
  end interface create
  
  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  !/ =====================================================================================
  subroutine hashmap_create( H, alloc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(HashMap),     intent(inout) :: H
    integer, optional, intent(in)    :: alloc
    !/ -----------------------------------------------------------------------------------

    H%num_chain = 16384
    if ( present( alloc ) ) then
       H%num_chain = alloc
    end if

    allocate( H%map(H%num_chain) )

  end subroutine hashmap_create

  !/ =====================================================================================
  subroutine hashmap_destroy( H )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(HashMap),     intent(inout) :: H
    !/ -----------------------------------------------------------------------------------
  end subroutine hashmap_destroy

  !/ =====================================================================================
  pure function string_to_hash( key, mod ) result( H )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: key  !! string key to hash.
    integer,      intent(in) :: mod  !! modulo to aply to the hash.
    integer                  :: H    !! return hash.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    
    H = 5381

    do i=1,len(key)
       H = ( ishft(H,5) + H ) + ichar( key(i:i) )
    end do
    H = modulo( H, mod )

  end function string_to_hash


  !/ =====================================================================================
  pure function integer_to_hash( key, mod ) result( H )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: key  !! integer key to hash.
    integer, intent(in) :: mod  !! modulo to aply to the hash.
    integer             :: H    !! return hash.
    !/ -----------------------------------------------------------------------------------

    H = modulo( key, mod )

  end function integer_to_hash


  !/ =====================================================================================
  pure function single_to_hash( key, mod ) result( H )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), intent(in) :: key  !! single precision key to hash.
    integer,  intent(in) :: mod  !! modulo to aply to the hash.
    integer              :: H    !! return hash.
    !/ -----------------------------------------------------------------------------------

    H = modulo( int( log(key) * 8.192e3 ), mod )

  end function single_to_hash


  !/ =====================================================================================
  pure function double_to_hash( key, mod ) result( H )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: key  !! single precision key to hash.
    integer,  intent(in) :: mod  !! modulo to aply to the hash.
    integer              :: H    !! return hash.
    !/ -----------------------------------------------------------------------------------

    H = modulo( int( log(key) * 3.2768d4 ), mod )

  end function double_to_hash


  !/ =====================================================================================
  pure function object_to_hash( key, mod ) result( H )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), intent(in) :: key  !! single precision key to hash.
    integer,  intent(in) :: mod  !! modulo to aply to the hash.
    integer              :: H    !! return hash.
    !/ -----------------------------------------------------------------------------------

    H = 1
    
    select type( key )
    type is( character(*) )
       H = string_to_hash( key, mod )
    type is( integer )
       H = integer_to_hash( key, mod )
    type is( real(sp) )
       H = single_to_hash( key, mod )
    type is( real(dp) )
       H = double_to_hash( key, mod )
    end select
    
  end function object_to_hash




  !/ =====================================================================================
  subroutine hashmap_set( self, key, obj )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap), intent(inout) :: self !! reference to this object
    class(*),       intent(in)    :: key  !! key to hash
    class(*),       intent(in)    :: obj  !! object to store
    !/ -----------------------------------------------------------------------------------

  end subroutine hashmap_set


  !/ =====================================================================================
  function hashmap_get( self, key, stat ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap),    intent(inout) :: self !! reference to this object
    class(*),          intent(in)    :: key  !! key to hash
    integer, optional, intent(out)   :: stat !! optional return status
    class(*), pointer                :: obj
    !/ -----------------------------------------------------------------------------------

  end function hashmap_get

  
end module hash_object_class

!/ =======================================================================================
!/ **                         H A S H _ O B J E C T _ C L A S S                         **
!/ =========================================================================== END FILE ==
