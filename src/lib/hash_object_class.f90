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

  type, public :: HashChain
     class(BTree), pointer :: ptr => null()
  end type HashChain


  
  !/ =====================================================================================
  type, public :: HashMap
     !/ ----------------------------------------------------------------------------------

     type(HashChain), allocatable, dimension(:) :: chain
     integer                                    :: num_chain  = 0
     integer                                    :: num_items  = 0
     integer                                    :: item_count = 0

   contains

     procedure, public :: isEmpty => hashmap_is_empty
     procedure, public :: size    => hashmap_size
     procedure, public :: find    => hashmap_find
     procedure, public :: set     => hashmap_set
     procedure, public :: get     => hashmap_get

     ! built-in iterator

     procedure, public :: rewind   => hashmap_iter_rewind
     procedure, public :: hasNext  => hashmap_iter_has_next
     procedure, public :: next     => hashmap_iter_next
     procedure, public :: nextNode => hashmap_iter_next_node
     procedure, public :: nextKey  => hashmap_iter_next_key


     final :: hashmap_destroy
  end type HashMap


  !/ -------------------------------------------------------------------------------------
  interface hash
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_hash
  end interface hash

  

  !/ -------------------------------------------------------------------------------------
  interface create
     !/ ----------------------------------------------------------------------------------
     module procedure :: hashmap_create
  end interface create
  
  public :: hash
  public :: create
  
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

    allocate( H%chain(H%num_chain) )

  end subroutine hashmap_create

  
  !/ =====================================================================================
  subroutine hashmap_destroy( H )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(HashMap),     intent(inout) :: H
    !/ -----------------------------------------------------------------------------------
  end subroutine hashmap_destroy




  !/ =====================================================================================
  pure function hashmap_is_empty( self ) result( stat )
    !/ -----------------------------------------------------------------------------------
    !! Determine if the hash map is empty.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap), intent(in) :: self !! reference to this hash map.
    logical                    :: stat !! true if the tree is empty.
    !/ -----------------------------------------------------------------------------------

    stat = ( 0 .eq. self%item_count )

  end function hashmap_is_empty


  !/ =====================================================================================
  pure function hashmap_size( self ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of objects stored in this hash map.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap), intent(in) :: self !! reference to this hash map.
    integer                    :: n    !! number of stored objects.
    !/ -----------------------------------------------------------------------------------

    n = self%item_count

  end function hashmap_size




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
    H = modulo( H, mod ) + 1

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

    H = modulo( key, mod ) + 1

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

    H = modulo( int( log(key) * 8.192e3 ), mod ) + 1

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

    H = modulo( int( log(key) * 3.2768d4 ), mod ) + 1

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
  function hashmap_find( self, key, stat ) result( node )
    !/ -----------------------------------------------------------------------------------
    !!
    !! |  stat  | errmsg         |
    !! | :----: | -------------- |
    !! |    0   | n/a            |
    !! |    1   | key is NULL    |
    !! |    2   | no chain found |
    !! |    3   | no node found  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap),    intent(inout) :: self !! reference to this object
    class(*), pointer, intent(in)    :: key  !! key to hash
    integer, optional, intent(out)   :: stat !! optional return status
    class(btree_node), pointer       :: node
    !/ -----------------------------------------------------------------------------------
    integer               :: index
    integer               :: istat
    integer               :: kstat
    class(BTree), pointer :: tree
    !/ -----------------------------------------------------------------------------------
    istat = 0

    nullify( node )

    if ( associated( key ) ) then
       index = object_to_hash( key, self%num_chain )

       tree => self%chain( index )%ptr

       if ( associated( tree ) ) then
          node => tree%find( key, kstat )
          if ( 0.eq.kstat ) then
             istat = 0
          else
             istat = 3
          end if
       else
          istat = 2
       end if
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function hashmap_find

  


  !/ =====================================================================================
  subroutine hashmap_set( self, key, obj, recover, stat )
    !/ -----------------------------------------------------------------------------------
    !!
    !! |  stat  | errmsg      |
    !! | :----: | ----------- |
    !! |    0   | n/a         |
    !! |    1   | key is NULL |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap),              intent(inout) :: self     !! reference to this object
    class(*), pointer,           intent(in)    :: key      !! key to hash
    class(*), pointer,           intent(in)    :: obj      !! object to store
    class(*), optional, pointer, intent(inout) :: recover  !! optional recovery
    integer,  optional,          intent(out)   :: stat     !! optional return status
    !/ -----------------------------------------------------------------------------------
    class(btree_node), pointer :: node
    class(*),          pointer :: old_data
    integer                    :: index
    integer                    :: istat
   !/ -----------------------------------------------------------------------------------

    nullify( old_data )
    
    node => self%find( key, istat )

    select case( istat )
    case (0) !----- old node found
       old_data => node%object
       node%object => obj
       istat = 0
    case (1) !----- key is null
       istat = 1
    case (2) !----- no existing chain
       index = hash( key, self%num_chain )
       allocate( self%chain(index)%ptr )
       call self%chain(index)%ptr%insert( key, obj )
       self%item_count = self%item_count + 1
       istat = 0
    case (3) !----- no existing node in the chain
       index = hash( key, self%num_chain )
       call self%chain(index)%ptr%insert( key, obj )
       self%item_count = self%item_count + 1
       istat = 0
    case default
    end select

    if ( present( recover ) ) recover => old_data

    if ( present( stat ) ) stat = istat
    
  end subroutine hashmap_set


  !/ =====================================================================================
  function hashmap_get( self, key, stat ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !!
    !! |  stat  | errmsg         |
    !! | :----: | -------------- |
    !! |    0   | n/a            |
    !! |    1   | key is NULL    |
    !! |    2   | no node found  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HashMap),     intent(inout) :: self !! reference to this object
    class(*), pointer,  intent(in)    :: key  !! key to hash
    integer,  optional, intent(out)   :: stat !! optional return status
    class(*), pointer                 :: obj
    !/ -----------------------------------------------------------------------------------
    class(btree_node), pointer :: node
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    nullify( obj )
    
    node => self%find( key, istat )

    if ( 0.eq.istat ) obj => node%object
    
    if ( present( stat ) ) then
       if ( 2.lt.istat ) istat = 2
       stat = istat
    end if
    
  end function hashmap_get





















  !/ =====================================================================================
  subroutine hashmap_iter_rewind( self )
    !/ -----------------------------------------------------------------------------------
    !! Rewind the internal iterator.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Hashmap), intent(inout) :: self !! reference to this hashmap
    !/ -----------------------------------------------------------------------------------

    self%chain_index = 1

  end subroutine hashmap_iter_rewind


  !/ =====================================================================================
  function hashmap_iter_has_next( self, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Can this Hashmap's internal iterator return more nodes?
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | hashmap index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Hashmap),    intent(in)  :: self  !! reference to this hashmap
    integer, optional, intent(out) :: stat  !! optional retuen status
    logical                        :: res   !! true if there are more nodes to be returned
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------


  end function hashmap_iter_has_next



  !/ =====================================================================================
  function hashmap_iter_next_node( self, stat ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Attempt to return a pointer to the next node in this Hashmap's internal iterattor.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | hashmap index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Hashmap),    intent(inout) :: self  !! reference to this hashmap
    integer, optional, intent(out)   :: stat  !! optional retuen status
    class(btree_node), pointer       :: node  !! pointer to the next node
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------


  end function hashmap_iter_next_node


  !/ =====================================================================================
  function hashmap_iter_next( self, stat ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Attempt to return a pointer to the next stored object in this Hashmap's
    !! internal iterattor.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | hashmap index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Hashmap),    intent(inout) :: self  !! reference to this hashmap
    integer, optional, intent(out)   :: stat  !! optional retuen status
    class(*), pointer                :: obj   !! pointer to the object
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------


  end function hashmap_iter_next


  !/ =====================================================================================
  function hashmap_iter_next_key( self, stat ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Attempt to return a pointer to the next key in this Hashmap's
    !! internal iterattor.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | hashmap index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Hashmap),    intent(inout) :: self  !! reference to this hashmap
    integer, optional, intent(out)   :: stat  !! optional retuen status
    class(*), pointer                :: obj   !! pointer to the key
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------


  end function hashmap_iter_next_key







  
end module hash_object_class

!/ =======================================================================================
!/ **                         H A S H _ O B J E C T _ C L A S S                         **
!/ =========================================================================== END FILE ==
