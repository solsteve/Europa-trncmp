!/ ====================================================================== BEGIN FILE =====
!/ **                       I N T E G E R _ D E Q U E _ C L A S S                       **
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
module integer_deque_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-05-06
  !! license: GPL
  !!
  !! Provides a double linked list of integers by wrapping the procedures from the
  !! object_deque_class module.
  !/ -------------------------------------------------------------------------------------
  use object_deque_class
  implicit none
  private

  
  !/ =====================================================================================
  type, public :: IntegerDeque
     !/ ----------------------------------------------------------------------------------
     !! Integer Deque Structure.
     !/ ----------------------------------------------------------------------------------
 
     type(Deque) :: queue

     
   contains


     procedure, public :: empty    => ideque_is_empty
     procedure, public :: size     => ideque_size
     procedure, public :: clear    => ideque_clear
     procedure, public :: retract  => ideque_pop_tail
     procedure, public :: assert   => ideque_push_tail
     procedure, public :: pop      => ideque_pop_head
     procedure, public :: push     => ideque_push_head
     procedure, public :: peekHead => ideque_peek_head
     procedure, public :: peekTail => ideque_peek_tail

     procedure, public :: head     => ideque_goto_head
     procedure, public :: tail     => ideque_goto_tail
     procedure, public :: hasNext  => ideque_has_next
     procedure, public :: hasPrev  => ideque_has_prev
     procedure, public :: next     => ideque_get_next
     procedure, public :: prev     => ideque_get_prev

     ! ----- alias for FIFO operations ----------------------
     procedure, public :: add      => ideque_push_tail
     procedure, public :: remove   => ideque_pop_head

     final :: ideque_destroy
     
  end type IntegerDeque

public :: object2integer
public :: integer2object




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function object2integer( obj, stat, errmsg ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into an integer.
    !!
    !! |  stat  | errmsg                   |
    !! | :----: | ------------------------ |
    !! |    0   | n/a                      |
    !! |    3   | node data not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! reference to an unlimited
    !!                                               polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    integer                             :: n      !! integer return value
    !/ -----------------------------------------------------------------------------------

    n = -1

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( integer )
          n = obj
          class default
          if ( present( stat ) )   stat = 3
          if ( present( errmsg ) ) errmsg = 'ideque: data is not integer'             
       end select
    end if

  end function object2integer


  !/ =====================================================================================
  function integer2object( n ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert an integer into an unlimited polymorphic object. This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: n   !! the input integer.
    class(*), pointer    :: obj !! return the integer as an unlimited polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=n )

  end function integer2object






  !/ =====================================================================================
  subroutine ideque_destroy( iqueue )
    !/ -----------------------------------------------------------------------------------
    !! Deque destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(IntegerDeque) :: iqueue !! new deque structure.
    !/ -----------------------------------------------------------------------------------

    call iqueue%queue%clear( .true. )

  end subroutine ideque_destroy






  !/ =======================================================================================
  pure function ideque_is_empty( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Determine if the deque is empty.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(in) :: self !! reference to this deque.
    logical                         :: stat !! true if the deque is empty.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%empty()

  end function ideque_is_empty


  !/ =======================================================================================
  pure function ideque_size( self ) result( n )
    !/ -------------------------------------------------------------------------------------
    !! Return the number of items currently stored in this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(in) :: self !! reference to this deque.
    integer                         :: n    !! number of items in deque.
    !/ -------------------------------------------------------------------------------------

    n = self%queue%size()

  end function ideque_size


  !/ =======================================================================================
  subroutine ideque_clear( self, del )
    !/ -------------------------------------------------------------------------------------
    !! Clear the contents of the ideque
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque.
    logical,             optional      :: del  !! should the deque deallocate the contents?
    !/ -------------------------------------------------------------------------------------

    call self%queue%clear( del )

  end subroutine ideque_clear








  !/ =====================================================================================
  subroutine ideque_push_head( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Push an integer onto the head of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque class.
    integer,             intent(in)    :: n    !! data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => integer2object( n )
    call self%queue%push( obj )

  end subroutine ideque_push_head


  !/ =====================================================================================
  subroutine ideque_push_tail( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Push an integer onto the tail of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque class.
    integer,             intent(in)    :: n    !! data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => integer2object( n )
    call self%queue%assert( obj )

  end subroutine ideque_push_tail






  !/ =====================================================================================
  function ideque_pop_head( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Pop an integer from the head of the deque.
    !!
    !! |  stat  | errmsg                   |
    !! | :----: | ------------------------ |
    !! |    0   | n/a                      |
    !! |    1   | deque empty              |
    !! |    2   | node data not allocated  |
    !! |    3   | node data not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque),    intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    integer                               :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%pop( stat, errmsg )
    val =  object2integer( obj, stat, errmsg )

  end function ideque_pop_head


  !/ =====================================================================================
  function ideque_pop_tail( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Pop an integer from the tail of the deque.
    !!
    !! |  stat  | errmsg                   |
    !! | :----: | ------------------------ |
    !! |    0   | n/a                      |
    !! |    1   | deque empty              |
    !! |    2   | node data not allocated  |
    !! |    3   | node data not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque),    intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    integer                               :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%retract( stat, errmsg )
    val =  object2integer( obj, stat, errmsg )

  end function ideque_pop_tail






  !/ =====================================================================================
  function ideque_peek_head( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the head integer without removing it. Return a -1 if the deque is empty.
    !!
    !! |  stat  | errmsg                   |
    !! | :----: | ------------------------ |
    !! |    0   | n/a                      |
    !! |    1   | deque empty              |
    !! |    2   | node data not allocated  |
    !! |    3   | node data not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque),    intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    integer                             :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%peekHead( stat, errmsg )
    val =  object2integer( obj, stat, errmsg )

  end function ideque_peek_head


  !/ =====================================================================================
  function ideque_peek_tail( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the tail integer without removing it. Return a -1 if the deque is empty.
    !!
    !! |  stat  | errmsg                   |
    !! | :----: | ------------------------ |
    !! |    0   | n/a                      |
    !! |    1   | deque empty              |
    !! |    2   | node data not allocated  |
    !! |    3   | node data not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque),    intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    integer                             :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%peekTail( stat, errmsg )
    val =  object2integer( obj, stat, errmsg )

  end function ideque_peek_tail






  !/ =======================================================================================
  subroutine ideque_goto_head( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Head. Set the iterator to the head of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    call self%queue%head

  end subroutine ideque_goto_head


  !/ =======================================================================================
  subroutine ideque_goto_tail( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Tail. Set the iterator to the tail of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    call self%queue%tail

  end subroutine ideque_goto_tail


  !/ =======================================================================================
  function ideque_has_next( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a next operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque.
    logical                            :: stat !! true if a next integer can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%hasNext()

  end function ideque_has_next


  !/ =======================================================================================
  function ideque_has_prev( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a previous operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque), intent(inout) :: self !! reference to this deque.
    logical                            :: stat !! true if a previous integer can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%hasPrev()

  end function ideque_has_prev


  !/ =======================================================================================
  function ideque_get_next( self, stat, errmsg ) result( val )
    !/ -------------------------------------------------------------------------------------
    !! Get Next. Advance the iterator to the next position in the deque and return the
    !! integer at that position. This does not remove the entry from the deque.
    !!
    !! |  stat  | errmsg                    |
    !! | :----: | ------------------------- |
    !! |    0   | n/a                       |
    !! |    1   | iterator in unknown state |
    !! |    2   | node data not allocated   |
    !! |    3   | node data not an integer  |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque),    intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    integer                               :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%next( stat, errmsg )
    val =  object2integer( obj, stat, errmsg )

  end function ideque_get_next


  !/ =======================================================================================
  function ideque_get_prev( self, stat, errmsg ) result( val )
    !/ -------------------------------------------------------------------------------------
    !! Get Previous. Retard the iterator to the previous position in the deque and return
    !! the integer at that position. This does not remove the entry from the deque.
    !!
    !! |  stat  | errmsg                    |
    !! | :----: | ------------------------- |
    !! |    0   | n/a                       |
    !! |    1   | iterator in unknown state |
    !! |    2   | node data not allocated   |
    !! |    3   | node data not an integer  |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(IntegerDeque),    intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    integer                               :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%prev( stat, errmsg )
    val =  object2integer( obj, stat, errmsg )

  end function ideque_get_prev


end module integer_deque_class


!/ =======================================================================================
!/ **                       I N T E G E R _ D E Q U E _ C L A S S                       **
!/ =========================================================================== END FILE ==
