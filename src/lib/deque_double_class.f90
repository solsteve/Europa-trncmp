!/ ====================================================================== BEGIN FILE =====
!/ **                        D E Q U E _ D O U B L E _ C L A S S                        **
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
module deque_double_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-05-06
  !! license: GPL
  !!
  !! Provides a double linked list of double precision by wrapping the procedures
  !! from the object_deque_class module.
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use deque_object_class
  implicit none
  private

  
  !/ =====================================================================================
  type, public :: DequeDouble
     !/ ----------------------------------------------------------------------------------
     !! Double Precision Deque Structure.
     !/ ----------------------------------------------------------------------------------
 
     type(Deque) :: queue

     
   contains


     procedure, public :: empty    => ddeque_is_empty
     procedure, public :: size     => ddeque_size
     procedure, public :: clear    => ddeque_clear
     procedure, public :: retract  => ddeque_pop_tail
     procedure, public :: assert   => ddeque_push_tail
     procedure, public :: pop      => ddeque_pop_head
     procedure, public :: push     => ddeque_push_head
     procedure, public :: peekHead => ddeque_peek_head
     procedure, public :: peekTail => ddeque_peek_tail

     procedure, public :: head     => ddeque_goto_head
     procedure, public :: tail     => ddeque_goto_tail
     procedure, public :: hasNext  => ddeque_has_next
     procedure, public :: hasPrev  => ddeque_has_prev
     procedure, public :: next     => ddeque_get_next
     procedure, public :: prev     => ddeque_get_prev

     ! ----- alias for FIFO operations ----------------------
     procedure, public :: add      => ddeque_push_tail
     procedure, public :: remove   => ddeque_pop_head

     final :: ddeque_destroy
     
  end type DequeDouble

public :: object2double
public :: double2object




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function object2double( obj, stat, errmsg ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a double precision.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    3   | node data not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! reference to an unlimited
    !!                                               polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    real(dp)                            :: n      !! double precision return value
    !/ -----------------------------------------------------------------------------------

    n = -1

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( real(dp) )
          n = obj
          class default
          if ( present( stat ) )   stat = 3
          if ( present( errmsg ) ) errmsg = 'ddeque: data is not double precision'             
       end select
    end if

  end function object2double


  !/ =====================================================================================
  function double2object( n ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a double precision into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: n   !! the input double precision.
    class(*), pointer    :: obj !! return the double precision as an unlimited
    !!                             polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=n )

  end function double2object






  !/ =====================================================================================
  subroutine ddeque_destroy( iqueue )
    !/ -----------------------------------------------------------------------------------
    !! Deque destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(DequeDouble) :: iqueue !! new deque structure.
    !/ -----------------------------------------------------------------------------------

    call iqueue%queue%clear( .true. )

  end subroutine ddeque_destroy






  !/ =======================================================================================
  pure function ddeque_is_empty( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Determine if the deque is empty.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(in) :: self !! reference to this deque.
    logical                        :: stat !! true if the deque is empty.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%empty()

  end function ddeque_is_empty


  !/ =======================================================================================
  pure function ddeque_size( self ) result( n )
    !/ -------------------------------------------------------------------------------------
    !! Return the number of items currently stored in this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(in) :: self !! reference to this deque.
    integer                        :: n    !! number of items in deque.
    !/ -------------------------------------------------------------------------------------

    n = self%queue%size()

  end function ddeque_size


  !/ =======================================================================================
  subroutine ddeque_clear( self, del )
    !/ -------------------------------------------------------------------------------------
    !! Clear the contents of the ddeque
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque.
    logical,            optional      :: del  !! should the deque deallocate the contents?
    !/ -------------------------------------------------------------------------------------

    call self%queue%clear( del )

  end subroutine ddeque_clear








  !/ =====================================================================================
  subroutine ddeque_push_head( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Push a double precision onto the head of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque class.
    real(dp),           intent(in)    :: n    !! data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => double2object( n )
    call self%queue%push( obj )

  end subroutine ddeque_push_head


  !/ =====================================================================================
  subroutine ddeque_push_tail( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Push a double precision onto the tail of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque class.
    real(dp),           intent(in)    :: n    !! data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => double2object( n )
    call self%queue%assert( obj )

  end subroutine ddeque_push_tail






  !/ =====================================================================================
  function ddeque_pop_head( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Pop a double precision from the head of the deque.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeDouble),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(dp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%pop( stat, errmsg )
    val =  object2double( obj, stat, errmsg )

  end function ddeque_pop_head


  !/ =====================================================================================
  function ddeque_pop_tail( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Pop a double precision from the tail of the deque.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeDouble),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(dp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%retract( stat, errmsg )
    val =  object2double( obj, stat, errmsg )

  end function ddeque_pop_tail






  !/ =====================================================================================
  function ddeque_peek_head( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the head double precision without removing it.
    !! Return a -1 if the deque is empty.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeDouble),     intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    real(dp)                            :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%peekHead( stat, errmsg )
    val =  object2double( obj, stat, errmsg )

  end function ddeque_peek_head


  !/ =====================================================================================
  function ddeque_peek_tail( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the tail double precision without removing it.
    !! Return a -1 if the deque is empty.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeDouble),     intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    real(dp)                            :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%peekTail( stat, errmsg )
    val =  object2double( obj, stat, errmsg )

  end function ddeque_peek_tail






  !/ =======================================================================================
  subroutine ddeque_goto_head( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Head. Set the iterator to the head of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    call self%queue%head

  end subroutine ddeque_goto_head


  !/ =======================================================================================
  subroutine ddeque_goto_tail( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Tail. Set the iterator to the tail of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    call self%queue%tail

  end subroutine ddeque_goto_tail


  !/ =======================================================================================
  function ddeque_has_next( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a next operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque.
    logical                           :: stat !! true if a next double can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%hasNext()

  end function ddeque_has_next


  !/ =======================================================================================
  function ddeque_has_prev( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a previous operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble), intent(inout) :: self !! reference to this deque.
    logical                           :: stat !! true if a previous double  can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%hasPrev()

  end function ddeque_has_prev


  !/ =======================================================================================
  function ddeque_get_next( self, stat, errmsg ) result( val )
    !/ -------------------------------------------------------------------------------------
    !! Get Next. Advance the iterator to the next position in the deque and return the
    !! double precision at that position. This does not remove the entry from the deque.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | iterator in unknown state         |
    !! |    2   | node data not allocated           |
    !! |    3   | node data not a double precision  |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(dp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%next( stat, errmsg )
    val =  object2double( obj, stat, errmsg )

  end function ddeque_get_next


  !/ =======================================================================================
  function ddeque_get_prev( self, stat, errmsg ) result( val )
    !/ -------------------------------------------------------------------------------------
    !! Get Previous. Retard the iterator to the previous position in the deque and return
    !! the double precision at that position. This does not remove the entry from the deque.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | iterator in unknown state         |
    !! |    2   | node data not allocated           |
    !! |    3   | node data not a double precision  |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeDouble),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(dp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%prev( stat, errmsg )
    val =  object2double( obj, stat, errmsg )

  end function ddeque_get_prev


end module deque_double_class


!/ =======================================================================================
!/ **                        D O U B L E _ D E Q U E _ C L A S S                        **
!/ =========================================================================== END FILE ==
