!/ ====================================================================== BEGIN FILE =====
!/ **                        D E Q U E _ S I N G L E _ C L A S S                        **
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
module deque_single_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-05-06
  !! license: GPL
  !!
  !! Provides a double linked list of single precision by wrapping the procedures
  !! from the object_deque_class module.
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use deque_object_class
  implicit none
  private

  
  !/ =====================================================================================
  type, public :: DequeSingle
     !/ ----------------------------------------------------------------------------------
     !! Single Precision Deque Structure.
     !/ ----------------------------------------------------------------------------------
 
     type(Deque) :: queue

     
   contains


     procedure, public :: empty    => fdeque_is_empty
     procedure, public :: size     => fdeque_size
     procedure, public :: clear    => fdeque_clear
     procedure, public :: retract  => fdeque_pop_tail
     procedure, public :: assert   => fdeque_push_tail
     procedure, public :: pop      => fdeque_pop_head
     procedure, public :: push     => fdeque_push_head
     procedure, public :: peekHead => fdeque_peek_head
     procedure, public :: peekTail => fdeque_peek_tail

     procedure, public :: head     => fdeque_goto_head
     procedure, public :: tail     => fdeque_goto_tail
     procedure, public :: hasNext  => fdeque_has_next
     procedure, public :: hasPrev  => fdeque_has_prev
     procedure, public :: next     => fdeque_get_next
     procedure, public :: prev     => fdeque_get_prev

     ! ----- alias for FIFO operations ----------------------
     procedure, public :: add      => fdeque_push_tail
     procedure, public :: remove   => fdeque_pop_head

     final :: fdeque_destroy
     
  end type DequeSingle

public :: object2single
public :: single2object




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function object2single( obj, stat, errmsg ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a single precision.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    3   | node data not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! reference to an unlimited
    !!                                               polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    real(sp)                            :: n      !! single precision return value
    !/ -----------------------------------------------------------------------------------

    n = -1

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( real(sp) )
          n = obj
          class default
          if ( present( stat ) )   stat = 3
          if ( present( errmsg ) ) errmsg = 'fdeque: data is not single precision'             
       end select
    end if

  end function object2single


  !/ =====================================================================================
  function single2object( n ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a single precision into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), intent(in) :: n   !! the input single precision.
    class(*), pointer    :: obj !! return the single precision as an unlimited
    !!                             polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=n )

  end function single2object






  !/ =====================================================================================
  subroutine fdeque_destroy( iqueue )
    !/ -----------------------------------------------------------------------------------
    !! Deque destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(DequeSingle) :: iqueue !! new deque structure.
    !/ -----------------------------------------------------------------------------------

    call iqueue%queue%clear( .true. )

  end subroutine fdeque_destroy






  !/ =======================================================================================
  pure function fdeque_is_empty( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Determine if the deque is empty.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(in) :: self !! reference to this deque.
    logical                        :: stat !! true if the deque is empty.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%empty()

  end function fdeque_is_empty


  !/ =======================================================================================
  pure function fdeque_size( self ) result( n )
    !/ -------------------------------------------------------------------------------------
    !! Return the number of items currently stored in this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(in) :: self !! reference to this deque.
    integer                        :: n    !! number of items in deque.
    !/ -------------------------------------------------------------------------------------

    n = self%queue%size()

  end function fdeque_size


  !/ =======================================================================================
  subroutine fdeque_clear( self, del )
    !/ -------------------------------------------------------------------------------------
    !! Clear the contents of the fdeque
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque.
    logical,            optional      :: del  !! should the deque deallocate the contents?
    !/ -------------------------------------------------------------------------------------

    call self%queue%clear( del )

  end subroutine fdeque_clear








  !/ =====================================================================================
  subroutine fdeque_push_head( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Push a single precision onto the head of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque class.
    real(sp),           intent(in)    :: n    !! data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => single2object( n )
    call self%queue%push( obj )

  end subroutine fdeque_push_head


  !/ =====================================================================================
  subroutine fdeque_push_tail( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Push a single precision onto the tail of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque class.
    real(sp),           intent(in)    :: n    !! data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => single2object( n )
    call self%queue%assert( obj )

  end subroutine fdeque_push_tail






  !/ =====================================================================================
  function fdeque_pop_head( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Pop a single precision from the head of the deque.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeSingle),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(sp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%pop( stat, errmsg )
    val =  object2single( obj, stat, errmsg )

  end function fdeque_pop_head


  !/ =====================================================================================
  function fdeque_pop_tail( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Pop a single precision from the tail of the deque.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeSingle),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(sp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%retract( stat, errmsg )
    val =  object2single( obj, stat, errmsg )

  end function fdeque_pop_tail






  !/ =====================================================================================
  function fdeque_peek_head( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the head single precision without removing it.
    !! Return a -1 if the deque is empty.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeSingle),     intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    real(sp)                            :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%peekHead( stat, errmsg )
    val =  object2single( obj, stat, errmsg )

  end function fdeque_peek_head


  !/ =====================================================================================
  function fdeque_peek_tail( self, stat, errmsg ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the tail single precision without removing it.
    !! Return a -1 if the deque is empty.
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    1   | deque empty                      |
    !! |    2   | node data not allocated          |
    !! |    3   | node data not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DequeSingle),     intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    real(sp)                            :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%peekTail( stat, errmsg )
    val =  object2single( obj, stat, errmsg )

  end function fdeque_peek_tail






  !/ =======================================================================================
  subroutine fdeque_goto_head( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Head. Set the iterator to the head of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    call self%queue%head

  end subroutine fdeque_goto_head


  !/ =======================================================================================
  subroutine fdeque_goto_tail( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Tail. Set the iterator to the tail of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    call self%queue%tail

  end subroutine fdeque_goto_tail


  !/ =======================================================================================
  function fdeque_has_next( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a next operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque.
    logical                           :: stat !! true if a next single can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%hasNext()

  end function fdeque_has_next


  !/ =======================================================================================
  function fdeque_has_prev( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a previous operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle), intent(inout) :: self !! reference to this deque.
    logical                           :: stat !! true if a previous single  can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = self%queue%hasPrev()

  end function fdeque_has_prev


  !/ =======================================================================================
  function fdeque_get_next( self, stat, errmsg ) result( val )
    !/ -------------------------------------------------------------------------------------
    !! Get Next. Advance the iterator to the next position in the deque and return the
    !! single precision at that position. This does not remove the entry from the deque.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | iterator in unknown state         |
    !! |    2   | node data not allocated           |
    !! |    3   | node data not a single precision  |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(sp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%next( stat, errmsg )
    val =  object2single( obj, stat, errmsg )

  end function fdeque_get_next


  !/ =======================================================================================
  function fdeque_get_prev( self, stat, errmsg ) result( val )
    !/ -------------------------------------------------------------------------------------
    !! Get Previous. Retard the iterator to the previous position in the deque and return
    !! the single precision at that position. This does not remove the entry from the deque.
    !!
    !! |  stat  | errmsg                            |
    !! | :----: | --------------------------------- |
    !! |    0   | n/a                               |
    !! |    1   | iterator in unknown state         |
    !! |    2   | node data not allocated           |
    !! |    3   | node data not a single precision  |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(DequeSingle),     intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    real(sp)                              :: val    !! returned data.
    !/ -----------------------------------------------------------------------------------
    class(*), pointer :: obj
    !/ -----------------------------------------------------------------------------------

    obj => self%queue%prev( stat, errmsg )
    val =  object2single( obj, stat, errmsg )

  end function fdeque_get_prev


end module deque_single_class


!/ =======================================================================================
!/ **                        D E Q U E _ S I N G L E _ C L A S S                        **
!/ =========================================================================== END FILE ==
