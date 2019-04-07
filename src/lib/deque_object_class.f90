!/ ====================================================================== BEGIN FILE =====
!/ **                        D E Q U E _ O B J E C T _ C L A S S                        **
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
module deque_object_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-05-01
  !! license: GPL
  !!
  !! Provides a double linked list of unlimited polymorphic objects.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  
  !/ =====================================================================================
  type, public :: deque_node
     !/ ----------------------------------------------------------------------------------
     !! Deque Node.
     !/ ----------------------------------------------------------------------------------
     
     class(*),          pointer :: object => null() !! node object
     class(deque_node), pointer :: prev   => null() !! pointer to the previous node in
     !!                                               the deque
     class(deque_node), pointer :: next   => null() !! pointer to the next node in the deque


   contains


     procedure, public :: delete => deque_node_delete

     final :: deque_node_destroy
     
  end type deque_node


  !/ =====================================================================================
  type, public :: Deque
     !/ ----------------------------------------------------------------------------------
     !! Deque Structure.
     !/ ----------------------------------------------------------------------------------

     class(deque_node), pointer :: head_node    => null() !! head of the deque
     class(deque_node), pointer :: tail_node    => null() !! tail of the deque
     class(deque_node), pointer :: current_node => null() !! current iterrator node

     integer                    :: nodes_in_deque = 0  !! number of nodes in the deque

     logical                    :: iterator_set = .false.
     logical                    :: before_head  = .false.
     logical                    :: after_tail   = .false.


   contains


     procedure, public :: empty    => deque_is_empty
     procedure, public :: size     => deque_size
     procedure, public :: clear    => deque_clear
     procedure, public :: retract  => deque_pop_tail_object
     procedure, public :: assert   => deque_push_tail_object
     procedure, public :: pop      => deque_pop_head_object
     procedure, public :: push     => deque_push_head_object
     procedure, public :: peekHead => deque_peek_head_object
     procedure, public :: peekTail => deque_peek_tail_object

     procedure, public :: head     => deque_goto_head
     procedure, public :: tail     => deque_goto_tail
     procedure, public :: hasNext  => deque_has_next
     procedure, public :: hasPrev  => deque_has_prev
     procedure, public :: next     => deque_get_next
     procedure, public :: prev     => deque_get_prev

     ! ----- alias for FIFO operations ----------------------
     procedure, public :: add      => deque_push_tail_object
     procedure, public :: remove   => deque_pop_head_object

     final :: deque_destroy
  end type deque


  !/ -------------------------------------------------------------------------------------
  interface deque_node
     !/ ----------------------------------------------------------------------------------
     module procedure :: deque_node_alloc
  end interface deque_node




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function deque_node_alloc( obj ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Initialize a new deque node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), target,  intent(inout) :: obj  !! reference to an object.
    class(deque_node), pointer       :: node !! new node structure.
    !/ -----------------------------------------------------------------------------------

    allocate( node )

    node%object => obj
    nullify( node%prev )
    nullify( node%next )    

  end function deque_node_alloc


  !/ =====================================================================================
  subroutine deque_node_delete( self, delobj )
    !/ -----------------------------------------------------------------------------------
    !! Clean up the deque node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(deque_node), intent(inout) :: self   !! reference to this deque node class.
    logical, optional, intent(in)    :: delobj !! if true then dispose the stored object.
    !/ -----------------------------------------------------------------------------------

    if ( present( delobj ) ) then
       if ( delobj ) then
          deallocate( self%object )
       end if
    end if

    nullify( self%object  )
    nullify( self%prev )
    nullify( self%next )

  end subroutine deque_node_delete


  !/ =====================================================================================
  subroutine deque_node_destroy( node )
    !/ -----------------------------------------------------------------------------------
    !! Deque node destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(deque_node) :: node !! new node structure.
    !/ -----------------------------------------------------------------------------------

    call node%delete( .true. )

  end subroutine deque_node_destroy


  !/ =====================================================================================
  subroutine deque_destroy( queue )
    !/ -----------------------------------------------------------------------------------
    !! Deque destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Deque) :: queue !! new deque structure.
    !/ -----------------------------------------------------------------------------------

    call queue%clear( .true. )

  end subroutine deque_destroy






  !/ =======================================================================================
  pure function deque_is_empty( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Determine if the deque is empty.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(in) :: self !! reference to this deque.
    logical                  :: stat !! true if the queue is empty.
    !/ -------------------------------------------------------------------------------------

    stat = .true.
    if ( associated( self%head_node ) ) then
       stat = .false.
    end if

  end function deque_is_empty


  !/ =======================================================================================
  pure function deque_size( self ) result( n )
    !/ -------------------------------------------------------------------------------------
    !! Return the number of items currently stored in this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(in) :: self !! reference to this deque.
    integer                  :: n    !! number of items in deque.
    !/ -------------------------------------------------------------------------------------

    n = self%nodes_in_deque

  end function deque_size


  !/ =======================================================================================
  subroutine deque_clear( self, del )
    !/ -------------------------------------------------------------------------------------
    !! Clear the contents of the deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(inout) :: self !! reference to this deque.
    logical,      optional      :: del  !! flag - should the deque deallocate the contents?
    !/ -------------------------------------------------------------------------------------
    logical           :: da
    class(*), pointer :: obj
    !/ -------------------------------------------------------------------------------------

    da = .false.
    if ( present( del ) ) then
       da = del
    end if

    pop_all: do
       if ( self%empty() ) exit pop_all
       obj => self%pop()
       if ( da ) then
          if ( associated( obj ) ) then
             deallocate( obj )
          end if
       end if
    end do pop_all

    nullify( self%head_node )
    nullify( self%current_node )
    nullify( self%tail_node )

    self%nodes_in_deque = 0

    self%iterator_set = .false.
    self%before_head  = .false.
    self%after_tail   = .false.

  end subroutine deque_clear






  !/ =====================================================================================
  subroutine deque_push_head_object( self, obj )
    !/ -----------------------------------------------------------------------------------
    !! Push an unlimited polymorphic object onto the head of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Deque),      intent(inout) :: self !! reference to this deque class.
    class(*), pointer, intent(in) :: obj  !! deque data.
    !/ -----------------------------------------------------------------------------------
    class(deque_node), pointer :: new_node
    !/ -----------------------------------------------------------------------------------

    new_node => deque_node( obj )
    self%nodes_in_deque = self%nodes_in_deque + 1

    if ( associated( self%head_node ) ) then
       !/ ----- deque already has items in it ------------------
       new_node%next       => self%head_node
       self%head_node%prev => new_node
       self%head_node      => new_node
    else
       !/ ----- deque is empty ---------------------------------
       self%head_node => new_node
       self%tail_node => new_node
    end if

    self%iterator_set = .false.
    self%before_head  = .false.
    self%after_tail   = .false.

  end subroutine deque_push_head_object


  !/ =====================================================================================
  subroutine deque_push_tail_object( self, obj )
    !/ -----------------------------------------------------------------------------------
    !! Push an unlimited polymorphic object onto the tail of the deque.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Deque),      intent(inout) :: self !! reference to this deque class.
    class(*), pointer, intent(in)    :: obj  !! deque data.
    !/ -----------------------------------------------------------------------------------
    class(deque_node), pointer :: new_node
    !/ -----------------------------------------------------------------------------------

    new_node => deque_node( obj )
    self%nodes_in_deque = self%nodes_in_deque + 1

    if ( associated( self%tail_node ) ) then
       !/ ----- deque already has items in it ------------------
       new_node%prev       => self%tail_node
       self%tail_node%next => new_node
       self%tail_node      => new_node
    else
       !/ ----- deque is empty ---------------------------------
       self%head_node => new_node
       self%tail_node => new_node
    end if

    self%iterator_set = .false.
    self%before_head  = .false.
    self%after_tail   = .false.

  end subroutine deque_push_tail_object






  !/ =====================================================================================
  function deque_pop_head_object( self, stat, errmsg ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Pop an unlimited polymorphic object from the head of the deque.
    !!
    !! |  stat  | errmsg                  |
    !! | :----: | ----------------------- |
    !! |    0   | n/a                     |
    !! |    1   | deque empty             |
    !! |    2   | node data not allocated |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Deque),           intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    class(*),     pointer                 :: obj    !! returned pointer to an object.
    !/ -----------------------------------------------------------------------------------
    class(deque_node), pointer :: temp_node
    integer                    :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify( obj )
    self%iterator_set = .false.
    self%before_head  = .false.
    self %after_tail  = .false.

    if ( associated(self%head_node) ) then
       !/ ----- deque has items in it --------------------------
       temp_node      => self%head_node
       self%head_node => self%head_node%next
       obj            => temp_node%object
       if ( .not.associated( obj ) ) istat = 2
       call temp_node%delete( .false. )
       nullify( temp_node )
       self%nodes_in_deque = self%nodes_in_deque - 1
       if ( 0.eq.self%nodes_in_deque ) then
          nullify( self%head_node )
          nullify( self%tail_node )
       end if
    else
       istat = 1
    end if

    if ( present( stat ) )  stat = istat

    if ( present( errmsg ) ) then
       select case( istat )
       case (1)
          errmsg = 'deque%pop head: deque is empty'
       case (2)
          errmsg = 'deque%pop head: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function deque_pop_head_object


  !/ =====================================================================================
  function deque_pop_tail_object( self, stat, errmsg ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Pop an unlimited polymorphic object from the tail of the deque.
    !!
    !! |  stat  | errmsg                  |
    !! | :----: | ----------------------- |
    !! |    0   | n/a                     |
    !! |    1   | deque empty             |
    !! |    2   | node data not allocated |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Deque),           intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    class(*),     pointer                 :: obj    !! returned pointer to an object.
    !/ -----------------------------------------------------------------------------------
    class(deque_node), pointer :: temp_node
    integer                   :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify( obj )
    self%iterator_set = .false.
    self%before_head  = .false.
    self %after_tail  = .false.

    if ( associated(self%tail_node) ) then
       !/ ----- deque has items in it --------------------------
       temp_node      => self%tail_node
       self%tail_node => self%tail_node%prev
       obj            => temp_node%object
       if ( .not.associated( obj ) ) istat = 2
       call temp_node%delete( .false. )
       nullify( temp_node )
       self%nodes_in_deque = self%nodes_in_deque - 1
       if ( 0.eq.self%nodes_in_deque ) then
          nullify( self%head_node )
          nullify( self%tail_node )
       end if
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

    if ( present( errmsg ) ) then
       select case( istat )
       case (1)
          errmsg = 'deque%pop tail: deque is empty'
       case (2)
          errmsg = 'deque%pop tail: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function deque_pop_tail_object






  !/ =====================================================================================
  function deque_peek_head_object( self, stat, errmsg ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the head object without removing it. Return a NULL if the deque is empty.
    !!
    !! |  stat  | errmsg                  |
    !! | :----: | ----------------------- |
    !! |    0   | n/a                     |
    !! |    1   | deque empty             |
    !! |    2   | node data not allocated |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Deque),           intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    class(*),     pointer               :: obj    !! returned pointer to an object.
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify(obj)

    if ( associated( self%head_node ) ) then
       obj => self%head_node%object
       if ( .not.associated( obj ) ) istat = 2
    else
       istat = 1
    end if

    if ( present( stat ) )  stat = istat

    if ( present( errmsg ) ) then
       select case( istat )
       case (1)
          errmsg = 'deque%peekHead: deque is empty'
       case (2)
          errmsg = 'deque%peekHead: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function deque_peek_head_object


  !/ =====================================================================================
  function deque_peek_tail_object( self, stat, errmsg ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the tail object without removing it. Return a NULL if the deque is empty.
    !!
    !! |  stat  | errmsg                  |
    !! | :----: | ----------------------- |
    !! |    0   | n/a                     |
    !! |    1   | deque empty             |
    !! |    2   | node data not allocated |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Deque),           intent(in)  :: self   !! reference to this deque class.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(*), optional, intent(out) :: errmsg !! optional error message.
    class(*),     pointer               :: obj    !! returned pointer to an object.
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify(obj)

    if ( associated( self%tail_node ) ) then
       obj => self%tail_node%object
       if ( .not.associated( obj ) ) istat = 2
    else
       istat = 1
    end if

    if ( present( stat ) )  stat = istat

    if ( present( errmsg ) ) then
       select case( istat )
       case (1)
          errmsg = 'deque%peekTail: deque is empty'
       case (2)
          errmsg = 'deque%peekTail: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function deque_peek_tail_object






  !/ =======================================================================================
  subroutine deque_goto_head( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Head. Set the iterator to the head of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    self%current_node => self%head_node
    self%iterator_set = .true.
    self%before_head  = .true.
    self%after_tail   = .false.

  end subroutine deque_goto_head


  !/ =======================================================================================
  subroutine deque_goto_tail( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Tail. Set the iterator to the tail of this deque.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(inout) :: self !! reference to this deque.
    !/ -------------------------------------------------------------------------------------

    self%current_node => self%tail_node
    self%iterator_set = .true.
    self%before_head  = .false.
    self%after_tail   = .true.

  end subroutine deque_goto_tail


  !/ =======================================================================================
  function deque_has_next( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a next operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(inout) :: self !! reference to this deque.
    logical                     :: stat !! true if a next object can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = .false.

    if ( self%iterator_set ) then
       if ( self%before_head ) then
          stat = .true.
       else
          if ( associated( self%current_node ) ) then
             if ( associated( self%current_node%next ) ) then
                stat = .true.
             end if
          end if
       end if
    end if

  end function deque_has_next


  !/ =======================================================================================
  function deque_has_prev( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a previous operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque), intent(inout) :: self !! reference to this deque.
    logical                     :: stat !! true if a previous object can be returned.
    !/ -------------------------------------------------------------------------------------

    stat = .false.

    if ( self%iterator_set ) then
       if ( self%after_tail ) then
          stat = .true.
       else
          if ( associated( self%current_node ) ) then
             if ( associated( self%current_node%prev ) ) then
                stat = .true.
             end if
          end if
       end if
    end if

  end function deque_has_prev


  !/ =======================================================================================
  function deque_get_next( self, stat, errmsg ) result( obj )
    !/ -------------------------------------------------------------------------------------
    !! Get Next. Advance the iterator to the next position in the deque and return the
    !! unlimited polymorphic object at that position. This does not remove the entry
    !! from the deque.
    !!
    !! |  stat  | errmsg                    |
    !! | :----: | ------------------------- |
    !! |    0   | n/a                       |
    !! |    1   | iterator in unknown state |
    !! |    2   | node data not allocated   |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque),           intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    class(*),     pointer                 :: obj    !! pointer to the next object.
    !/ -------------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify(obj)

    if ( self%iterator_set ) then

       if ( self%before_head ) then
          self%current_node => self%head_node
          self%before_head  = .false.
       else
          self%current_node => self%current_node%next
       end if

       if ( associated( self%current_node ) ) then

          if ( associated( self%current_node%object ) ) then
             obj => self%current_node%object
          else
             istat = 2
          end if

       else
          self%iterator_set = .false.
       end if

    else
       istat = 1
    end if

    if ( present( stat ) )  stat = istat

    if ( present( errmsg ) ) then
       select case( istat )
       case (1)
          errmsg = 'deque%next: iterator is in an unknown state'
       case (2)
          errmsg = 'deque%next: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function deque_get_next


  !/ =======================================================================================
  function deque_get_prev( self, stat, errmsg ) result( obj )
    !/ -------------------------------------------------------------------------------------
    !! Get Previous. Retard the iterator to the previous position in the deque and return
    !! the unlimited polymorphic object at that position. This does not remove the entry
    !! from the deque.
    !!
    !! |  stat  | errmsg                    |
    !! | :----: | ------------------------- |
    !! |    0   | n/a                       |
    !! |    1   | iterator in unknown state |
    !! |    2   | node data not allocated   |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(Deque),           intent(inout) :: self   !! reference to this deque class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    class(*),     pointer                 :: obj    !! pointer to the previous object.
    !/ -------------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify(obj)

    if ( self%iterator_set ) then

       if ( self%after_tail ) then
          self%current_node => self%tail_node
          self%after_tail  = .false.
       else
          self%current_node => self%current_node%prev
       end if

       if ( associated( self%current_node ) ) then

          if ( associated( self%current_node%object ) ) then
             obj => self%current_node%object
          else
             istat = 2
          end if

       else
          self%iterator_set = .false.
       end if

    else
       istat = 1
    end if

    if ( present( stat ) )  stat = istat

    if ( present( errmsg ) ) then
       select case( istat )
       case (1)
          errmsg = 'deque%prev: iterator is in an unknown state'
       case (2)
          errmsg = 'deque%prev: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function deque_get_prev


end module deque_object_class


!/ =======================================================================================
!/ **                        D E Q U E _ O B J E C T _ C L A S S                        **
!/ =========================================================================== END FILE ==
