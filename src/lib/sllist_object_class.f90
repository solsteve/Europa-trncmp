!/ ====================================================================== BEGIN FILE =====
!/ **                       S L L I S T _ O B J E C T _ C L A S S                       **
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
module sllist_object_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-02
  !! license: GPL
  !!
  !! Provides a single linked list of unlimited polymorphic objects.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  
  !/ =====================================================================================
  type, public :: sllist_node
     !/ ----------------------------------------------------------------------------------
     !! sllist Node.
     !/ ----------------------------------------------------------------------------------
     
     class(*),           pointer :: object => null() !! node object
     class(sllist_node), pointer :: next   => null() !! pointer to the next node in the sllist

   contains

     procedure, public :: delete => sllist_node_delete

     final :: sllist_node_destroy
     
  end type sllist_node


  !/ =====================================================================================
  type, public :: sllist
     !/ ----------------------------------------------------------------------------------
     !! sllist Structure.
     !/ ----------------------------------------------------------------------------------

     class(sllist_node), pointer :: head_node    => null() !! head of the sllist
     class(sllist_node), pointer :: tail_node    => null() !! tail of the sllist
     class(sllist_node), pointer :: current_node => null() !! current iterrator node

     integer                     :: nodes_in_sllist = 0  !! number of nodes in the sllist

     logical                     :: iterator_set = .false.
     logical                     :: before_head  = .false.


   contains


     procedure, public :: empty    => sllist_is_empty
     procedure, public :: size     => sllist_size
     procedure, public :: clear    => sllist_clear
     procedure, public :: add      => sllist_push_tail_object
     procedure, public :: remove   => sllist_pop_head_object
     procedure, public :: peekHead => sllist_peek_head_object

     procedure, public :: rewind   => sllist_goto_head
     procedure, public :: hasNext  => sllist_has_next
     procedure, public :: next     => sllist_get_next

     final :: sllist_destroy

  end type sllist


  !/ -------------------------------------------------------------------------------------
  interface sllist_node
     !/ ----------------------------------------------------------------------------------
     module procedure :: sllist_node_alloc
  end interface sllist_node




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function sllist_node_alloc( obj ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Initialize a new sllist node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), target, intent(inout) :: obj  !! reference to an object.
    class(sllist_node), pointer     :: node !! new node structure.
    !/ -----------------------------------------------------------------------------------

    allocate( node )

    node%object => obj
    nullify( node%next )    

  end function sllist_node_alloc


  !/ =====================================================================================
  subroutine sllist_node_delete( self, delobj )
    !/ -----------------------------------------------------------------------------------
    !! Clean up the sllist node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(sllist_node), intent(inout) :: self   !! reference to this sllist node class.
    logical, optional,  intent(in)    :: delobj !! if true then dispose the stored object.
    !/ -----------------------------------------------------------------------------------

    if ( present( delobj ) ) then
       if ( delobj ) then
          deallocate( self%object )
       end if
    end if

    nullify( self%object  )
    nullify( self%next )

  end subroutine sllist_node_delete


  !/ =====================================================================================
  subroutine sllist_node_destroy( node )
    !/ -----------------------------------------------------------------------------------
    !! sllist node destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(sllist_node) :: node !! new node structure.
    !/ -----------------------------------------------------------------------------------

    call node%delete( .true. )

  end subroutine sllist_node_destroy


  !/ =====================================================================================
  subroutine sllist_destroy( queue )
    !/ -----------------------------------------------------------------------------------
    !! sllist destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(sllist) :: queue !! new sllist structure.
    !/ -----------------------------------------------------------------------------------

    call queue%clear( .true. )

  end subroutine sllist_destroy






  !/ =======================================================================================
  pure function sllist_is_empty( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Determine if the sllist is empty.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(sllist), intent(in) :: self !! reference to this sllist.
    logical                   :: stat !! true if the queue is empty.
    !/ -------------------------------------------------------------------------------------

    stat = .true.
    if ( associated( self%head_node ) ) then
       stat = .false.
    end if

  end function sllist_is_empty


  !/ =======================================================================================
  pure function sllist_size( self ) result( n )
    !/ -------------------------------------------------------------------------------------
    !! Return the number of items currently stored in this sllist.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(sllist), intent(in) :: self !! reference to this sllist.
    integer                   :: n    !! number of items in sllist.
    !/ -------------------------------------------------------------------------------------

    n = self%nodes_in_sllist

  end function sllist_size


  !/ =======================================================================================
  subroutine sllist_clear( self, del )
    !/ -------------------------------------------------------------------------------------
    !! Clear the contents of the sllist.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(sllist), intent(inout) :: self !! reference to this sllist.
    logical,       optional      :: del  !! flag - should the sllist deallocate the contents?
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
       obj => self%remove()
       if ( da ) then
          if ( associated( obj ) ) then
             deallocate( obj )
          end if
       end if
    end do pop_all

    nullify( self%head_node )
    nullify( self%current_node )
    nullify( self%tail_node )

    self%nodes_in_sllist = 0

    self%iterator_set = .false.
    self%before_head  = .false.

  end subroutine sllist_clear





  !/ =====================================================================================
  subroutine sllist_push_tail_object( self, obj )
    !/ -----------------------------------------------------------------------------------
    !! Push an unlimited polymorphic object onto the tail of the sllist.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(sllist),     intent(inout) :: self !! reference to this sllist class.
    class(*), pointer, intent(in)    :: obj  !! sllist data.
    !/ -----------------------------------------------------------------------------------
    class(sllist_node), pointer :: new_node
    !/ -----------------------------------------------------------------------------------

    new_node => sllist_node( obj )
    self%nodes_in_sllist = self%nodes_in_sllist + 1

    if ( associated( self%tail_node ) ) then
       !/ ----- sllist already has items in it ------------------
       self%tail_node%next => new_node
       self%tail_node      => new_node
    else
       !/ ----- sllist is empty ---------------------------------
       self%head_node => new_node
       self%tail_node => new_node
    end if

    self%iterator_set = .false.
    self%before_head  = .false.

  end subroutine sllist_push_tail_object






  !/ =====================================================================================
  function sllist_pop_head_object( self, stat, errmsg ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Pop an unlimited polymorphic object from the head of the sllist.
    !!
    !! |  stat  | errmsg                  |
    !! | :----: | ----------------------- |
    !! |    0   | n/a                     |
    !! |    1   | sllist empty            |
    !! |    2   | node data not allocated |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(sllist),          intent(inout) :: self   !! reference to this sllist class.
    integer,      optional, intent(out)   :: stat   !! optional error status.
    character(*), optional, intent(out)   :: errmsg !! optional error message.
    class(*),     pointer                 :: obj    !! returned pointer to an object.
    !/ -----------------------------------------------------------------------------------
    class(sllist_node), pointer :: temp_node
    integer                     :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify( obj )
    self%iterator_set = .false.
    self%before_head  = .false.

    if ( associated(self%head_node) ) then
       !/ ----- sllist has items in it --------------------------
       temp_node      => self%head_node
       self%head_node => self%head_node%next
       obj            => temp_node%object
       if ( .not.associated( obj ) ) istat = 2
       call temp_node%delete( .false. )
       nullify( temp_node )
       self%nodes_in_sllist = self%nodes_in_sllist - 1
       if ( 0.eq.self%nodes_in_sllist ) then
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
          errmsg = 'sllist%pop head: sllist is empty'
       case (2)
          errmsg = 'sllist%pop head: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function sllist_pop_head_object







  !/ =====================================================================================
  function sllist_peek_head_object( self, stat, errmsg ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Peek at the head object without removing it. Return a NULL if the sllist is empty.
    !!
    !! |  stat  | errmsg                  |
    !! | :----: | ----------------------- |
    !! |    0   | n/a                     |
    !! |    1   | sllist empty            |
    !! |    2   | node data not allocated |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(sllist),          intent(in)  :: self   !! reference to this sllist class.
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
          errmsg = 'sllist%peekHead: sllist is empty'
       case (2)
          errmsg = 'sllist%peekHead: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function sllist_peek_head_object








  !/ =======================================================================================
  subroutine sllist_goto_head( self )
    !/ -------------------------------------------------------------------------------------
    !! Goto Head. Set the iterator to the head of this sllist.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(sllist), intent(inout) :: self !! reference to this sllist.
    !/ -------------------------------------------------------------------------------------

    self%current_node => self%head_node
    self%iterator_set = .true.
    self%before_head  = .true.

  end subroutine sllist_goto_head


  !/ =======================================================================================
  function sllist_has_next( self ) result( stat )
    !/ -------------------------------------------------------------------------------------
    !! Check if the iterator can perform a next operation.
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(sllist), intent(inout) :: self !! reference to this sllist.
    logical                      :: stat !! true if a next object can be returned.
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

  end function sllist_has_next


  !/ =======================================================================================
  function sllist_get_next( self, stat, errmsg ) result( obj )
    !/ -------------------------------------------------------------------------------------
    !! Get Next. Advance the iterator to the next position in the sllist and return the
    !! unlimited polymorphic object at that position. This does not remove the entry
    !! from the sllist.
    !!
    !! |  stat  | errmsg                    |
    !! | :----: | ------------------------- |
    !! |    0   | n/a                       |
    !! |    1   | iterator in unknown state |
    !! |    2   | node data not allocated   |
    !/ -------------------------------------------------------------------------------------
    implicit none
    class(sllist),          intent(inout) :: self   !! reference to this sllist class.
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
          errmsg = 'sllist%next: iterator is in an unknown state'
       case (2)
          errmsg = 'sllist%next: node object NULL'
       case default
          errmsg = ''
       end select
    end if

  end function sllist_get_next


end module sllist_object_class


!/ =======================================================================================
!/ **                       S L L I S T _ O B J E C T _ C L A S S                       **
!/ =========================================================================== END FILE ==
