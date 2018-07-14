!/ ====================================================================== BEGIN FILE =====
!/ **                               B T R E E _ C L A S S                               **
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
module btree_object_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-04
  !! license: GPL
  !!
  !! Provides a binary search tree of unlimited polymorphic objects.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private


  !/ =====================================================================================
  type, public :: btree_node
     !/ ----------------------------------------------------------------------------------
     !! BTree Node.
     !/ ----------------------------------------------------------------------------------

     class(*),          pointer :: key    => null() !! key object.
     class(*),          pointer :: object => null() !! storage object.
     class(btree_node), pointer :: left   => null() !! pointer to the left  child node.
     class(btree_node), pointer :: right  => null() !! pointer to the right child node.


   contains

     procedure, public :: delete => btree_node_delete

     final :: btree_node_destroy

  end type btree_node

  
  !/ =====================================================================================
  type, public :: btree_node_pointer
     !/ ----------------------------------------------------------------------------------
     class(btree_node), pointer :: ptr
  end type btree_node_pointer


  !/ =====================================================================================
  type, public :: BTree
     !/ ----------------------------------------------------------------------------------
     !! BTree Structure.
     !/ ----------------------------------------------------------------------------------

     class(btree_node),        pointer                   :: root_node => null()
     type(btree_node_pointer), allocatable, dimension(:) :: table
     integer                                             :: count   = 0
     integer                                             :: current = 0
     logical                                             :: needs_index_rebuild = .true.

   contains

     procedure :: internal_build_index

     procedure, public  :: isEmpty    => btree_is_empty
     procedure, public  :: size       => btree_size
     procedure, public  :: insert     => btree_insert_object
     procedure, public  :: find       => btree_find_object
     procedure, public  :: hasKey     => btree_has_key
     procedure, public  :: execute    => btree_execute_lcr_node
     procedure, public  :: buildIndex => btree_build_index
     procedure, public  :: index      => btree_get_object_at_index

     ! built-in iterator

     procedure, public :: rewind   => btree_iter_rewind
     procedure, public :: hasNext  => btree_iter_has_next
     procedure, public :: next     => btree_iter_next
     procedure, public :: nextNode => btree_iter_next_node
     procedure, public :: nextKey  => btree_iter_next_key
     

     final :: btree_destroy
  end type BTree




  !/ -------------------------------------------------------------------------------------
  interface btree_node
     !/ ----------------------------------------------------------------------------------
     module procedure :: btree_node_alloc
  end interface btree_node




  !/ -------------------------------------------------------------------------------------
  abstract interface
     subroutine btree_node_procedure( key, obj )
       !/ --------------------------------------------------------------------------------
       class(*), pointer, intent(in) :: key !! pointer to a key.
       class(*), pointer, intent(in) :: obj !! pointer to an object.
     end subroutine btree_node_procedure
  end interface

  public :: btree_node_procedure




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function btree_node_alloc( key, obj ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Initialize a new btree node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), pointer, intent(in) :: key  !! pointer to a key.
    class(*), pointer, intent(in) :: obj  !! pointer to an object.
    class(btree_node), pointer    :: node !! new node structure.
    !/ -----------------------------------------------------------------------------------

    allocate( node )

    allocate( node%key, source=key )
    node%object => obj
    nullify( node%left )
    nullify( node%right )    

  end function btree_node_alloc


  !/ =====================================================================================
  subroutine btree_node_delete( self, delobj )
    !/ -----------------------------------------------------------------------------------
    !! Clean up the btree node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(btree_node), intent(inout) :: self   !! reference to this btree node class.
    logical, optional, intent(in)    :: delobj !! if true then dispose the stored object.
    !/ -----------------------------------------------------------------------------------

    if ( present( delobj ) ) then
       if ( delobj ) then
          deallocate( self%key )
          deallocate( self%object )
       end if
    end if

    nullify( self%key  )
    nullify( self%object  )
    nullify( self%left )
    nullify( self%right )

  end subroutine btree_node_delete


  !/ =====================================================================================
  subroutine btree_node_destroy( node )
    !/ -----------------------------------------------------------------------------------
    !! BTree node destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(btree_node) :: node !! new node structure.
    !/ -----------------------------------------------------------------------------------

    call node%delete( .true. )

  end subroutine btree_node_destroy


  !/ =====================================================================================
  subroutine btree_destroy( tree )
    !/ -----------------------------------------------------------------------------------
    !! BTree destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(BTree) :: tree !! new btree structure.
    !/ -----------------------------------------------------------------------------------

    !call queue%clear( .true. )

  end subroutine btree_destroy


  !/ =====================================================================================
  pure function btree_is_empty( self ) result( stat )
    !/ -----------------------------------------------------------------------------------
    !! Determine if the tree is empty.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree), intent(in) :: self !! reference to this btree.
    logical                  :: stat !! true if the tree is empty.
    !/ -----------------------------------------------------------------------------------

    stat = .true.
    if ( associated( self%root_node ) ) then
       stat = .false.
    end if

  end function btree_is_empty


  !/ =====================================================================================
  pure function btree_size( self ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of objects stored in this btree.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree), intent(in) :: self !! reference to this btree.
    integer                  :: n    !! number of stored objects.
    !/ -----------------------------------------------------------------------------------

    n = self%count

  end function btree_size


  !/ =====================================================================================
  recursive subroutine recursive_insert_node( root, node )
    !/ -----------------------------------------------------------------------------------
    !! Internal procedure to recursivly insert nodes.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(btree_node),          intent(inout) :: root !! insert new node here.
    type(btree_node), pointer, intent(in)    :: node !! new node.
    !/ -----------------------------------------------------------------------------------

    if ( 0 .gt. compare( node%key, root%key ) ) then
       if ( associated( root%left ) ) then
          call recursive_insert_node( root%left, node )
       else
          root%left => node
       end if
    else
       if ( associated( root%right ) ) then
          call recursive_insert_node( root%right, node )
       else
          root%right => node
       end if
    end if

  end subroutine recursive_insert_node


  !/ =====================================================================================
  subroutine btree_insert_object( self, key, obj )
    !/ -----------------------------------------------------------------------------------
    !! Push an unlimited polymorphic object onto the root of the tree.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),      intent(inout) :: self !! reference to this btree class.
    class(*), pointer, intent(in)    :: key  !! btree key.
    class(*), pointer, intent(in)    :: obj  !! btree data.
    !/ -----------------------------------------------------------------------------------
    class(btree_node), pointer :: new_node
    !/ -----------------------------------------------------------------------------------

    new_node => btree_node( key, obj )
    self%count = self%count + 1

    if ( associated( self%root_node ) ) then
       call recursive_insert_node( self%root_node, new_node )
    else
       self%root_node => new_node
    end if

    self%needs_index_rebuild = .true.

  end subroutine btree_insert_object


  !/ =====================================================================================
  recursive function recursive_find_node( root, key, stat ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Find a node with a matching key. This is a private internal recursive function.
    !!
    !! |  stat  | errmsg        |
    !! | :----: | ------------- |
    !! |    0   | n/a           |
    !! |    1   | no node found |
    !! |    2   | key is NULL   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(btree_node), pointer, intent(in)  :: root !! search new node here.
    class(*),          pointer, intent(in)  :: key  !! btree key.
    integer,                    intent(out) :: stat !! return status
    type(btree_node),  pointer              :: node !! start execution here.
    !/ -----------------------------------------------------------------------------------
    integer :: c

    stat = 0

    nullify( node )

    if ( associated( root ) ) then
       if ( associated( key ) ) then
          c = compare( key, root%key )
          select case(c)
          case(:-1) ! key < root
             node => recursive_find_node( root%left, key, stat )
          case(0)   ! key = root
             node => root
             stat =  0
          case(1:)  ! key > root
             node => recursive_find_node( root%right, key, stat )
          end select
       else
          stat = 2
       end if
    else
       stat = 1
    end if

  end function recursive_find_node


  !/ =====================================================================================
  function btree_has_key( self, key, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! test for an association between a key and an object.
    !! |  stat  | errmsg         |
    !! | :----: | -------------- |
    !! |    0   | n/a            |
    !! |    1   | key is NULL    |
    !! |    2   | no chain found |
    !! |    3   | no node found  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),    intent(inout) :: self !! reference to this object
    class(*), pointer, intent(in)  :: key  !! key to hash
    integer, optional, intent(out) :: stat !! optional return status
    logical                        :: res  !! true if object is assoiated with key
    !/ -----------------------------------------------------------------------------------
    integer                    :: istat
    class(btree_node), pointer :: node
    !/ -----------------------------------------------------------------------------------

    res = .false.
    node => self%find( key, istat )
    if ( 0.eq.istat ) res = .true.
    
    if ( present( stat ) ) stat = istat
  end function btree_has_key
    

  !/ =====================================================================================
  function btree_find_object( self, key, stat ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Find a node with a matching key.
    !!
    !! |  stat  | errmsg        |
    !! | :----: | ------------- |
    !! |    0   | n/a           |
    !! |    1   | no node found |
    !! |    2   | key is NULL   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),               intent(inout) :: self !! reference to this btree class.
    class(*),         pointer,  intent(in)    :: key  !! btree key.
    integer,          optional, intent(out)   :: stat !! optional return status
    type(btree_node), pointer                 :: node !! start execution here.
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    istat = 0

    node => recursive_find_node( self%root_node, key, istat )

    if ( present( stat ) ) stat = istat

  end function btree_find_object


  !/ =====================================================================================
  recursive subroutine recursive_execute_node( node, proc )
    !/ -----------------------------------------------------------------------------------
    !! Execute a function on every node of this tree using in order traversal.
    !! This is a recursive private internal function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(btree_node),                pointer, intent(in) :: node !! start execution here.
    procedure(btree_node_procedure), pointer, intent(in) :: proc !! pointer to procedure.
    !/ -----------------------------------------------------------------------------------

    if ( associated( node ) ) then

       if ( associated( node%left ) ) then
          call recursive_execute_node( node%left, proc )
       end if

       call proc( node%key, node%object )

       if ( associated( node%right ) ) then
          call recursive_execute_node( node%right, proc )
       end if

    end if

  end subroutine recursive_execute_node


  !/ =====================================================================================
  subroutine btree_execute_lcr_node( self, proc )
    !/ -----------------------------------------------------------------------------------
    !! Execute a function on every node of this tree using in order traversal.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),                             intent(inout) :: self !! this btree class.
    procedure(btree_node_procedure), pointer, intent(in)    :: proc !! pointer to procedure.
    !/ -----------------------------------------------------------------------------------

    call recursive_execute_node( self%root_node, proc )

  end subroutine btree_execute_lcr_node


  !/ =====================================================================================
  recursive subroutine internal_build_index( self, node )
    !/ -----------------------------------------------------------------------------------
    !! Build an internal index that may be retrieved as an array with an index.
    !! This is a recursive private internal function.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),              intent(inout) :: self !! reference to this btree class.
    type(btree_node), pointer, intent(in)    :: node
    !/ -----------------------------------------------------------------------------------

    if ( associated( node ) ) then

       if ( associated( node%left ) ) then
          call self%internal_build_index( node%left )
       end if

       self%table(self%current)%ptr => node
       self%current = self%current + 1

       if ( associated( node%right ) ) then
          call self%internal_build_index( node%right )
       end if

    end if

  end subroutine internal_build_index


  !/ =====================================================================================
  subroutine btree_build_index( self )
    !/ -----------------------------------------------------------------------------------
    !! Build an in-order index of the objects in this tree
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------

    if ( self%needs_index_rebuild ) then
       
       allocate( self%table( self%count ) )

       self%current = 1

       call self%internal_build_index( self%root_node )

       self%needs_index_rebuild = .false.
    end if

  end subroutine btree_build_index



  !/ =====================================================================================
  function btree_get_object_at_index( self, n, stat ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! get the object stored at an in-order position.
    !!
    !! |  stat  | errmsg                             |
    !! | :----: | ---------------------------------- |
    !! |    0   | n/a                                |
    !! |    1   | btree index needs to be rebuilt    |
    !! |    2   | in-order position is out of bounds |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),       intent(in)  :: self !! reference to this btree
    integer,            intent(in)  :: n    !! in-order position
    integer,  optional, intent(out) :: stat !! optional return status code
    class(btree_node),  pointer     :: node !! pointer to the returned btree node.
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0

    nullify( node )

    if ( self%needs_index_rebuild ) then
       istat = 1
    else
       if ( ( n.gt.self%count ).or.( 0.gt.n ) ) then
          istat = 2
       else
          node => self%table(n)%ptr
       end if
    end if

    if ( present( stat ) ) stat = istat
  end function btree_get_object_at_index






  !/ =====================================================================================
  subroutine btree_iter_rewind( self )
    !/ -----------------------------------------------------------------------------------
    !! Rewind the internal iterator.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree), intent(inout) :: self !! reference to this btree
    !/ -----------------------------------------------------------------------------------

    if ( self%needs_index_rebuild ) then
       call self%buildIndex
    end if

    self%current = 1

  end subroutine btree_iter_rewind


  !/ =====================================================================================
  function btree_iter_has_next( self, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Can this BTree's internal iterator return more nodes?
    !!
    !! |  stat  | errmsg                          |
    !! | :----: | ------------------------------- |
    !! |    0   | n/a                             |
    !! |    1   | btree index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),      intent(in)  :: self  !! reference to this btree
    integer, optional, intent(out) :: stat  !! optional retuen status
    logical                        :: res   !! true if there are more nodes to be returned
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------

    istat = 0
    res   = .false.
    if ( self%needs_index_rebuild ) then
       istat = 1
    else
       if ( self%current.gt.self%count ) then
          istat = 2
       else
          res = .true.
       end if
    end if

    if ( present( stat ) ) stat = istat

  end function btree_iter_has_next



  !/ =====================================================================================
  function btree_iter_next_node( self, stat ) result( node )
    !/ -----------------------------------------------------------------------------------
    !! Attempt to return a pointer to the next node in this BTree's internal iterattor.
    !!
    !! |  stat  | errmsg                          |
    !! | :----: | ------------------------------- |
    !! |    0   | n/a                             |
    !! |    1   | btree index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),      intent(inout) :: self  !! reference to this btree
    integer, optional, intent(out)   :: stat  !! optional retuen status
    class(btree_node), pointer       :: node  !! pointer to the next node
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    !/ -----------------------------------------------------------------------------------
    
    istat = 0
    nullify( node )
    
    if ( self%needs_index_rebuild ) then
       istat = 1
    else
       if ( self%current.gt.self%count ) then
          istat = 2
       else
          node => self%table( self%current )%ptr
          self%current = self%current + 1
       end if
    end if

    if ( present( stat ) ) stat = istat

  end function btree_iter_next_node


  !/ =====================================================================================
  function btree_iter_next( self, stat ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Attempt to return a pointer to the next stored object in this BTree's
    !! internal iterattor.
    !!
    !! |  stat  | errmsg                          |
    !! | :----: | ------------------------------- |
    !! |    0   | n/a                             |
    !! |    1   | btree index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),      intent(inout) :: self  !! reference to this btree
    integer, optional, intent(out)   :: stat  !! optional retuen status
    class(*), pointer                :: obj   !! pointer to the object
    !/ -----------------------------------------------------------------------------------
    type(btree_node), pointer :: bnode
    !/ -----------------------------------------------------------------------------------

    nullify( obj )
    bnode => self%nextNode(stat)
    if ( associated( bnode ) ) obj => bnode%object

  end function btree_iter_next


  !/ =====================================================================================
  function btree_iter_next_key( self, stat ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Attempt to return a pointer to the next key in this BTree's
    !! internal iterattor.
    !!
    !! |  stat  | errmsg                          |
    !! | :----: | ------------------------------- |
    !! |    0   | n/a                             |
    !! |    1   | btree index needs to be rebuilt |
    !! |    2   | no more nodes may be retrievied |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(BTree),      intent(inout) :: self  !! reference to this btree
    integer, optional, intent(out)   :: stat  !! optional retuen status
    class(*), pointer                :: obj   !! pointer to the key
    !/ -----------------------------------------------------------------------------------
    type(btree_node), pointer :: bnode
    !/ -----------------------------------------------------------------------------------

    nullify( obj )
    bnode => self%nextNode(stat)
    if ( associated( bnode ) ) obj => bnode%key

  end function btree_iter_next_key


end module btree_object_class


!/ =======================================================================================
!/ **                               B T R E E _ C L A S S                               **
!/ =========================================================================== END FILE ==
