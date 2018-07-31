!/ ====================================================================== BEGIN FILE =====
!/ **                              K D T R E E _ C L A S S                              **
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
module KDTree_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-07-30
  !! license: GPL
  !!
  !! Provides a K-D Tree of k-dimentional vectors.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  integer, public :: kd_tree_visited = 0
  
  !/ =====================================================================================
  type, public :: kd_node_t
     !/ ----------------------------------------------------------------------------------
     real(dp), allocatable, dimension(:) :: x
     class(kd_node_t), pointer :: left  => null()
     class(kd_node_t), pointer :: right => null()
   contains
     final :: kd_node_destroy
  end type kd_node_t

  
  !/ =====================================================================================
  type, public :: kd_tree_t
     !/ ----------------------------------------------------------------------------------
     class(kd_node_t), pointer :: root => null()

   contains

     procedure, public :: search => kdtree_nearest

     procedure :: kdtree_insert_point
     procedure :: kdtree_insert_list
     generic,   public :: insert => kdtree_insert_point, kdtree_insert_list

     final :: kd_tree_destroy
  end type kd_tree_t


  !/ -------------------------------------------------------------------------------------
  interface KDNode
     !/ ----------------------------------------------------------------------------------
     module procedure :: kd_node_alloc
  end interface KDNode


  !/ -------------------------------------------------------------------------------------
  interface KDTree
     !/ ----------------------------------------------------------------------------------
     module procedure :: kd_tree_alloc
  end interface KDTree


  public :: KDNode
  public :: KDTree
  public :: exhaustive_search




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  recursive subroutine insert_node( root, leaf, dim, max_dim )
    !/ -----------------------------------------------------------------------------------
    !! Insert a kd_node into a tree.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(kd_node_t), pointer, intent(inout) :: root    !! root kd_node.
    class(kd_node_t), pointer, intent(in)    :: leaf    !! node to be inserted.
    integer,                   intent(in)    :: dim     !! dimension to perform difference.
    integer,                   intent(in)    :: max_dim !! number of dimensions. 
    !/ -----------------------------------------------------------------------------------
    integer :: nd
    !/ -----------------------------------------------------------------------------------

!    if ( associated( root ) ) then
!       if ( allocated( root%x ) ) then
!          if ( associated( leaf ) ) then
!             if ( allocated( leaf%x ) ) then
                nd = dim + 1
                if ( nd.gt.max_dim ) nd = 1
                if ( leaf%x(dim).lt.root%x(dim) ) then
                   if ( associated( root%left ) ) then
                      call insert_node( root%left, leaf, nd, max_dim )
                   else
                      root%left => leaf
                   end if
                else
                   if ( leaf%x(dim).gt.root%x(dim) ) then
                      if ( associated( root%right ) ) then
                         call insert_node( root%right, leaf, nd, max_dim )
                      else
                         root%right => leaf
                      end if
                   else
                      write( ERROR_UNIT, * ) 'Error * duplicate record'
                   end if
                end if
 !            else
 !               print *,'leaf has no data'
 !            end if
 !         else
 !            print *,'leaf is null'
 !         end if
 !      else
 !         print *,'root has no data'
 !      end if
 !   else
 !      print *,'root is null'
 !   end if

  end subroutine insert_node


  !/ =====================================================================================
  subroutine kdtree_insert_point( self, point )
    !/ -----------------------------------------------------------------------------------
    !! Add a single point to this KDTree
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(kd_tree_t),       intent(inout) :: self  !! Reference to this KDTree object.
    real(dp), dimension(:), intent(in)    :: point !! Data point to store.
    !/ -----------------------------------------------------------------------------------
    class(kd_node_t), pointer :: node
    integer                   :: max_dim
    !/ -----------------------------------------------------------------------------------

    max_dim = size(point)

    node => KDNode( point )

    if ( associated( self%root ) ) then
       call insert_node( self%root, node, 1, max_dim )
    else
       self%root => node
    end if
    
  end subroutine kdtree_insert_point


  !/ =====================================================================================
  subroutine kdtree_insert_list( self, list )
    !/ -----------------------------------------------------------------------------------
    !! Add a list of points to this KDTree
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(kd_tree_t),         intent(inout) :: self !! Reference to this KDTree object.
    real(dp), dimension(:,:), intent(in)    :: list !! List of data points to store.
    !/ -----------------------------------------------------------------------------------
    class(kd_node_t), pointer :: node
    integer                   :: max_dim, samples, i
    !/ -----------------------------------------------------------------------------------

    max_dim = size(list,DIM=1)
    samples = size(list,DIM=2)

    do i=1,samples
       call self%kdtree_insert_point( list(:,i) )
    end do
    
  end subroutine kdtree_insert_list




  !/ =====================================================================================
  recursive subroutine kdtree_search( root, nd, dim, max_dim, best, best_dist )
    !/ -----------------------------------------------------------------------------------
    !! Recursive search procedure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(kd_node_t), pointer,  intent(in)    :: root      !! Root node.
    type(kd_node_t),           intent(in)    :: nd        !! Test node.
    integer,                   intent(in)    :: dim       !! Dimension to test.
    integer,                   intent(in)    :: max_dim   !! Number of dimensions.
    class(kd_node_t), pointer, intent(inout) :: best      !! Best matching node.
    real(dp),                  intent(inout) :: best_dist !! Best distance
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, dx, dx2
    integer  :: next_dim
    !/ -----------------------------------------------------------------------------------

    if ( associated( root ) ) then
       d   = dist2( root%x, nd%x )
       dx  = root%x(dim) - nd%x(dim)
       dx2 = dx*dx

       kd_tree_visited = kd_tree_visited + 1

       if ( ( .not.associated(best) ).or.( d.lt.best_dist ) ) then
          best_dist =  d
          best      => root
       end if

       if ( .not.isZero( best_dist ) ) then
          next_dim = dim + 1
          if ( next_dim.gt.max_dim ) next_dim = 1

          if ( 0.0d0.lt.dx ) then
             call kdtree_search( root%left,  nd, next_dim, max_dim, best, best_dist )
          else
             call kdtree_search( root%right, nd, next_dim, max_dim, best, best_dist )
          end if

          if ( dx2.le.best_dist) then
             if ( 0.0d0.lt.dx ) then
                call kdtree_search( root%right, nd, next_dim, max_dim, best, best_dist )
             else
                call kdtree_search( root%left,  nd, next_dim, max_dim, best, best_dist )
             end if
          end if

       end if

    end if

  end subroutine kdtree_search


  !/ =====================================================================================
  subroutine kdtree_nearest( self, match, point, dist )
    !/ -----------------------------------------------------------------------------------
    !! Find the nearest matching node in the tree.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(kd_tree_t),                    intent(inout) :: self  !! Reference to this KDTree.
    real(dp), allocatable, dimension(:), intent(out)   :: match !! Data match.
    real(dp), dimension(:),              intent(in)    :: point !! Test point.
    real(dp), optional,                  intent(out)   :: dist  !! Distance between data and test.
    !/ -----------------------------------------------------------------------------------
    class(kd_node_t), pointer :: found, test
    real(dp)                  :: min_dist
    integer                   :: kdim
    !/ -----------------------------------------------------------------------------------

    test => KDNode( point )
    kdim =  size(point)

    min_dist = 1.0d20
    call kdtree_search( self%root, test, 1, kdim, found, min_dist )

    if ( associated(found) ) match = found%x
    if ( present( dist ) )   dist  = sqrt(min_dist)
  end subroutine kdtree_nearest


  !/ =====================================================================================
  function kd_node_alloc( x ) result( new_node )
    !/ -----------------------------------------------------------------------------------
    !! Allocate a KD_Node.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:), intent(in) :: x        !! Data point.
    class(kd_node_t), pointer          :: new_node !! Return a new Node.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(x)
    allocate( new_node )
    allocate( new_node%x(n) )

    do i=1,n
       new_node%x(i) = x(i)
    end do
    
    nullify( new_node%left )
    nullify( new_node%right )
    
  end function kd_node_alloc


  !/ =====================================================================================
  subroutine kd_node_destroy( self )
    !/ -----------------------------------------------------------------------------------
    !! Deallocate memory.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(kd_node_t), intent(inout) :: self !! Reference to this node.
    !/ -----------------------------------------------------------------------------------
    
    if ( allocated( self%x ) ) deallocate( self%x )
    nullify( self%left )
    nullify( self%right )
    
  end subroutine kd_node_destroy


  !/ =====================================================================================
  function kd_tree_alloc( point, list ) result( new_tree )
    !/ -----------------------------------------------------------------------------------
    !! Allocate a new Tree. Empty, single point or list of points.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), optional, dimension(:),   intent(in) :: point !! optional point
    real(dp), optional, dimension(:,:), intent(in) :: list  !! optional list
    !/ -----------------------------------------------------------------------------------
    class(kd_tree_t), pointer :: new_tree
    !/ -----------------------------------------------------------------------------------

    allocate( new_tree )

    if ( present( point ) ) then
       call new_tree%kdtree_insert_point( point )
    end if

    if ( present( list ) ) then
       call new_tree%kdtree_insert_list( list )
    end if

  end function kd_tree_alloc


  !/ =====================================================================================
  recursive subroutine recursive_node_delete( root )
    !/ -----------------------------------------------------------------------------------
    !! Recursive node deletion. Remove all nodes from a tree.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(kd_node_t), pointer, intent(inout) :: root !! root node of a tree.
    !/ -----------------------------------------------------------------------------------

    if ( associated( root ) ) then
       if ( associated( root%left ) ) then
          call recursive_node_delete( root%left )
          nullify( root%left )
       end if
       if ( associated( root%right ) ) then
          call recursive_node_delete( root%right )
          nullify( root%right )
       end if
       deallocate( root )
       nullify( root )
    end if
    
  end subroutine recursive_node_delete


  !/ =====================================================================================
  subroutine kd_tree_destroy( self )
    !/ -----------------------------------------------------------------------------------
    !! Remove all nodes from this tree.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(kd_tree_t), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------

    call recursive_node_delete( self%root )

  end subroutine kd_tree_destroy


  !/ =====================================================================================
  function exhaustive_search( list, test, dist ) result( idx )
    !/ -----------------------------------------------------------------------------------
    !! Helper function used for testing.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), dimension(:,:), intent(in)  :: list
    real(dp), dimension(:),   intent(in)  :: test
    real(dp), optional,       intent(out) :: dist
    integer                               :: idx
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, min_dist
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    min_dist = 1.0d20
    idx      = 0

    n = size(list,DIM=2)
    do i=1,n
       d = dist2( list(:,i), test )
       if ( d .lt. min_dist ) then
          min_dist = d
          idx      = i
       end if
    end do

    if ( present( dist ) ) dist = sqrt(min_dist)

  end function exhaustive_search


end module KDTree_class


!/ =======================================================================================
!/ **                              K D T R E E _ C L A S S                              **
!/ =========================================================================== END FILE ==
