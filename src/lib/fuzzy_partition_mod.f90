!/ ====================================================================== BEGIN FILE =====
!/ **                       F U Z Z Y _ P A R T I T I O N _ M O D                       **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  Europa is free software: you can redistribute it and/or modify it under the      **
!/ **  terms of the GNU General Public License as published by the Free Software        **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  Europa is distributed in the hope that it will be useful, but WITHOUT ANY        **
!/ **  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR    **
!/ **  A PARTICULAR PURPOSE. See the GNU General Public License for more details.       **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  Europa. If not, see <http://www.gnu.org/licenses/>.                              **
!/ **                                                                                   **
!/ =======================================================================================
module fuzzy_partition_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides the interface for a fuzzy partition.  R -> R^n
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-12-28
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use fuzzy_set_mod
  use string_tools
  use tlogger
  implicit none


  !/ =====================================================================================
  type :: FuzzyPartition
     !/ ----------------------------------------------------------------------------------
     ! Provides the interface for a fuzzy partition.  R -> R^n
     !
     !   NEG    SN  ZERO  SP    POS
     !  ____                    ____ 1
     !      \   /\   /\   /\   /
     !       \ /  \ /  \ /  \ /
     !        X    X    X    X          This partition contains 5 fuzzy functions
     !       / \  / \  / \  / \         1 LeftTrapezoid 3 Triangle, and 1 RightTrapezoid
     !  ____/   \/   \/   \/   \____
     !  ============================ 0
     !/ ----------------------------------------------------------------------------------

     type(FuzzySet_ptr), allocatable :: fset(:)          !! array  of fuzzy sets.
     integer                         :: num_set =  0     !! number of fuzzy sets
     real(dp)                        :: min_ctr = -D_ONE !! minimum center value.
     real(dp)                        :: max_ctr =  D_ONE !! maximum center value.

   contains

     procedure, private :: fp_init_default
     procedure, private :: fp_init_left_right
     procedure, private :: fp_init_params
     procedure, private :: fp_resize
     procedure, private :: fp_balance

     procedure :: copy      => fp_copy
     procedure :: clone     => fp_clone

     procedure :: set       => fp_get_set
     procedure :: nIn       => fp_get_number_inputs
     procedure :: nOut      => fp_get_number_outputs
     procedure :: size      => fp_get_storage_size

     procedure :: getCenter => fp_get_center
     procedure :: mu        => fp_mu
     procedure :: area      => fp_area
     procedure :: coa       => fp_coa

     procedure :: load      => fp_load
     procedure :: store     => fp_store
     procedure :: read      => fp_read
     procedure :: write     => fp_write

     procedure :: destroy   => fp_destroy

     generic :: init => fp_init_default, fp_init_left_right, fp_init_params

     final :: fp_final

  end type FuzzyPartition


  !/ =====================================================================================
  type :: FuzzyPartition_ptr
     !/ ----------------------------------------------------------------------------------
     class(FuzzyPartition), pointer :: ptr => null()
  end type FuzzyPartition_ptr


  !/ =====================================================================================
  interface FuzzyPartition
     !/ ----------------------------------------------------------------------------------
     module procedure :: fp_create_default
     module procedure :: fg_create_left_right
     module procedure :: fp_create_params
  end interface FuzzyPartition




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function fp_clone( dts ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Clone this FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to this FuzzyPartition.
    class(FuzzyPartition), pointer       :: ptr !! ponter to a new FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    call ptr%copy( dts )
  end function fp_clone


  !/ =====================================================================================
  subroutine fp_copy( dts, src )
    !/ -----------------------------------------------------------------------------------
    !! Clone this FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to this FuzzyPartition.
    type(FuzzyPartition),  intent(in)    :: src !! reference to a source FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    call dts%fp_resize(src%num_set)
    do i=1,src%num_set
       if ( associated( dts%fset(i)%ptr ) ) then
          call dts%fset(i)%ptr%copy(src%fset(i)%ptr)
       else
          dts%fset(i)%ptr => src%fset(i)%ptr%clone()
       end if
    end do
  end subroutine fp_copy


  !/ =====================================================================================
  function fp_create_default( n ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The left is -1 and the right is +1
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,               intent(in) :: n   !! number of fuzzy sets.
    class(FuzzyPartition), pointer    :: ptr !! pointer to a new FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    call ptr%init(n)
  end function fp_create_default


  !/ =====================================================================================
  function fg_create_left_right( n, mn, mx ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The centers are computed from the left
    !! and right extents and the number of sets.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,               intent(in) :: n   !! number of fuzzy sets,
    real(dp),              intent(in) :: mn  !! left  most center.
    real(dp),              intent(in) :: mx  !! right most center.
    class(FuzzyPartition), pointer    :: ptr !! pointer to a new FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    call ptr%init(n,mn,mx)
  end function fg_create_left_right


  !/ =====================================================================================
  function fp_create_params(ctrs ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The centers are defined by (ctrs) the left
    !! and right extents are defined by the centers of the sets to the left and right.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),              intent(in) :: ctrs(:) !! real array of center values.
    class(FuzzyPartition), pointer    :: ptr     !! pointer to a new FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    call ptr%init(ctrs)
  end function fp_create_params


  !/ =====================================================================================
  subroutine fp_init_default( dts, n )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The left is -1 and the right is +1.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer,               intent(in)    :: n   !! number of fuzzy sets.
    !/ -----------------------------------------------------------------------------------
    call dts%fp_resize(n)
    dts%min_ctr = -D_ONE
    dts%max_ctr =  D_ONE
    call dts%fp_balance
  end subroutine fp_init_default


  !/ =====================================================================================
  subroutine fp_init_left_right( dts, n, mn, mx )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The centers are computed from the left
    !! and right extents and the number of sets.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer,               intent(in)    :: n   !! number of fuzzy sets.
    real(dp),              intent(in)    :: mn  !! left  most center.
    real(dp),              intent(in)    :: mx  !! right most center.
    !/ -----------------------------------------------------------------------------------
    call dts%fp_resize(n)
    dts%min_ctr = mn
    dts%max_ctr = mx
    call dts%fp_balance
  end subroutine fp_init_left_right


  !/ =====================================================================================
  subroutine fp_init_params( dts, ctrs )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The centers are defined by (ctrs) the left
    !! and right extents are defined by the centers of the sets to the left and right.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts     !! reference to a FuzzyPartition.
    real(dp),              intent(in)    :: ctrs(:) !! real array of center values.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: mid
    !/ -----------------------------------------------------------------------------------
    n = size(ctrs)

    call dts%fp_resize(n)

    if ( 1.gt.dts%num_set ) then
       call log_error( 'FuzzyPartition%init: resize should have rejected this' )
       goto 999
    end if

    if      ( 1.eq.dts%num_set ) then !/ ----- special case n=1 --------------------------
       !/ set one triangle set

       dts%min_ctr = ctrs(1) - D_ONE
       dts%max_ctr = ctrs(1) + D_ONE
       call dts%fset(1)%ptr%set( dts%min_ctr, ctrs(1), dts%max_ctr )

    else if ( 2.eq.dts%num_set ) then !/ ----- special case n=2 --------------------------
       !/ set a left and right pair of trapezoid sets.

       dts%min_ctr = ctrs(1)
       dts%max_ctr = ctrs(2)
       call dts%fset(1)%ptr%set( dts%min_ctr, dts%max_ctr )
       call dts%fset(2)%ptr%set( dts%min_ctr, dts%max_ctr )

    else if ( 3.eq.dts%num_set ) then !/ ----- special case n=3 --------------------------
       !/ set a trapezoid-triangle-trapezoid classic combination.

       dts%min_ctr = ctrs(1)
       mid         = ctrs(2)
       dts%max_ctr = ctrs(3)
       call dts%fset(1)%ptr%set( dts%min_ctr, mid              )
       call dts%fset(2)%ptr%set( dts%min_ctr, mid, dts%max_ctr )
       call dts%fset(3)%ptr%set(              mid, dts%max_ctr )

    else                              !/ ----- general case n>3 --------------------------

       call dts%fset(1)%ptr%set( ctrs(1), ctrs(2) )

       do i=2,dts%num_set-1
          call dts%fset(i)%ptr%set( ctrs(i-1), ctrs(i), ctrs(i+1) )
       end do

       call dts%fset(dts%num_set)%ptr%set( ctrs(dts%num_set-1), ctrs(dts%num_set) )

    end if

999 continue

  end subroutine fp_init_params


  !/ =====================================================================================
  subroutine fp_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Deallocate storage for this FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( allocated( dts%fset ) ) then
       do i=1,dts%num_set
          if ( associated( dts%fset(i)%ptr ) ) then
             deallocate( dts%fset(i)%ptr )
          end if
          nullify( dts%fset(i)%ptr )
       end do
       deallocate( dts%fset )
    end if
    dts%num_set = 0

  end subroutine fp_destroy


  !/ =====================================================================================
  subroutine fp_final( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    call dts%destroy
  end subroutine fp_final


  !/ =====================================================================================
  subroutine fp_resize( dts, n, CHANGED )
    !/ -----------------------------------------------------------------------------------
    !! Allocate a list of FuzzySet.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts     !! reference to a FuzzyPartition.
    integer,               intent(in)    :: n       !! change the number of sets.
    logical, optional,     intent(out)   :: CHANGED !! true if the number of sets changed.
    !/ -----------------------------------------------------------------------------------
    logical :: chg
    integer :: i
    !/ -----------------------------------------------------------------------------------

    chg = .false.

    if ( 1.gt.n ) then
       call log_error( 'FuzzyPartition%resize: There needs to be at least one function only got', I4=n )
       goto 999
    end if

    if ( n.eq.dts%num_set ) then
       call log_debug( 'FuzzyPartition%resize: Resize called with no change' )
       goto 999
    end if

    call dts%destroy

    allocate( dts%fset(n) )

    chg = .true.

    dts%num_set = n

    !print *, 'Create partition with',n,'sets'

    if      ( 1.eq.dts%num_set ) then !/ ----- special case n=1 --------------------------
       !/ set one triangle set

       !allocate( class(TriangleSet) :: dts%fset(1)%ptr )
       dts%fset(1)%ptr => TriangleSet()

    else if ( 2.eq.dts%num_set ) then !/ ----- special case n=2 --------------------------
       !/ set a left and right pair of trapezoid sets.

       !allocate( class(LeftTrapezoidSet)  :: dts%fset(1)%ptr )
       !allocate( class(RightTrapezoidSet) :: dts%fset(2)%ptr )

       dts%fset(1)%ptr => LeftTrapezoidSet()
       dts%fset(2)%ptr => RightTrapezoidSet()

    else if ( 3.eq.dts%num_set ) then !/ ----- special case n=3 --------------------------
       !/ set a trapezoid-triangle-trapezoid classic combination.

       !allocate( class(LeftTrapezoidSet)  :: dts%fset(1)%ptr )
       !allocate( class(TriangleSet)       :: dts%fset(2)%ptr )
       !allocate( class(RightTrapezoidSet) :: dts%fset(3)%ptr )

       dts%fset(1)%ptr => LeftTrapezoidSet()
       dts%fset(2)%ptr => TriangleSet()
       dts%fset(3)%ptr => RightTrapezoidSet()

    else                              !/ ----- general case n>3 --------------------------

       !allocate( class(LeftTrapezoidSet)  :: dts%fset(1)%ptr )
       dts%fset(1)%ptr => LeftTrapezoidSet()
       do i=2,n-1
          !allocate( class(TriangleSet)       :: dts%fset(i)%ptr )
          dts%fset(i)%ptr => TriangleSet()
       end do
       !allocate( class(RightTrapezoidSet) :: dts%fset(n)%ptr )
       dts%fset(n)%ptr => RightTrapezoidSet()

    end if

999 continue
    if ( present( CHANGED ) ) CHANGED = chg

  end subroutine fp_resize


  !/ =====================================================================================
  subroutine fp_balance( dts )
    !/ -----------------------------------------------------------------------------------
    !! Balance the centers such that the are evenly distributed between min_ctr & max_ctr.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    real(dp) :: mid, delta, L, C, R
    !/ -----------------------------------------------------------------------------------

    if      ( 1.gt.dts%num_set ) then !/ ----- this is an error --------------------------
       call log_error( 'FuzzyPartition%balance: resize should have rejected this' )

    else if ( 1.eq.dts%num_set ) then !/ ----- special case n=1 --------------------------
       !/ set one triangle set
       mid = D_HALF * ( dts%min_ctr + dts%max_ctr )
       call dts%fset(1)%ptr%set( dts%min_ctr, mid, dts%max_ctr )

    else if ( 2.eq.dts%num_set ) then !/ ----- special case n=2 --------------------------
       !/ set a left and right pair of trapezoid sets.

       call dts%fset(1)%ptr%set( dts%min_ctr, dts%max_ctr )
       call dts%fset(2)%ptr%set( dts%min_ctr, dts%max_ctr )

    else if ( 3.eq.dts%num_set ) then !/ ----- special case n=3 --------------------------
       !/ set a trapezoid-triangle-trapezoid classic combination.
       mid = D_HALF * ( dts%min_ctr + dts%max_ctr )
       call dts%fset(1)%ptr%set( dts%min_ctr, mid              )
       call dts%fset(2)%ptr%set( dts%min_ctr, mid, dts%max_ctr )
       call dts%fset(3)%ptr%set(              mid, dts%max_ctr )

    else                              !/ ----- general case n>3 --------------------------
       delta = ( dts%max_ctr - dts%min_ctr ) / real(dts%num_set-1,dp)
       L = dts%min_ctr
       C = L + delta
       R = C + delta

       call dts%fset(1)%ptr%set( L, C )

       do i=2,dts%num_set-1
          call dts%fset(i)%ptr%set( L, C, R )
          L = C
          C = R
          R = R + delta
       end do

       call dts%fset(dts%num_set)%ptr%set( L, C )
    end if
  end subroutine fp_balance


  !/ =====================================================================================
  function fp_get_set( dts, idx ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Get a FuzzySet form this FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer,               intent(in)    :: idx !! FuzzySet index.
    class(FuzzySet),       pointer       :: ptr !! pointer to the indexed FuzzySet.
    !/ -----------------------------------------------------------------------------------
    nullify( ptr )
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(idx)%ptr ) ) then
          ptr => dts%fset(idx)%ptr
       else
          call log_error( 'FuzzyPartition: fset pointer not assigned', I4=idx )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_get_set


  !/ =====================================================================================
  function fp_get_number_inputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of inputs to this FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer                              :: n   !! number of inputs.
    !/ -----------------------------------------------------------------------------------
    n = 1
  end function fp_get_number_inputs


  !/ =====================================================================================
  function fp_get_number_outputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !!Number of outputs to this FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer                              :: n   !! number of outputs.
    !/ -----------------------------------------------------------------------------------
    n = dts%num_set
  end function fp_get_number_outputs


  !/ =====================================================================================
  function fp_get_storage_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer                              :: n   !! number of parameters.
    !/ -----------------------------------------------------------------------------------
    n = dts%num_set
  end function fp_get_storage_size


  !/ =====================================================================================
  function fp_get_center( dts, idx ) result( c )
    !/ -----------------------------------------------------------------------------------
    !! Get an indexed parameter.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer,               intent(in)    :: idx !! index.
    real(dp)                             :: c   !! indexed center.
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(idx)%ptr ) ) then
          c = dts%fset(idx)%ptr%getCenter()
       else
          call log_error( 'FuzzyPartition: fset pointer not assigned', I4=idx )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_get_center


  !/ =====================================================================================
  subroutine fp_mu( dts, m, x )
    !/ -----------------------------------------------------------------------------------
    !! Compute the degree of membership for each FuzzySet based on a crisp value x.
    !! The domain is all real numbers. The range is 0 to 1 inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts  !! reference to a FuzzyPartition.
    real(dp),              intent(out)   :: m(:) !! degree of membership list.
    real(dp),              intent(in)    :: x    !! crisp value.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(1)%ptr ) ) then
          do i=1,dts%num_set
             m(i) = dts%fset(i)%ptr%mu(x)
          end do
       else
          call log_error( 'FuzzyPartition: fset pointers not assigned' )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end subroutine fp_mu


  !/ =====================================================================================
  function fp_area( dts, deg ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Compute the area under the degree of memberships for this FuzzyPartition.
    !! The domain is 0 to 1 inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts    !! reference to a FuzzyPartition.
    real(dp),              intent(in)    :: deg(:) !! degree of membership list.
    real(dp)                             :: a      !! total area under the membership.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(1)%ptr ) ) then
          a = D_ZERO
          do i=1,dts%num_set
             a = a + dts%fset(i)%ptr%area(deg(i))
          end do
       else
          call log_error( 'FuzzyPartition: fset pointers not assigned' )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_area


  !/ =====================================================================================
  function fp_coa( dts, deg ) result( c )
    !/ -----------------------------------------------------------------------------------
    !! Compute the center of area based on the degrees of membership in this FuzzyPartition. 
    !! The domain is 0 to 1 inclusive. The range is (left) to (right) inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts    !! reference to a FuzzyPartition.
    real(dp),              intent(in)    :: deg(:) !! degree of membership list.
    real(dp)                             :: c      !! center of area.
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    real(dp) :: a, x, sum_a, sum_ax
    !/ -----------------------------------------------------------------------------------
    c = 0
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(1)%ptr ) ) then
          sum_ax = D_EPSILON
          sum_a  = D_EPSILON
          do i=1,dts%num_set
             a = dts%fset(i)%ptr%area(deg(i))
             x = dts%fset(i)%ptr%coa(deg(i))
             sum_ax = sum_ax + ( a * x )
             sum_a  = sum_a  +   a
          end do
          c = sum_ax / sum_a
       else
          call log_error( 'FuzzyPartition: fset pointers not assigned' )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_coa


  !/ =====================================================================================
  function fp_load( dts, buffer, INDEX ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Load the parameters for this FuzzyPartition from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts       !! reference to a FuzzyPartition.
    real(dp),              intent(in)    :: buffer(:) !! buffer for the parameters.
    integer, optional,     intent(in)    :: INDEX     !! index of starting parameter.
    integer                              :: post_idx  !! next available index.
    !/ -----------------------------------------------------------------------------------
    integer :: s
    !/ -----------------------------------------------------------------------------------
    s = 1
    if ( present(INDEX) ) s = INDEX
    post_idx = s + dts%num_set

    call dts%init(buffer(s:post_idx-1))

  end function fp_load


  !/ =====================================================================================
  function fp_store( dts, buffer, INDEX ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Store the parameters for this FuzzySet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts       !! reference to a FuzzyPartition.
    real(dp),              intent(inout) :: buffer(:) !! buffer for the parameters.
    integer, optional,     intent(in)    :: INDEX     !! index of starting parameter.
    integer                              :: post_idx  !! next available index.
    !/ -----------------------------------------------------------------------------------
    integer :: i, s
    !/ -----------------------------------------------------------------------------------
    s = 0
    if ( present( INDEX ) ) s = INDEX - 1
    do i=1,dts%num_set
       buffer(i+s) = dts%getCenter(i)
    end do
    post_idx = dts%num_set + s + 1
  end function fp_store


  !/ =====================================================================================
  subroutine fp_read( dts, un, IOSTAT, MAXINDEX )
    !/ -----------------------------------------------------------------------------------
    !! Read the parameters for this FuzzyPartition from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts      !! reference to a FuzzyPartition.
    integer,               intent(in)    :: un       !! file unit.
    integer, optional,     intent(out)   :: IOSTAT   !! error return status.
    integer, optional,     intent(in)    :: MAXINDEX !! maximum parameters that can be read.
    !/ -----------------------------------------------------------------------------------
    integer               :: i, n, mxn
    real(dp), allocatable :: buffer(:)
    !/ -----------------------------------------------------------------------------------
    mxn = 128
    if ( present( MAXINDEX ) ) then
       mxn = MAXINDEX
    end if

    allocate( buffer(mxn) )

    read( un, * ) n, (buffer(i), i=1,n )

    call dts%init( buffer(1:n) )

    deallocate( buffer )

  end subroutine fp_read


  !/ =====================================================================================
  subroutine fp_write( dts, un, FMT, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !! Write the parameters for this FuzzyParrtition from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition),  intent(inout) :: dts    !! reference to a FuzzyPartition.
    integer,                intent(in)    :: un     !! file unit.
    character(*), optional, intent(in)    :: FMT    !! edit descriptor.
    integer,      optional, intent(in)    :: IOSTAT !! error return status.
    !/ -----------------------------------------------------------------------------------
    character(64) :: afmt
    integer       :: i
    !/ -----------------------------------------------------------------------------------
    if ( present(FMT) ) then
       write(afmt,100) dts%num_set, FMT
    else
       write(afmt,200) dts%num_set
    end if

    write(un,trim(adjustl(afmt))) dts%num_set, (dts%getCenter(i),i=1,dts%num_set)

100 format('(I0,',I0,'(1X,',A,'))')
200 format('(I0,',I0,'(1X,ES13.6))')

  end subroutine fp_write


end module fuzzy_partition_mod


!/ =======================================================================================
!/ **                       F U Z Z Y _ P A R T I T I O N _ M O D                       **
!/ =========================================================================== END FILE ==
