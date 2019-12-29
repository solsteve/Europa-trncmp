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
  type :: FuzzySet_ptr
     !/ ----------------------------------------------------------------------------------
     class(FuzzySet), pointer :: ptr => null()
  end type FuzzySet_ptr


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

     type(FuzzySet_ptr), allocatable :: fset(:)  !!  array of fuzzy sets.
     integer  :: num_set =  0
     real(dp) :: min_ctr = -D_ONE
     real(dp) :: max_ctr =  D_ONE

   contains

     procedure, private :: fp_init_default
     procedure, private :: fp_init_left_right
     procedure, private :: fp_init_params
     procedure, private :: fp_resize
     procedure, private :: fp_balance

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

  end type FuzzyPartition



  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine fp_init_default( dts, n )
    !/ -----------------------------------------------------------------------------------
    !! Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
    !! The internal sets are triangular. The left is -1 and the right is +1
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer,               intent(in)    :: n   !! number of fuzzy sets,
    !/ -----------------------------------------------------------------------------------

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
    integer,  intent(in) :: n   !! number of fuzzy sets,
    real(dp), intent(in) :: mn  !! left  most center.
    real(dp), intent(in) :: mx  !! right most center.
    !/ -----------------------------------------------------------------------------------

  end subroutine fp_init_left_right


  !/ =====================================================================================
  subroutine fp_init_params( dts, ctrs )
    !/ -----------------------------------------------------------------------------------
    !!  Construct a domain of fuzzy sets. The left and right sets are trapezoidal.
  !! The internal sets are triangular. The centers are defined by (ctrs) the left
  !! and right extents are defined by the centers of the sets to the left and right.

    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    real(dp), intent(in) :: ctrs(:)  !! real array of center values.
    !/ -----------------------------------------------------------------------------------

  end subroutine fp_init_params


  !/ =====================================================================================
  subroutine fp_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.

  end subroutine fp_destroy


!/ =====================================================================================
  subroutine fp_resize( dts, n, CHANGED )
    !/ -----------------------------------------------------------------------------------
    !!
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

    if ( 1.gt.dts%num_set ) then
       call log_error( 'There needs to be at least one function only got' )
       goto 999
    end if

    if ( n.eq.dts%num_set ) then
       call log_debug( 'Resize called with no change' )
       goto 999
    end if

    call dts%destroy

    allocate( dts%fset(n) )

    chg = .true.

    dts%num_set = n

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
       call log_error( 'FuzzyPartition%resize should have rejected this' )

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
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: idx
    class(FuzzySet),       pointer       :: ptr 
    !/ -----------------------------------------------------------------------------------
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
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer :: n
    !/ -----------------------------------------------------------------------------------
    n = 1
  end function fp_get_number_inputs


  !/ =====================================================================================
  function fp_get_number_outputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer :: n
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
    integer :: n
    !/ -----------------------------------------------------------------------------------
    n = dts%num_set
  end function fp_get_storage_size


  !/ =====================================================================================
  function fp_get_center( dts, idx ) result( c )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: idx
    real(dp) :: c
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
  function fp_mu( dts, idx, x ) result( m )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: idx
    real(dp), intent(in) :: x
    real(dp) :: m
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(idx)%ptr ) ) then
          m = dts%fset(idx)%ptr%mu(x)
       else
          call log_error( 'FuzzyPartition: fset pointer not assigned', I4=idx )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_mu


  !/ =====================================================================================
  function fp_area( dts, idx, deg ) result( a )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: idx
    real(dp), intent(in) :: deg
    real(dp) :: a
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(idx)%ptr ) ) then
          a = dts%fset(idx)%ptr%area(deg)
       else
          call log_error( 'FuzzyPartition: fset pointer not assigned', I4=idx )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_area


  !/ =====================================================================================
  function fp_coa( dts, idx, deg ) result( c )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: idx
    real(dp), intent(in) :: deg
    real(dp) :: c
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%fset ) ) then
       if ( associated( dts%fset(idx)%ptr ) ) then
          c = dts%fset(idx)%ptr%coa(deg)
       else
          call log_error( 'FuzzyPartition: fset pointer not assigned', I4=idx )
       end if
    else
       call log_error( 'FuzzyPartition: fset array not allocated' )
    end if
  end function fp_coa


  !/ =====================================================================================
  function fp_load( dts, buffer, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts       !! reference to a FuzzyPartition.
    real(dp),              intent(in)    :: buffer(:) !! 
    integer,               intent(in)    :: pre_idx   !! 
    integer                              :: post_idx  !! 
    !/ -----------------------------------------------------------------------------------

  end function fp_load


  !/ =====================================================================================
  function fp_store( dts, buffer, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts       !! reference to a FuzzyPartition.
    real(dp),              intent(in)    :: buffer(:) !! 
    integer,               intent(in)    :: pre_idx   !! 
    integer                              :: post_idx  !! 
    !/ -----------------------------------------------------------------------------------

  end function fp_store


  !/ =====================================================================================
  subroutine fp_read( dts, un, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: un
    integer, optional, intent(in) :: IOSTAT
    !/ -----------------------------------------------------------------------------------

  end subroutine fp_read


  !/ =====================================================================================
  subroutine fp_write( dts, un, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyPartition), intent(inout) :: dts !! reference to a FuzzyPartition.
    integer, intent(in) :: un
    integer, optional, intent(in) :: IOSTAT
    !/ -----------------------------------------------------------------------------------

  end subroutine fp_write


end module fuzzy_partition_mod


!/ =======================================================================================
!/ **                       F U Z Z Y _ P A R T I T I O N _ M O D                       **
!/ =========================================================================== END FILE ==
