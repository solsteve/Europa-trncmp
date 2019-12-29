!/ ====================================================================== BEGIN FILE =====
!/ **                             F U Z Z Y _ S E T _ M O D                             **
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
module fuzzy_set_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides an interface for fuzzy sets.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-12-25
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use string_tools
  implicit none


  !/ =====================================================================================
  type, abstract :: FuzzySet
     !/ ----------------------------------------------------------------------------------
     !! This class defines an abstract type for a generic fuzzy set
     !/ ----------------------------------------------------------------------------------

   contains

     procedure(fset_abst_set),        pass(dts), deferred :: set
     procedure(fset_abst_update),     pass(dts), deferred :: update

     procedure(fset_abst_get_left),   pass(dts), deferred :: getLeft
     procedure(fset_abst_get_center), pass(dts), deferred :: getCenter
     procedure(fset_abst_get_right),  pass(dts), deferred :: getRight

     procedure(fset_abst_mu),         pass(dts), deferred :: mu
     procedure(fset_abst_area),       pass(dts), deferred :: area
     procedure(fset_abst_coa),        pass(dts), deferred :: coa
     procedure(fset_abst_to_string),  pass(dts), deferred :: toString
     procedure(fset_abst_load),       pass(dts), deferred :: load
     procedure(fset_abst_store),      pass(dts), deferred :: store
  end type FuzzySet


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     subroutine fset_abst_set( dts, p1, p2, P3 )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet),    intent(inout) :: dts !! reference to a FuzzySet
       real(dp),           intent(in)    :: p1  !! parameter one
       real(dp),           intent(in)    :: p2  !! parameter two
       real(dp), optional, intent(in)    :: P3  !! parameter three
     end subroutine fset_abst_set
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     subroutine fset_abst_update( dts )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet),    intent(inout) :: dts !! reference to a FuzzySet
     end subroutine fset_abst_update
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_get_left( dts ) result( v )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       real(dp)                       :: v   !! left extreme.
     end function fset_abst_get_left
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     !! Get center of the set.
     !/ ----------------------------------------------------------------------------------
     function fset_abst_get_center( dts ) result( v )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       real(dp)                       :: v   !! center of FuzzySet
     end function fset_abst_get_center
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     !! Get right extreme.
     !/ ----------------------------------------------------------------------------------
     function fset_abst_get_right( dts ) result( v )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       real(dp)                       :: v   !! left extreme.
     end function fset_abst_get_right
  end interface









  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_mu( dts, x ) result( m )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       real(dp),        intent(in)    :: x   !! crisp value
       real(dp)                       :: m   !! degree of membership
     end function fset_abst_mu
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_area( dts, deg ) result( a )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       real(dp),        intent(in)    :: deg !! degree of membership
       real(dp)                       :: a   !! area
     end function fset_abst_area
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_coa( dts, deg ) result( c )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       real(dp),        intent(in)    :: deg !! degree of membership
       real(dp)                       :: c   !! center
     end function fset_abst_coa
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_to_string( dts, fmt ) result( str )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts !! reference to a FuzzySet
       character(*),    intent(in)    :: fmt !! edit descriptor
       character(:),    allocatable   :: str !! output string
     end function fset_abst_to_string
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_load( dts, array, pre_idx ) result( post_idx )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts      !! reference to a FuzzySet
       real(dp),        intent(inout) :: array(:) !! source array
       integer,         intent(in)    :: pre_idx  !! pre-index
       integer                        :: post_idx !! post-index
     end function fset_abst_load
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function fset_abst_store( dts, array, pre_idx ) result( post_idx )
       use trncmp_env, only : dp
       import :: FuzzySet
       class(FuzzySet), intent(inout) :: dts      !! reference to a FuzzySet
       real(dp),        intent(inout) :: array(:) !! destination array
       integer,         intent(in)    :: pre_idx  !! pre-index
       integer                        :: post_idx !! post-index
     end function fset_abst_store
  end interface


  !/ =====================================================================================
  type, extends( FuzzySet ) :: LeftTrapezoidSet
     !/ ----------------------------------------------------------------------------------
     !  Provides the methods for a derived left trapezoid fuzzy set.
     !
     !  1 ___|
     !       |\
     !       | \
     !       |  \    Left Trapezoid Set
     !       |   \
     !       |    \____
     !  0 ===|===========
     !       C     R
     !/ ----------------------------------------------------------------------------------

     real(dp) :: C = D_ZERO !! Point of maximum membership extreme of this fuzzy set.
     real(dp) :: R = D_ONE  !! Right extreme of this fuzzy set.
     real(dp) :: W = D_ONE  !! Work variable.

   contains

     procedure, pass(dts) :: set       => ltrap_set
     procedure, pass(dts) :: update    => ltrap_update

     procedure, pass(dts) :: getLeft   => ltrap_get_left
     procedure, pass(dts) :: getCenter => ltrap_get_center
     procedure, pass(dts) :: getRight  => ltrap_get_right

     procedure, pass(dts) :: mu        => ltrap_mu
     procedure, pass(dts) :: area      => ltrap_area
     procedure, pass(dts) :: coa       => ltrap_coa
     procedure, pass(dts) :: toString  => ltrap_toString
     procedure, pass(dts) :: load      => ltrap_load
     procedure, pass(dts) :: store     => ltrap_store

  end type LeftTrapezoidSet



  !/ =====================================================================================
  type, extends( FuzzySet ) :: RightTrapezoidSet
     !/ ----------------------------------------------------------------------------------
     !  Provides the methods for a derived right trapezoid fuzzy set.
     !
     !  1            |___
     !              /|
     !             / |
     !            /  |   Right Trapezoid Set
     !           /   |
     !      ____/    |
     !  0 ===========|====
     !          L    C
     !/ ----------------------------------------------------------------------------------

     real(dp) :: L = -D_ONE  !! Left extreme of this fuzzy set.
     real(dp) :: C =  D_ZERO !! Point of maximum membership extreme of this fuzzy set.
     real(dp) :: W =  D_ONE  !! Work variable.

   contains

     procedure, pass(dts) :: set       => rtrap_set
     procedure, pass(dts) :: update    => rtrap_update

     procedure, pass(dts) :: getLeft   => rtrap_get_left
     procedure, pass(dts) :: getCenter => rtrap_get_center
     procedure, pass(dts) :: getRight  => rtrap_get_right

     procedure, pass(dts) :: mu        => rtrap_mu
     procedure, pass(dts) :: area      => rtrap_area
     procedure, pass(dts) :: coa       => rtrap_coa
     procedure, pass(dts) :: toString  => rtrap_toString
     procedure, pass(dts) :: load      => rtrap_load
     procedure, pass(dts) :: store     => rtrap_store

  end type RightTrapezoidSet



  !/ =====================================================================================
  type, extends( FuzzySet ) :: TriangleSet
     !/ ----------------------------------------------------------------------------------
     !  Provides the methods for a derived triangle fuzzy set.
     !  
     !  1            |
     !              /|\
     !             / | \
     !            /  |  \   Triangle Set
     !           /   |   \
     !      ____/    |    \____
     !  0 ===========|===========
     !         L     C     R
     !/ ----------------------------------------------------------------------------------

     real(dp) :: L  = -D_ONE  !! Left extreme of this fuzzy set.
     real(dp) :: C  =  D_ZERO !! Point of maximum membership extreme of this fuzzy set.
     real(dp) :: R  =  D_ONE  !! Right extreme of this fuzzy set.
     real(dp) :: W  =  D_TWO  !! Work variable.
     real(dp) :: LD =  D_ONE  !! Left  difference.
     real(dp) :: RD =  D_ONE  !! Right difference.

   contains

     procedure, pass(dts) :: set       => triangle_set
     procedure, pass(dts) :: update    => triangle_update

     procedure, pass(dts) :: getLeft   => triangle_get_left
     procedure, pass(dts) :: getCenter => triangle_get_center
     procedure, pass(dts) :: getRight  => triangle_get_right

     procedure, pass(dts) :: mu        => triangle_mu
     procedure, pass(dts) :: area      => triangle_area
     procedure, pass(dts) :: coa       => triangle_coa
     procedure, pass(dts) :: toString  => triangle_toString
     procedure, pass(dts) :: load      => triangle_load
     procedure, pass(dts) :: store     => triangle_store

  end type TriangleSet



  !/ =====================================================================================
  interface LeftTrapezoidSet
    !/ -----------------------------------------------------------------------------------
     module procedure :: ltrap_create
  end interface LeftTrapezoidSet


  !/ =====================================================================================
  interface TriangleSet
    !/ -----------------------------------------------------------------------------------
     module procedure :: triangle_create
  end interface TriangleSet


  !/ =====================================================================================
  interface RightTrapezoidSet
    !/ -----------------------------------------------------------------------------------
     module procedure :: rtrap_create
  end interface RightTrapezoidSet


  !/ =====================================================================================
  interface FuzzySet
    !/ -----------------------------------------------------------------------------------
     module procedure :: fuzzy_set_create
  end interface FuzzySet




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function ltrap_create( CENTER, RIGHT ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Allocate and set a LeftTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    real(dp), optional,   intent(in) :: CENTER
    real(dp), optional,   intent(in) :: RIGHT
    class(LeftTrapezoidSet), pointer :: ptr
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    if ( present(CENTER).and.present(RIGHT) ) then
       call ptr%set( P1=CENTER, P2=RIGHT )
    end if
  end function ltrap_create

  
  !/ =====================================================================================
  function triangle_create( LEFT, CENTER, RIGHT ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Allocate and set a LeftTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    real(dp), optional, intent(in) :: LEFT
    real(dp), optional, intent(in) :: CENTER
    real(dp), optional, intent(in) :: RIGHT
    class(TriangleSet), pointer    :: ptr
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    if ( present(LEFT).and.present(CENTER).and.present(RIGHT) ) then
       call ptr%set( P1=LEFT, P2=CENTER, P3=RIGHT )
    end if
  end function triangle_create

  
  !/ =====================================================================================
  function rtrap_create( LEFT, CENTER ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Allocate and set a LeftTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    real(dp), optional,   intent(in) :: LEFT
    real(dp), optional,   intent(in) :: CENTER
    class(LeftTrapezoidSet), pointer :: ptr
    !/ -----------------------------------------------------------------------------------
    allocate( ptr )
    if ( present(LEFT).and.present(CENTER) ) then
       call ptr%set( P1=LEFT, P2=CENTER )
    end if
  end function rtrap_create


  !/ =====================================================================================
  function fuzzy_set_create( sub_type, P1, P2, P3 ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Allocate and set a RightTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    character(1),       intent(in) :: sub_type
    real(dp), optional, intent(in) :: P1
    real(dp), optional, intent(in) :: P2
    real(dp), optional, intent(in) :: P3
    class(FuzzySet),    pointer    :: ptr
    !/ -----------------------------------------------------------------------------------
    !class(LeftTrapezoidSet),  pointer :: LT
    !class(TriangleSet),       pointer :: TR
    !class(RightTrapezoidSet), pointer :: RT
    !/ -----------------------------------------------------------------------------------

    if      ( 'L'.eq.sub_type ) then
       ptr => LeftTrapezoidSet( CENTER=P1, RIGHT=P2 )
    else if ( 'T'.eq.sub_type ) then
       ptr => TriangleSet( LEFT=P1, CENTER=P2, RIGHT=P3 )
    else if ( 'R'.eq.sub_type ) then
       ptr => RightTrapezoidSet( LEFT=P1, CENTER=P2 )
    else
       write(ERROR_UNIT,'(A)') 'must be L, T, or R'
    end if

  end function fuzzy_set_create








  !/ =====================================================================================
  function ltrap_get_left( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get left extreme.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet.
    real(dp)                               :: v   !! left extreme.
    !/ -----------------------------------------------------------------------------------
    v = D_HALF*(D_THREE*dts%C - dts%R)
  end function ltrap_get_left


  !/ =====================================================================================
  function ltrap_get_center( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get Center.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet.
    real(dp)                               :: v   !! center of the LeftTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    v = dts%C
  end function ltrap_get_center


  !/ =====================================================================================
  function ltrap_get_right( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get right extreme.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet.
    real(dp)                               :: v   !! right extreme.
    !/ -----------------------------------------------------------------------------------
    v = dts%R
  end function ltrap_get_right


  !/ =====================================================================================
  subroutine ltrap_set( dts, p1, p2, P3 )
    !/ -----------------------------------------------------------------------------------
    !! Set main parameters.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet
    real(dp),                intent(in)    :: p1  !! parameter one
    real(dp),                intent(in)    :: p2  !! parameter two
    real(dp), optional,      intent(in)    :: P3  !! unused
    !/ -----------------------------------------------------------------------------------
    dts%C = p1
    dts%R = p2
    call dts%update
  end subroutine ltrap_set


  !/ =====================================================================================
  subroutine ltrap_update( dts )
    !/ -----------------------------------------------------------------------------------
    !! Update pre-computed constants.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet
    !/ -----------------------------------------------------------------------------------
    dts%W = dts%R - dts%C
  end subroutine ltrap_update


  !/ =====================================================================================
  function ltrap_mu( dts, x ) result( m )
    !/ -----------------------------------------------------------------------------------
    !! Compute the degree of membership in this LeftTrapezoidSet based on a crisp value x.
    !! The domain is all real numbers. The range is 0 to 1 inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet
    real(dp),                intent(in)    :: x   !! crisp value
    real(dp)                               :: m   !! degree of membership
    !/ -----------------------------------------------------------------------------------
    m = D_ONE
    if ( x.gt.dts%C ) then
       if ( x.lt.dts%R ) then
          m = (dts%R - x)/dts%W
       else
          m = D_ZERO
       end if
    end if
  end function ltrap_mu


  !/ =====================================================================================
  function ltrap_area( dts, deg ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Compute the area under the degree of membership for this LeftTrapezoidSet.
    !! The domain is 0 to 1 inclusive. The range is 0 to max area for this LeftTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet
    real(dp),                intent(in)    :: deg !! degree of membership
    real(dp)                               :: a   !! area
    !/ -----------------------------------------------------------------------------------
    a = D_HALF*dts%W*deg*(D_THREE-deg)
  end function ltrap_area


  !/ =====================================================================================
  function ltrap_coa( dts, deg ) result( c )
    !/ -----------------------------------------------------------------------------------
    !! Compute the center of area based on the degree of membership in this LeftTrapezoidSet. 
    !! The domain is 0 to 1 inclusive. The range is (left) to (right) inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet
    real(dp),                intent(in)    :: deg !! degree of membership
    real(dp)                               :: c   !! center
    !/ -----------------------------------------------------------------------------------
    c = ( D_NINE*(D_THREE*dts%C + dts%R) - (1.2d1*dts%R - D_FOUR*dts%W*deg)*deg ) / (1.2d1*(D_THREE - deg))
  end function ltrap_coa


  !/ =====================================================================================
  function ltrap_toString( dts, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert parameters to a string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts !! reference to a LeftTrapezoidSet
    character(*),            intent(in)    :: fmt !! edit descriptor
    character(:),            allocatable   :: str !! output string
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: buffer
    !/ -----------------------------------------------------------------------------------

    str = 'L('
    buffer = toString( dts%C, fmt )
    str = str // buffer // ','
    buffer = toString( dts%R, fmt )
    str = str // buffer // ')'

  end function ltrap_toString


  !/ =====================================================================================
  function ltrap_load( dts, array, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Load the parameters for this LeftTrapezoidSet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts      !! reference to a LeftTrapezoidSet
    real(dp),                intent(inout) :: array(:) !! source array
    integer,                 intent(in)    :: pre_idx  !! pre-index
    integer                                :: post_idx !! post-index
    !/ -----------------------------------------------------------------------------------
    dts%C    = array(pre_idx)
    dts%R    = array(pre_idx+1)
    post_idx = pre_idx + 2
    call dts%update
  end function ltrap_load


  !/ =====================================================================================
  function ltrap_store( dts, array, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Store the parameters for this LeftTrapezoidSet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(LeftTrapezoidSet), intent(inout) :: dts      !! reference to a LeftTrapezoidSet
    real(dp),                intent(inout) :: array(:) !! destination array
    integer,                 intent(in)    :: pre_idx  !! pre-index
    integer                                :: post_idx !! post-index
    !/ -----------------------------------------------------------------------------------
    array(pre_idx)   = dts%C
    array(pre_idx+1) = dts%R
    post_idx = pre_idx + 2
  end function ltrap_store








  !/ =====================================================================================
  function rtrap_get_left( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get left extreme.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet.
    real(dp)                                :: v   !! left extreme.
    !/ -----------------------------------------------------------------------------------
    v = dts%L
  end function rtrap_get_left


  !/ =====================================================================================
  function rtrap_get_center( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get Center.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet.
    real(dp)                                :: v   !! center of the RightTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    v = dts%C
  end function rtrap_get_center


  !/ =====================================================================================
  function rtrap_get_right( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get right extreme.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet.
    real(dp)                                :: v   !! right extreme.
    !/ -----------------------------------------------------------------------------------
    v = D_HALF*(D_THREE*dts%C-dts%L)
  end function rtrap_get_right


  !/ =====================================================================================
  subroutine rtrap_set( dts, p1, p2, P3 )
    !/ -----------------------------------------------------------------------------------
    !! Set main parameters.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet
    real(dp),                 intent(in)    :: p1  !! parameter one
    real(dp),                 intent(in)    :: p2  !! parameter two
    real(dp), optional,       intent(in)    :: P3  !! unused
    !/ -----------------------------------------------------------------------------------
    dts%L = p1
    dts%C = p2
    call dts%update
  end subroutine rtrap_set


  !/ =====================================================================================
  subroutine rtrap_update( dts )
    !/ -----------------------------------------------------------------------------------
    !! Update pre-computed constants.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet
    !/ -----------------------------------------------------------------------------------
    dts%W = dts%C - dts%L
  end subroutine rtrap_update


  !/ =====================================================================================
  function rtrap_mu( dts, x ) result( m )
    !/ -----------------------------------------------------------------------------------
    !! Compute the degree of membership in this RightTrapezoidSet based on a crisp value x.
    !! The domain is all real numbers. The range is 0 to 1 inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet
    real(dp),                 intent(in)    :: x   !! crisp value
    real(dp)                                :: m   !! degree of membership
    !/ -----------------------------------------------------------------------------------
    m = D_ONE
    if ( x.lt.dts%C ) then
       if ( x.gt.dts%L ) then
          m = (x - dts%L)/dts%W
       else
          m = D_ZERO
       end if
    end if
  end function rtrap_mu


  !/ =====================================================================================
  function rtrap_area( dts, deg ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Compute the area under the degree of membership for this RightTrapezoidSet.
    !! The domain is 0 to 1 inclusive. The range is 0 to max area for this RightTrapezoidSet.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet
    real(dp),                 intent(in)    :: deg !! degree of membership
    real(dp)                                :: a   !! area
    !/ -----------------------------------------------------------------------------------
    a = D_HALF*(D_THREE - deg)*dts%W*deg
  end function rtrap_area


  !/ =====================================================================================
  function rtrap_coa( dts, deg ) result( c )
    !/ -----------------------------------------------------------------------------------
    !! Compute the center of area based on the degree of membership in this RightTrapezoidSet. 
    !! The domain is 0 to 1 inclusive. The range is (left) to (right) inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet
    real(dp),                 intent(in)    :: deg !! degree of membership
    real(dp)                                :: c   !! center
    !/ -----------------------------------------------------------------------------------
    c = ( D_NINE*(D_THREE*dts%C + dts%L) - (D_FOUR*dts%W*deg + 1.2d1*dts%L)*deg ) / &
         &                                  (1.2d1*(D_THREE - deg))
  end function rtrap_coa


  !/ =====================================================================================
  function rtrap_toString( dts, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert parameters to a string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts !! reference to a RightTrapezoidSet
    character(*),             intent(in)    :: fmt !! edit descriptor
    character(:),             allocatable   :: str !! output string
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: buffer
    !/ -----------------------------------------------------------------------------------

    str = 'R('
    buffer = toString( dts%L, fmt )
    str = str // buffer // ','
    buffer = toString( dts%C, fmt )
    str = str // buffer // ')'

  end function rtrap_toString


  !/ =====================================================================================
  function rtrap_load( dts, array, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Load the parameters for this RightTrapezoidSet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts      !! reference to a RightTrapezoidSet
    real(dp),                 intent(inout) :: array(:) !! source array
    integer,                  intent(in)    :: pre_idx  !! pre-index
    integer                                 :: post_idx !! post-index
    !/ -----------------------------------------------------------------------------------
    dts%L    = array(pre_idx)
    dts%C    = array(pre_idx+1)
    post_idx = pre_idx + 2
    call dts%update
  end function rtrap_load


  !/ =====================================================================================
  function rtrap_store( dts, array, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Store the parameters for this RightTrapezoidSet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RightTrapezoidSet), intent(inout) :: dts      !! reference to a RightTrapezoidSet
    real(dp),                 intent(inout) :: array(:) !! destination array
    integer,                  intent(in)    :: pre_idx  !! pre-index
    integer                                 :: post_idx !! post-index
    !/ -----------------------------------------------------------------------------------
    array(pre_idx)   = dts%L
    array(pre_idx+1) = dts%C
    post_idx = pre_idx + 2
  end function rtrap_store










  !/ =====================================================================================
  function triangle_get_left( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get left extreme.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet.
    real(dp)                          :: v   !! left extreme.
    !/ -----------------------------------------------------------------------------------
    v = dts%L
  end function triangle_get_left


  !/ =====================================================================================
  function triangle_get_center( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get Center.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet.
    real(dp)                          :: v   !! center of the TriangleSet.
    !/ -----------------------------------------------------------------------------------
    v = dts%C
  end function triangle_get_center


  !/ =====================================================================================
  function triangle_get_right( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get right extreme.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet.
    real(dp)                          :: v   !! right extreme.
    !/ -----------------------------------------------------------------------------------
    v = dts%R
  end function triangle_get_right


  !/ =====================================================================================
  subroutine triangle_set( dts, p1, p2, P3 )
    !/ -----------------------------------------------------------------------------------
    !! Set main parameters.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet
    real(dp),           intent(in)    :: p1  !! parameter one
    real(dp),           intent(in)    :: p2  !! parameter two
    real(dp), optional, intent(in)    :: P3  !! parameter three
    !/ -----------------------------------------------------------------------------------
    if ( present( P3 ) ) then
       dts%L  = p1
       dts%C  = p2
       dts%R  = P3
       call dts%update
    else
       write(ERROR_UNIT,'(A)') 'TriangleSet%set: requires three parameters'
    end if
  end subroutine triangle_set


  !/ =====================================================================================
  subroutine triangle_update( dts )
    !/ -----------------------------------------------------------------------------------
    !! Update pre-computed constants.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet
    !/ -----------------------------------------------------------------------------------
    dts%LD = dts%C - dts%L
    dts%RD = dts%R - dts%C
    dts%W  = dts%R - dts%L
  end subroutine triangle_update


  !/ =====================================================================================
  function triangle_mu( dts, x ) result( m )
    !/ -----------------------------------------------------------------------------------
    !! Compute the degree of membership in this TriangleSet based on a crisp value x.
    !! The domain is all real numbers. The range is 0 to 1 inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet
    real(dp),           intent(in)    :: x   !! crisp value
    real(dp)                          :: m   !! degree of membership
    !/ -----------------------------------------------------------------------------------
    m = D_ONE
    if ( x.lt.dts%C ) then
       if ( x.gt.dts%L ) then
          m = (x - dts%L)/dts%LD
       else
          m = D_ZERO
       end if
    else
       if ( x.gt.dts%C ) then
          if ( x.lt.dts%R ) then
             m = (dts%R - x)/dts%RD
          else
             m = D_ZERO
          end if
       end if
    end if
  end function triangle_mu


  !/ =====================================================================================
  function triangle_area( dts, deg ) result( a )
    !/ -----------------------------------------------------------------------------------
    !! Compute the area under the degree of membership for this TriangleSet.
    !! The domain is 0 to 1 inclusive. The range is 0 to max area for this TriangleSet.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet
    real(dp),           intent(in)    :: deg !! degree of membership
    real(dp)                          :: a   !! area
    !/ -----------------------------------------------------------------------------------
    a = D_HALF*dts%W*deg*(D_TWO-deg)
  end function triangle_area


  !/ =====================================================================================
  function triangle_coa( dts, deg ) result( c )
    !/ -----------------------------------------------------------------------------------
    !! Compute the center of area based on the degree of membership in this TriangleSet. 
    !! The domain is 0 to 1 inclusive. The range is (left) to (right) inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet
    real(dp),           intent(in)    :: deg !! degree of membership
    real(dp)                          :: c   !! center
    !/ -----------------------------------------------------------------------------------
    c = (D_THREE*(dts%L+dts%R) - (D_THREE*(dts%R-dts%C+dts%L) - &
         &          (dts%R-D_TWO*dts%C+dts%L)*deg)*deg ) / &
         &                  (D_THREE*(D_TWO-deg))
  end function triangle_coa


  !/ =====================================================================================
  function triangle_toString( dts, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert parameters to a string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts !! reference to a TriangleSet
    character(*),       intent(in)    :: fmt !! edit descriptor
    character(:),       allocatable   :: str !! output string
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: buffer
    !/ -----------------------------------------------------------------------------------

    str = 'T('
    buffer = toString( dts%L, fmt )
    str = str // buffer // ','
    buffer = toString( dts%C, fmt )
    str = str // buffer // ','
    buffer = toString( dts%R, fmt )
    str = str // buffer // ')'

  end function triangle_toString


  !/ =====================================================================================
  function triangle_load( dts, array, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Load the parameters for this TriangleSet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts      !! reference to a TriangleSet
    real(dp),           intent(inout) :: array(:) !! source array
    integer,            intent(in)    :: pre_idx  !! pre-index
    integer                           :: post_idx !! post-index
    !/ -----------------------------------------------------------------------------------
    dts%L    = array(pre_idx)
    dts%C    = array(pre_idx+1)
    dts%R    = array(pre_idx+2)
    post_idx = pre_idx + 3
    call dts%update
  end function triangle_load


  !/ =====================================================================================
  function triangle_store( dts, array, pre_idx ) result( post_idx )
    !/ -----------------------------------------------------------------------------------
    !! Store the parameters for this TriangleSet from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TriangleSet), intent(inout) :: dts      !! reference to a TriangleSet
    real(dp),           intent(inout) :: array(:) !! destination array
    integer,            intent(in)    :: pre_idx  !! pre-index
    integer                           :: post_idx !! post-index
    !/ -----------------------------------------------------------------------------------
    array(pre_idx)   = dts%L
    array(pre_idx+1) = dts%C
    array(pre_idx+2) = dts%R
    post_idx = pre_idx + 3
  end function triangle_store


end module fuzzy_set_mod


!/ =======================================================================================
!/ **                             F U Z Z Y _ S E T _ M O D                             **
!/ =========================================================================== END FILE ==
