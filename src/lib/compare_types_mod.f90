!/ ====================================================================== BEGIN FILE =====
!/ **                         C O M P A R E _ T Y P E S _ M O D                         **
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
module compare_types_mod
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2018-06-03
  !! license: GPL
  !! 
  !!##Tran-Comp Environment
  !! 
  !! Compare unlimited polymorphic
  ! 
  !/ -------------------------------------------------------------------------------------
  use iso_fortran_env
  implicit none
  private

  integer, private, parameter :: dp=kind(1.0d0)
  integer, private, parameter :: sp=kind(1.0e0)

  interface compare
     module procedure :: compare_objects
  end interface compare

  public :: compare

  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  function compare_string_to_object( cval, obj, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Compare a character string with an unlimited polymorphic object.
    !! Return  -1   cval .lt. obj
    !!          0   cval .eq. obj
    !!         +1   cval .rt. obj
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    2   | object is NULL                   |
    !! |    3   | object is not a character string |
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),      intent(in)  :: cval !! left hand side character string.
    class(*), pointer, intent(in)  :: obj  !! right hand side unlimited polymorphic object.
    integer, optional, intent(out) :: stat !! optional return status.
    integer                        :: res  !! -1: l<r,  0: l==r,  +1: l>r
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    res   = 0
    istat = 0
    
    if ( associated( obj ) ) then
       select type ( obj )
       type is ( character(*) )
          if ( LLT(cval,obj) ) then
             res = -1
          else
             if ( LGT(cval,obj) ) res = 1
          end if
       class default
          istat = 3
       end select
    else
       istat = 2
    end if

    if ( present( stat ) ) stat = istat

  end function compare_string_to_object


  !/ =====================================================================================
  function compare_integer_to_object( ival, obj, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Compare an integer with an unlimited polymorphic object.
    !! Return  -1   ival .lt. obj
    !!          0   ival .eq. obj
    !!         +1   ival .rt. obj
    !!
    !! |  stat  | errmsg                   |
    !! | :----: | ------------------------ |
    !! |    0   | n/a                      |
    !! |    2   | object is NULL           |
    !! |    3   | object is not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,           intent(in)  :: ival !! left hand side integer.
    class(*), pointer, intent(in)  :: obj  !! right hand side unlimited polymorphic object.
    integer, optional, intent(out) :: stat !! optional return status.
    integer                        :: res  !! -1: l<r,  0: l==r,  +1: l>r
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    res   = 0
    istat = 0
    
    if ( associated( obj ) ) then
       select type ( obj )
       type is ( integer )
          if ( ival .lt. obj ) then
             res = -1
          else
             if ( ival .gt. obj ) res = 1
          end if
       class default
          istat = 3
       end select
    else
       istat = 2
    end if

    if ( present( stat ) ) stat = istat

  end function compare_integer_to_object

  
  !/ =====================================================================================
  function compare_single_to_object( fval, obj, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Compare an single precision with an unlimited polymorphic object.
    !! Return  -1   fval .lt. obj
    !!          0   fval .eq. obj
    !!         +1   fval .rt. obj
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    2   | object is NULL                   |
    !! |    3   | object is not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp),          intent(in)  :: fval !! left hand side single precision.
    class(*), pointer, intent(in)  :: obj  !! right hand side unlimited polymorphic object.
    integer, optional, intent(out) :: stat !! optional return status.
    integer                        :: res  !! -1: l<r,  0: l==r,  +1: l>r
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    res   = 0
    istat = 0
    
    if ( associated( obj ) ) then
       select type ( obj )
       type is ( real(sp) )
          if ( fval .lt. obj ) then
             res = -1
          else
             if ( fval .gt. obj ) res = 1
          end if
       class default
          istat = 3
       end select
    else
       istat = 2
    end if

    if ( present( stat ) ) stat = istat

  end function compare_single_to_object


  !/ =====================================================================================
  function compare_double_to_object( dval, obj, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Compare an double precision with an unlimited polymorphic object.
    !! Return  -1   dval .lt. obj
    !!          0   dval .eq. obj
    !!         +1   dval .rt. obj
    !!
    !! |  stat  | errmsg                           |
    !! | :----: | -------------------------------- |
    !! |    0   | n/a                              |
    !! |    2   | object is NULL                   |
    !! |    3   | object is not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),          intent(in)  :: dval !! left hand side double precision.
    class(*), pointer, intent(in)  :: obj  !! right hand side unlimited polymorphic object.
    integer, optional, intent(out) :: stat !! optional return status.
    integer                        :: res  !! -1: l<r,  0: l==r,  +1: l>r
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    res   = 0
    istat = 0
    
    if ( associated( obj ) ) then
       select type ( obj )
       type is ( real(dp) )
          if ( dval .lt. obj ) then
             res = -1
          else
             if ( dval .gt. obj ) res = 1
          end if
       class default
          istat = 3
       end select
    else
       istat = 2
    end if

    if ( present( stat ) ) stat = istat

  end function compare_double_to_object


  !/ =====================================================================================
  function compare_objects( lhs, rhs, stat ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Compare two unlimited polymorphic object.
    !! Return  -1   lhs .lt. rhs
    !!          0   lhs .eq. rhs
    !!         +1   lhs .rt. rhs
    !!
    !! |  stat  | errmsg                        |
    !! | :----: | ----------------------------- |
    !! |    0   | n/a                           |
    !! |    1   | left  object is NULL          |
    !! |    2   | right object is NULL          |
    !! |    3   | objects are not the same type |
    !! |    4   | objects are not vaild types   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), pointer, intent(in)  :: lhs !! left  hand side unlimited polymorphic object.
    class(*), pointer, intent(in)  :: rhs !! right hand side unlimited polymorphic object.
    integer, optional, intent(out) :: stat !! optional return status.
    integer                        :: res  !! -1: l<r,  0: l==r,  +1: l>r
    !/ -----------------------------------------------------------------------------------

    res   = 0

    if ( associated( lhs ) ) then
       select type( lhs )
       type is( character(*) )
          res = compare_string_to_object( lhs, rhs, stat )
       type is( integer )
          res = compare_integer_to_object( lhs, rhs, stat )
       type is( real(sp) )
          res = compare_single_to_object( lhs, rhs, stat )
       type is( real(dp) )
          res = compare_double_to_object( lhs, rhs, stat )
       class default
          if ( present( stat ) ) stat = 4
       end select
    else
       if ( present( stat ) ) stat = 1
    end if

  end function compare_objects

  
end module compare_types_mod

!/ =======================================================================================
!/ **                         C O M P A R E _ T Y P E S _ M O D                         **
!/ =========================================================================== END FILE ==
