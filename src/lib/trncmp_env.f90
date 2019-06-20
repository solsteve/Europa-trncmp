!/ ====================================================================== BEGIN FILE =====
!/ **                                T R N C M P _ E N V                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2017, Stephen W. Soliday                                           **
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
module trncmp_env
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2017-03-30
  !! license: GPL
  !!
  !!## Tran-Comp Environment.
  !!
  !! Collection of definitions for common developmental environment
  !
  !/ -------------------------------------------------------------------------------------
  use constants_env
  use math_aux
  use copy_mod
  use zero_mod
  use summation_mod
  use compare_types_mod
  use poly_cast_mod
  implicit none


  !/ =====================================================================================
  type :: string_array_t
     !/ ----------------------------------------------------------------------------------
     character(:), allocatable :: str
  end type string_array_t


  !/ =====================================================================================
  type, public :: object_pointer
     !/ ----------------------------------------------------------------------------------
     class(*), pointer :: ptr => null()
  end type object_pointer

  
  integer, public, parameter :: MAX_PATH = 128 !! Maximum charaters in file path

    !/ -------------------------------------------------------------------------------------
  interface LEQ
     !/ ----------------------------------------------------------------------------------
     module procedure :: lexical_equals
  end interface LEQ

  !/ -------------------------------------------------------------------------------------
  interface strcmp
     !/ ----------------------------------------------------------------------------------
     module procedure :: string_compare
  end interface strcmp



  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  function lexical_equals( lhs, rhs ) result( cmp )
    !/ -----------------------------------------------------------------------------------
    !! Lexical equal to.
    !!
    !! Determines whether one string is lexically equal to another string, where the two
    !! strings are interpreted as containing ASCII character codes. 
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: lhs !! left  hand side string
    character(*), intent(in) :: rhs !! right hand side string
    logical                  :: cmp

    cmp = .true.
    if ( llt( lhs, rhs ) ) then
       cmp = .false.
    else
       if ( lgt( lhs, rhs ) ) then
          cmp = .false.
       end if
    end if

  end function lexical_equals

  
  !/ =====================================================================================
  function string_compare( lhs, rhs ) result( cmp )
    !/ -----------------------------------------------------------------------------------
    !! Compare two strings
    !!
    !! | cmp  | condition  |
    !! |:----:|------------|
    !! |  -1  |lhs .lt. rhs|
    !! |   0  |lhs .eq. rhs|
    !! |  +1  |lhs .gt. rhs|
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: lhs !! left  hand side string
    character(*), intent(in) :: rhs !! right hand side string
    integer                  :: cmp !! result
    !/ -----------------------------------------------------------------------------------
    cmp = 0
    if ( llt( lhs, rhs ) ) then
       cmp = -1
    else
       if ( lgt( lhs, rhs ) ) then
          cmp = 1
       end if
    end if

  end function string_compare


end module trncmp_env


!/ =======================================================================================
!/ **                                T R N C M P _ E N V                                **
!/ =========================================================================== END FILE ==
