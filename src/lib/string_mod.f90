!/ ====================================================================== BEGIN FILE =====
!/ **                                   G I S _ M O D                                   **
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
module string_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a basic string class loosley modeled after the STL std::string for C++
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-12-14
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none


  !/ =====================================================================================
  type :: tstring
     !/ ----------------------------------------------------------------------------------

     character(len=:), allocatable :: cstr

   contains

     procedure :: clear => ts_clear
     procedure :: empty => ts_is_empty
     procedure :: at    => ts_get_char

     procedure, private :: ts_internal_copy
     procedure, private :: ts_internal_from_alloc_char

     generic :: copy => ts_internal_copy, ts_internal_from_alloc_char

  end type tstring


  !/ -------------------------------------------------------------------------------------
  interface assignment(=)
     !/ ----------------------------------------------------------------------------------
     module procedure :: ts_external_copy
     module procedure :: ts_external_to_alloc_char
     module procedure :: ts_external_from_alloc_char
  end interface assignment(=)

  
  !/ -------------------------------------------------------------------------------------
  interface operator(//)
     !/ ----------------------------------------------------------------------------------
     module procedure :: ts_concatinate_ss
     module procedure :: ts_concatinate_sc
     module procedure :: ts_concatinate_cs
  end interface operator(//)

  
  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: ts_length
  end interface size


  

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine ts_clear( ts )
    !/ -----------------------------------------------------------------------------------
    !! Clear TString.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TString), intent(inout) :: ts  !! reference to this TString.
    !/ -----------------------------------------------------------------------------------
    ts%cstr = ''
  end subroutine ts_clear


  !/ =====================================================================================
  function ts_is_empty( ts ) result(e)
    !/ -----------------------------------------------------------------------------------
    !! Test if TString is empty.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(tstring), intent(in) :: ts  !! reference to a TString.
    logical                   :: e   !! result.
    !/ -----------------------------------------------------------------------------------
    e = 0.eq.len( ts%cstr )
  end function ts_is_empty


  !/ =====================================================================================
  function ts_length( ts ) result(n)
    !/ -----------------------------------------------------------------------------------
    !! Length of TString
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(tstring), intent(in) :: ts  !! reference to a TString.
    integer                   :: n   !! length of TString.
    !/ -----------------------------------------------------------------------------------
    n = len( ts%cstr )
  end function ts_length



  !/ =====================================================================================
  function ts_get_char( ts, idx ) result(c)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(tstring), intent(in) :: ts  !! reference to a TString.
    integer,        intent(in) :: idx !! index of the character
    character(1)               :: c   !! character
    !/ -----------------------------------------------------------------------------------
    c = ts%cstr(idx:idx)
  end function ts_get_char


    


  !/ =====================================================================================
  subroutine ts_internal_copy( ts, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy from another TString.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TString), intent(inout) :: ts   !! reference to this TString.
    type(TString),  intent(in)    :: src  !! reference to a source TString.
    !/ -----------------------------------------------------------------------------------
    ts%cstr = src%cstr
  end subroutine ts_internal_copy


  !/ =====================================================================================
  subroutine ts_internal_from_alloc_char( ts, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy from an allocatable character.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TString), intent(inout) :: ts   !! reference to this TString.
    character(*),   intent(in)    :: src  !! reference to a character string.
    !/ -----------------------------------------------------------------------------------
    ts%cstr = src
  end subroutine ts_internal_from_alloc_char


  !/ =====================================================================================
  subroutine ts_external_copy( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy from one TString to another.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(TString), intent(inout) :: dst  !! reference to the destination TString.
    type(TString), intent(in)    :: src  !! reference to the source TString.
    !/ -----------------------------------------------------------------------------------
    dst%cstr = src%cstr
  end subroutine ts_external_copy


  !/ =====================================================================================
  subroutine ts_external_to_alloc_char( dst, ts )
    !/ -----------------------------------------------------------------------------------
    !! Copy from a TString to an allocatable character.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=:), allocatable, intent(out) :: dst  !! destination allocatable character.
    type(TString),                  intent(in)  :: ts  !! reference to the source TString.
    !/ -----------------------------------------------------------------------------------
    dst = ts%cstr
  end subroutine ts_external_to_alloc_char


  !/ =====================================================================================
  subroutine ts_external_from_alloc_char( ts, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy from one TString to another.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TString), intent(inout) :: ts   !! reference to the destination TString.
    character(*),   intent(in)    :: src  !! reference to the source character.
    !/ -----------------------------------------------------------------------------------
    ts%cstr = src
  end subroutine ts_external_from_alloc_char


  !/ =====================================================================================
  function ts_concatinate_ss( lh_ts, rh_ts ) result(dst)
    !/ -----------------------------------------------------------------------------------
    !! Concatinate
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(TString), intent(in)  :: lh_ts
    type(TString), intent(in)  :: rh_ts
    type(TString) :: dst
    !/ -----------------------------------------------------------------------------------
    dst%cstr = lh_ts%cstr // rh_ts%cstr
  end function ts_concatinate_ss


  !/ =====================================================================================
  function ts_concatinate_sc( lh_ts, rh_char ) result(dst)
    !/ -----------------------------------------------------------------------------------
    !! Concatinate
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(TString), intent(in)  :: lh_ts
    character(*),  intent(in)  :: rh_char
    type(TString) :: dst
    !/ -----------------------------------------------------------------------------------
    dst%cstr = lh_ts%cstr // rh_char
  end function ts_concatinate_sc


  !/ =====================================================================================
  function ts_concatinate_cs( lh_char, rh_ts ) result(dst)
    !/ -----------------------------------------------------------------------------------
    !! Concatinate
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),  intent(in)  :: lh_char
    type(TString), intent(in)  :: rh_ts
    type(TString) :: dst
    !/ -----------------------------------------------------------------------------------
    dst%cstr = lh_char // rh_ts%cstr
  end function ts_concatinate_cs

  
end module string_mod
