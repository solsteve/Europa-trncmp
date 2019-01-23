!/ ====================================================================== BEGIN FILE =====
!/ **                             P O L Y _ C A S T _ M O D                             **
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
module poly_cast_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-05-06
  !! license: GPL
  !!
  !!##Cast Polymorphic.
  !!
  !! Provides a double linked list of integers by wrapping the procedures from the
  !! object_deque_class module.
  !
  !/ -------------------------------------------------------------------------------------
  use iso_fortran_env
  implicit none
  private

  public :: toObject, castCharacter, castLogical, castInteger, castSingle, castDouble, castComplex

  integer, parameter :: qp=REAL128
  integer, parameter :: dp=REAL64
  integer, parameter :: sp=REAL32


  !/ -------------------------------------------------------------------------------------
  interface toObject
     !/ ----------------------------------------------------------------------------------
     module procedure :: charater_to_object
     module procedure :: logical_to_object
     module procedure :: integer_to_object
     module procedure :: single_to_object
     module procedure :: double_to_object
     module procedure :: complex_to_object
  end interface toObject


  !/ -------------------------------------------------------------------------------------
  interface castCharacter
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_character
  end interface castCharacter

  
  !/ -------------------------------------------------------------------------------------
  interface castLogical
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_logical
  end interface castLogical

  
  !/ -------------------------------------------------------------------------------------
  interface castInteger
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_integer
  end interface castInteger

  
  !/ -------------------------------------------------------------------------------------
  interface castSingle
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_single
  end interface castSingle

  
  !/ -------------------------------------------------------------------------------------
  interface castDouble
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_double
  end interface castDouble

  
  !/ -------------------------------------------------------------------------------------
  interface castComplex
     !/ ----------------------------------------------------------------------------------
     module procedure :: object_to_complex
  end interface castComplex


  
  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function charater_to_object( val ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a character string into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: val !! the input character string.
    class(*), pointer        :: obj !! return an unlimited polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=val )

  end function charater_to_object


  !/ =====================================================================================
  function logical_to_object( val ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a logical into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical,  intent(in) :: val !! the input logical.
    class(*), pointer    :: obj !! return an unlimited polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=val )

  end function logical_to_object


  !/ =====================================================================================
  function integer_to_object( val ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a single precision into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: val !! the input integer.
    class(*), pointer    :: obj !! return an unlimited polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=val )

  end function integer_to_object


  !/ =====================================================================================
  function single_to_object( val ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a single precision into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), intent(in) :: val !! the input single precision.
    class(*), pointer    :: obj !! return the single precision as an unlimited
    !!                             polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=val )

  end function single_to_object


  !/ =====================================================================================
  function double_to_object( val ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a double precision into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: val !! the input double precision.
    class(*), pointer    :: obj !! return the double precision as an unlimited
    !!                             polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=val )

  end function double_to_object


  !/ =====================================================================================
  function complex_to_object( val ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Convert a double precision complex into an unlimited polymorphic object.
    !! This is a deep copy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), intent(in) :: val !! the input double precision complex.
    class(*),    pointer    :: obj !! return the double precision complex as an
    !!                                unlimited polymorphic object.
    !/ -----------------------------------------------------------------------------------

    allocate( obj, source=val )

  end function complex_to_object








  !/ =====================================================================================
  function object_to_character( obj, stat ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a character string.
    !!
    !! |  stat  | errmsg             |
    !! | :----: | ------------------ |
    !! |    0   | n/a                |
    !! |    1   | object is NULL     |
    !! |    2   | data not a logical |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! unlimited polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    character(len=:), allocatable       :: val    !! logical return value
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    val   = ""
    istat = 0

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( character(*) )
          val = obj
       class default
          istat = 2
       end select
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function object_to_character


  !/ =====================================================================================
  function object_to_logical( obj, stat ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a logical.
    !!
    !! |  stat  | errmsg             |
    !! | :----: | ------------------ |
    !! |    0   | n/a                |
    !! |    1   | object is NULL     |
    !! |    2   | data not a logical |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! unlimited polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    logical                             :: val    !! logical return value
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    val   = .false.
    istat = 0

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( logical )
          val = obj
          class default
          istat = 2
       end select
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function object_to_logical


  !/ =====================================================================================
  function object_to_integer( obj, stat ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into an integer.
    !!
    !! |  stat  | errmsg              |
    !! | :----: | ------------------- |
    !! |    0   | n/a                 |
    !! |    1   | object is NULL      |
    !! |    2   | data not an integer |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! unlimited polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    integer                             :: val    !! integer return value
    !/ -----------------------------------------------------------------------------------
    integer :: istat

    val   = 0
    istat = 0

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( integer )
          val = obj
          class default
          istat = 2
       end select
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function object_to_integer


  !/ =====================================================================================
  function object_to_single( obj, stat ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a single precision.
    !!
    !! |  stat  | errmsg                      |
    !! | :----: | --------------------------- |
    !! |    0   | n/a                         |
    !! |    1   | object is NULL              |
    !! |    2   | data not a single precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! unlimited polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    real(sp)                            :: val    !! single precision return value
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    
    val   = 0.0_sp
    istat = 0

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( real(sp) )
          val = obj
          class default
          istat = 2
       end select
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function object_to_single


  !/ =====================================================================================
  function object_to_double( obj, stat ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a double precision.
    !!
    !! |  stat  | errmsg                      |
    !! | :----: | --------------------------- |
    !! |    0   | n/a                         |
    !! |    1   | object is NULL              |
    !! |    2   | data not a double precision |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*),     pointer,  intent(in)  :: obj    !! unlimited polymorphic object.
    integer,      optional, intent(out) :: stat   !! optional error status.
    real(dp)                            :: val    !! double precision return value
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    
    val   = 0.0_dp
    istat = 0

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( real(dp) )
          val = obj
          class default
          istat = 2
       end select
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function object_to_double


  !/ =====================================================================================
  function object_to_complex( obj, stat ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convert an unlimited polymorphic object into a double precision.
    !!
    !! |  stat  | errmsg                              |
    !! | :----: | ----------------------------------- |
    !! |    0   | n/a                                 |
    !! |    1   | object is NULL                      |
    !! |    2   | data not a double precision complex |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), pointer,  intent(in)  :: obj   !! unlimited polymorphic object.
    integer,  optional, intent(out) :: stat  !! optional error status.
    complex(dp)                     :: val   !! double precision complex return value
    !/ -----------------------------------------------------------------------------------
    integer :: istat
    
    val   = cmplx( 0.0, 0.0, dp )
    istat = 0

    if ( associated( obj ) ) then
       select type ( obj )
       type is ( complex(dp) )
          val = obj
          class default
          istat = 2
       end select
    else
       istat = 1
    end if

    if ( present( stat ) ) stat = istat

  end function object_to_complex


end module poly_cast_mod

!/ =======================================================================================
!/ **                             P O L Y _ C A S T _ M O D                             **
!/ =========================================================================== END FILE ==
