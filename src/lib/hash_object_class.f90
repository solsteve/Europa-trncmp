!/ ====================================================================== BEGIN FILE =====
!/ **                         H A S H _ O B J E C T _ C L A S S                         **
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
module hash_object_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-03
  !! license: GPL
  !!
  !! Provides a simple hash map of unlimited polymorphic objects.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use sllist_object_class
  use poly_cast_mod
  implicit none
  private

  
  type HashSlot
     class(SLList), pointer :: list
  end type HashSlot

  
  type ObjectHash
     type(HashSlot), allocatable, dimension(:) :: slots
  end type ObjectHash

  
  interface computeHash
     module procedure :: object_to_hash
  end interface computeHash


  

  
  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function string_to_hash( key, mod ) result( hash )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: key  !! string key to hash.
    integer,      intent(in) :: mod  !! modulo to aply to the hash.
    integer                  :: hash !! return hash.
    !/ -----------------------------------------------------------------------------------

    hash = 1

  end function string_to_hash


  !/ =====================================================================================
  function integer_to_hash( key, mod ) result( hash )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: key  !! integer key to hash.
    integer, intent(in) :: mod  !! modulo to aply to the hash.
    integer             :: hash !! return hash.
    !/ -----------------------------------------------------------------------------------

    hash = MODULO( key, mod )

  end function integer_to_hash


  !/ =====================================================================================
  function single_to_hash( key, mod ) result( hash )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp), intent(in) :: key  !! single precision key to hash.
    integer,  intent(in) :: mod  !! modulo to aply to the hash.
    integer              :: hash !! return hash.
    !/ -----------------------------------------------------------------------------------

    hash = 1

  end function single_to_hash


  !/ =====================================================================================
  function double_to_hash( key, mod ) result( hash )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: key  !! single precision key to hash.
    integer,  intent(in) :: mod  !! modulo to aply to the hash.
    integer              :: hash !! return hash.
    !/ -----------------------------------------------------------------------------------

    hash = 1

  end function double_to_hash


  !/ =====================================================================================
  function object_to_hash( key, mod ) result( hash )
    !/ -----------------------------------------------------------------------------------
    !! Creates a HASH from an integer key with modulo.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(*), intent(in) :: key  !! single precision key to hash.
    integer,  intent(in) :: mod  !! modulo to aply to the hash.
    integer              :: hash !! return hash.
    !/ -----------------------------------------------------------------------------------

    select type( key )
    type is( character(*) )
       hash = string_to_hash( key, mod )
    type is( integer )
       hash = integer_to_hash( key, mod )
    type is( real(sp) )
       hash = single_to_hash( key, mod )
    type is( real(dp) )
       hash = double_to_hash( key, mod )
    end select
    
  end function object_to_hash











  
end module hash_object_class

!/ =======================================================================================
!/ **                         H A S H _ O B J E C T _ C L A S S                         **
!/ =========================================================================== END FILE ==
