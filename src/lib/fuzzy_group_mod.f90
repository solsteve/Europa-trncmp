!/ ====================================================================== BEGIN FILE =====
!/ **                           F U Z Z Y _ G R O U P _ M O D                           **
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
module fuzzy_group_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a collection of tools for geographical information services.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-12-28
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use fuzzy_partition_mod
  use string_tools
  implicit none

  !/ =====================================================================================
  type :: FuzzyGroup
     !/ ----------------------------------------------------------------------------------

     type(FuzzyPartition_ptr), allocatable :: fpart(:)  !!  array of fuzzy partitions.

   contains

     procedure, private :: fg_init_default
     procedure, private :: fg_init_parts
     procedure, private :: fg_init_params
     procedure, private :: fg_resize

     procedure :: part      => fg_get_set
     procedure :: nIn       => fg_get_number_inputs
     procedure :: nOut      => fg_get_number_outputs
     procedure :: size      => fg_get_storage_size

     procedure :: fuzzify   => fg_fuzzify
     procedure :: defuzzify => fg_defuzzify

     procedure :: load      => fg_load
     procedure :: store     => fg_store
     procedure :: read      => fg_read
     procedure :: write     => fg_write

     procedure :: destroy   => fg_destroy

     final :: fg_final

     generic :: init => fg_init_default, fg_init_parts, fg_init_params

  end type FuzzyGroup




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  subroutine fg_init_default( dts, n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer,           intent(in)    :: n 
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_init_default


  !/ =====================================================================================
  subroutine fg_init_parts( dts )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_init_parts


  !/ =====================================================================================
  subroutine fg_init_params( dts, buffer )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    real(dp),          intent(in)    :: buffer(:)
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_init_params


  !/ =====================================================================================
  subroutine fg_resize( dts, n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer,           intent(in)    :: n 
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_resize


  !/ =====================================================================================
  subroutine fg_final( dts )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_final


  !/ =====================================================================================
  subroutine fg_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_destroy


  !/ =====================================================================================
  subroutine fg_get_set( dts )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_get_set


  !/ =====================================================================================
  function fg_get_number_inputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer                          :: n 
    !/ -----------------------------------------------------------------------------------


  end function fg_get_number_inputs


  !/ =====================================================================================
  function fg_get_number_outputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer                          :: n 
    !/ -----------------------------------------------------------------------------------


  end function fg_get_number_outputs


  !/ =====================================================================================
  function fg_get_storage_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer                          :: n 
    !/ -----------------------------------------------------------------------------------


  end function fg_get_storage_size


  !/ =====================================================================================
  subroutine fg_fuzzify( dts, mu, x )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts    !! reference to this FuzzyGroup.
    real(dp),          intent(out)   :: mu(:)  !! 
    real(dp),          intent(in)    :: x(:)   !! 
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_fuzzify


  !/ =====================================================================================
  subroutine fg_defuzzify( dts, x, mu )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts    !! reference to this FuzzyGroup.
    real(dp),          intent(out)   :: x(:)   !! 
    real(dp),          intent(in)    :: mu(:)  !! 
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_defuzzify


  !/ =====================================================================================
  function fg_load( dts, buffer, INDEX ) result( post_index )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    real(dp),          intent(in)    :: buffer(:)   !! 
    integer, optional, intent(in)    :: INDEX
    integer                          :: post_index
    !/ -----------------------------------------------------------------------------------


  end function fg_load


  !/ =====================================================================================
  function fg_store( dts, buffer, INDEX ) result( post_index )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    real(dp),          intent(out)   :: buffer(:)   !! 
    integer, optional, intent(in)    :: INDEX
    integer                          :: post_index
    !/ -----------------------------------------------------------------------------------


  end function fg_store


  !/ =====================================================================================
  subroutine fg_read( dts, un, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts     !! reference to this FuzzyGroup.
    integer,           intent(in)    :: un      !! file unit.
    integer,           intent(out)   :: IOSTAT  !! return error code.
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_read


  !/ =====================================================================================
  subroutine fg_write( dts, un, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer,           intent(in)    :: un      !! file unit.
    integer,           intent(out)   :: IOSTAT  !! return error code.
    !/ -----------------------------------------------------------------------------------


  end subroutine fg_write


end module fuzzy_group_mod


!/ =======================================================================================
!/ **                           F U Z Z Y _ G R O U P _ M O D                           **
!/ =========================================================================== END FILE ==
