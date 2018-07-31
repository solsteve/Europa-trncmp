!/ ====================================================================== BEGIN FILE =====
!/ **                           S T O P W A T C H _ C L A S S                           **
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
module config_entry_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2017-03-13
  !! license: GPL
  !!
  !! Provides a Configuration Database Entry.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use string_tools
  implicit none
  private

  
  !/ =====================================================================================
  type :: config_entry
     !/ ----------------------------------------------------------------------------------
     character(len=:), allocatable :: key
     character(len=:), allocatable :: val
     character(len=:), allocatable :: com
     
   !contains

    ! procedure, public :: delete   => entry_delete
    ! procedure, public :: clear    => entry_clear
    ! procedure, public :: set      => entry_set_kv, entry_set_kvc
    ! procedure, public :: parse    => entry_parse
    ! procedure, public :: toString => entry_to_string

  end type config_entry
  
public :: config_entry




!/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

end module config_entry_class
