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
module stopwatch_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-23
  !! license: GPL
  !!
  !!##Stop Watch
  !!
  !! Provides a class for performing high resolution timing.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  public :: stopwatch

  !/ =====================================================================================
  type :: stopwatch
     !/ ----------------------------------------------------------------------------------
     real(dp) :: start_time = 0.0d0

   contains

     procedure :: sw_reset_internals

     procedure, public :: reset => sw_reset_internals
     procedure, public :: check => sw_get_diff_time

  end type stopwatch

  !/ -------------------------------------------------------------------------------------
  interface stopwatch
     !/ -----------------------------------------------------------------------------------
     module procedure sw_create
  end interface stopwatch




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  type(stopwatch) function sw_create()
    !/ -----------------------------------------------------------------------------------
    !! Create and set the timer
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------

    call sw_create%sw_reset_internals()

  end function sw_create


  !/ =====================================================================================
  subroutine sw_reset_internals( self )
    !/ -----------------------------------------------------------------------------------
    !! Start the timmer.
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    class(stopwatch) :: self  !! reference to this stop watch.
    !/ -----------------------------------------------------------------------------------

    call cpu_time( self%start_time )

  end subroutine sw_reset_internals


  !/ =====================================================================================
  function sw_get_diff_time( self ) result( diff )
    !/ -----------------------------------------------------------------------------------
    !! Return the time that lapsed between reset and now.
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    class(stopwatch) :: self  !! reference to this stop watch.
    real(dp)         :: diff  !! elapsed time in seconds.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------

    call cpu_time( t )
    diff = t - self%start_time

  end function sw_get_diff_time


end module stopwatch_class


!/ =======================================================================================
!/ **                           S T O P W A T C H _ C L A S S                           **
!/ =========================================================================== END FILE ==
