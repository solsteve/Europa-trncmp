!/ ====================================================================== BEGIN FILE =====
!/ **                             Z E R O _ C O P Y _ M O D                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
!/ ----- Modification History ------------------------------------------------------------
!
!! Zero and Copy procedures.
!!
!! Provides generic copy and zero procedures.
!!
!! author: Stephen W. Soliday
!! date:   2015-10-23
!
!/ =======================================================================================
module px_blas
  !/ -------------------------------------------------------------------------------------
  use constants_env
  implicit none

  !/ -------------------------------------------------------------------------------------


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine btest( msg )
    !/ -----------------------------------------------------------------------------------
    !! Test function will display a message on the standard out file unit.
    !! @note The dummy array containing tokens must allocatable and its character elements must have the same length of the input
    !! string. If the length of the delimiter is higher than the input string one then the output tokens array is allocated with
    !! only one element set to input string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*), intent(in) :: msg !! message to be displayed
    !/ -----------------------------------------------------------------------------------

    write(*,*) 'heather', msg

  end subroutine btest



end module px_blas


!/ =======================================================================================
!/ **                             Z E R O _ C O P Y _ M O D                             **
!/ =========================================================================== END FILE ==
