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
  !! author: Stephen W. Soliday
  !! date:   2017-03-30
  !! license: GPL
  !! 
  !!##Tran-Comp Environment
  !! 
  !! Collection of definitions for common developmental environment
  ! 
  !/ -------------------------------------------------------------------------------------
  use constants_env
  use math_aux
  use copy_mod
  use zero_mod
  use summation_mod
  implicit none

  
  integer, public, parameter :: MAX_PATH = 128 !! Maximum charaters in file path


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


end module trncmp_env

!/ =======================================================================================
!/ **                                T R N C M P _ E N V                                **
!/ =========================================================================== END FILE ==
