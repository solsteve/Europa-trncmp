!/ ====================================================================== BEGIN FILE =====
!/ **                                  M A T H _ A U X                                  **
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
!/ =======================================================================================
module math_aux
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2015-10-23
  !! license: GPL
  !!
  !!##Auxillary Mathematical Functions
  !!
  !/ -------------------------------------------------------------------------------------
  use constants_env
  implicit none

  private :: sum_one_to_n

  interface nsum
     module procedure :: sum_one_to_n
  end interface nsum

  interface n2sum
     module procedure :: sumsq_one_to_n
  end interface n2sum



  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  pure function sum_one_to_n( n ) result( s )
    !/ -----------------------------------------------------------------------------------
    !! Sum all integers one to n inclusive.
    !/ -----------------------------------------------------------------------------------
    integer, intent(in)  :: n !! number to sum to
    integer              :: s !! resulting sum
    !/ -----------------------------------------------------------------------------------
    s = n*(n + 1)/2
  end function sum_one_to_n

   !/ =====================================================================================
  pure function sumsq_one_to_n( n ) result( s )
    !/ -----------------------------------------------------------------------------------
    !! Sum all squares of integers one to n inclusive.
    !/ -----------------------------------------------------------------------------------
    integer, intent(in)  :: n !! number to sum to
    integer              :: s !! resulting sum
    !/ -----------------------------------------------------------------------------------
    s = n*(n + 1)*(2*n + 1)/6
    !s = n*n*n/3 + n*n/2 + n/6
  end function sumsq_one_to_n
  
  end module math_aux

  !/ =======================================================================================
  !/ **                                  M A T H _ A U X                                  **
  !/ =========================================================================== END FILE ==
