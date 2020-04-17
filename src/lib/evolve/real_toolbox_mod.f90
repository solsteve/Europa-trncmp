!/ ====================================================================== BEGIN FILE =====
!/ **                          R E A L _ T O O L B O X _ M O D                          **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 1994-2020, Stephen W. Soliday                                      **
!/ **                           stephen.soliday@trncmp.org                              **
!/ **                           http://research.trncmp.org                              **
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
module real_toolbox_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use dice_mod
  implicit none

  type(Dice) :: dd


  interface initialize_parameters
     module procedure :: initialize_real_params
  end interface initialize_parameters

  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine initialize_real_params( param )
    !/ -----------------------------------------------------------------------------------
    !! Set parameters to uniform distribution [0,1)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp) :: param(:) !! array of parameters.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(param)

    do i=1,n
       param(i) = D_TWO * dd%uniform() - D_ONE
    end do
    
  end subroutine initialize_real_params
    

  !/ =====================================================================================
  subroutine cross_real_params( c1, c2, p1, p2, pcross )
    !/ -----------------------------------------------------------------------------------
    !! Perform parametric crossover.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp) :: c1(:)  !! array of first  child  parameters.
    real(dp) :: c2(:)  !! array of second child  parameters.
    real(dp) :: p1(:)  !! array of first  parent parameters.
    real(dp) :: p2(:)  !! array of second parent parameters.
    real(dp) :: pcross !! probability of crossover
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: t, omtg
    !/ -----------------------------------------------------------------------------------
    n = size(p1)

    if ( dd%boolean(pcross) ) then
       t   = dd%uniform()
       omt = D_ZERO - t
       do i=1,n
          c1(i) = omt*p1(i) + t*p2(i)
          c2(i) = omt*p2(i) + t*p1(i)
       end do
    else
       do i=1,n
          c1(i) = p1(i)
          c2(i) = p2(i)
       end do
    end if
    
    
  end subroutine initialize_real_params
    

end module real_toolbox_mod


!/ =======================================================================================
!/ **                          R E A L _ T O O L B O X _ M O D                          **
!/ ======================================================================== END FILE =====
