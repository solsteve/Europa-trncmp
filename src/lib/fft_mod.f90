!/ ====================================================================== BEGIN FILE =====
!/ **                                   F F T _ M O D                                   **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module fft_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-11-23
  !! license: GPL
  !!
  !! Provides an implementation of the Colley-Tukey Fast Fouier Transform
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  recursive subroutine fft(x)
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(inout)  :: x
    !/ -----------------------------------------------------------------------------------
    complex(dp)              :: t
    integer                  :: N, i
    complex(dp), allocatable :: even(:)
    complex(dp), allocatable :: odd(:)
    !/ -----------------------------------------------------------------------------------

    N=size(x)

    if( N .le. 1 ) return

    allocate( odd((N+1)/2) )
    allocate( even(N/2) )

    ! divide
    odd  = x(1:N:2)
    even = x(2:N:2)

    ! conquer
    call fft(odd)
    call fft(even)

    ! combine
    do i=1,N/2
       t=exp(cmplx(0.0_dp,-2.0_dp*N_PI*real(i-1,dp)/real(N,dp),kind=dp))*even(i)
       x(i)     = odd(i) + t
       x(i+N/2) = odd(i) - t
    end do

    deallocate(odd)
    deallocate(even)

  end subroutine fft


end module fft_mod

!/ =======================================================================================
!/ **                                   F F T _ M O D                                   **
!/ =========================================================================== END FILE ==
