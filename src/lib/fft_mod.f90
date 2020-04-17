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
  !! Provides an implementation of the Cooley-Tukey Fast Fouier Transform, and
  !! auto correlation.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2018-11-23
  !! license: GPL
  !/ -------------------------------------------------------------------------------------
  use trncmp_env


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  recursive subroutine fft(x)
    !/ -----------------------------------------------------------------------------------
    !! Fast Fourier Transform, in place.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(inout)  :: x !! real array
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
       t=exp(cmplx(D_ZERO,-D_2PI*real(i-1,dp)/real(N,dp),dp))*even(i)
       x(i)     = odd(i) + t
       x(i+N/2) = odd(i) - t
    end do

    deallocate(odd)
    deallocate(even)

  end subroutine fft

  
  !/ =====================================================================================
  recursive subroutine ifft(x)
    !/ -----------------------------------------------------------------------------------
    !! Inverse Fast Fourier Transform, in place.
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(dp), dimension(:), intent(inout)  :: x !! real array
    !/ -----------------------------------------------------------------------------------
    integer  :: i, N
    real(dp) :: fn
    !/ -----------------------------------------------------------------------------------

    N  = size(x)
    fn = real(N,dp)

    do concurrent(i=1:N)
       x(i) = conjg(x(i))
    end do

    call fft( x )
    
    do concurrent(i=1:N)
       x(i) = conjg(x(i)) / fn
    end do

  end subroutine ifft


  !/ =====================================================================================
  function find_p2( n ) result( p )
    !/ -----------------------------------------------------------------------------------
    !/ Pad to power of two greater than or equal to n.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer             :: p !! return length
    integer, intent(in) :: n !! length
    !/ -----------------------------------------------------------------------------------
    integer :: e
    !/ -----------------------------------------------------------------------------------

    e = int( floor(log(real(n,dp))/log(2.0d0)) )
    p = n
    if ( 2**e .ne. n ) then
       p = int(2 ** (e + 1))
    end if
      
  end function find_p2
  

  !/ =====================================================================================
  subroutine auto_correlate( R, X )
    !/ -----------------------------------------------------------------------------------
    !/ Auto correlation.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), allocatable, intent(out) :: R(:) !! auto correlation.
    real(dp),              intent(in)  :: X(:) !! real array.
    !/ -----------------------------------------------------------------------------------
    complex(dp), allocatable :: S(:)
    integer                  :: i, N, P
    real(dp)                 :: fn, mean
    !/ -----------------------------------------------------------------------------------

    N  = size(X)
    P  = find_p2( N )
    fn = real( N, dp )

    allocate( R(N) )
    allocate( S(P) )

    mean = D_ZERO
    do concurrent(i=1:N)
       mean = mean + X(I)
    end do
    mean = mean / fn

    do concurrent(i=1:N)
       S(i) = cmplx( X(i) - mean, 0.0d0, dp )
    end do
    do concurrent(i=N+1:P)
       S(i) = cmplx( 0.0d0, 0.0d0, dp )
    end do

    call fft( S )

    do concurrent(i=1:N)
       S(i) = S(i) * conjg( S(i) )
    end do
    
    call ifft( S )

    do concurrent(i=1:N)
       R(i) = real( S(i), dp )
    end do

    deallocate( S )

  end subroutine auto_correlate
  

end module fft_mod

!/ =======================================================================================
!/ **                                   F F T _ M O D                                   **
!/ =========================================================================== END FILE ==
