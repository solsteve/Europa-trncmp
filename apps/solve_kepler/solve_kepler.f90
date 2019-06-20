!/ ====================================================================== BEGIN FILE =====
!/ **                              S O L V E _ K E P L E R                              **
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
module solve_kepler
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use kepler_mod
  implicit none


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine Test_01
    !/ -----------------------------------------------------------------------------------
    implicit none

    integer  :: N, cnt
    real(dp) :: M, dM, ecc, E, rM, fn, ep, S_err, N_err, C3_err, C6_err
    real(dp) :: diff, S_cnt, N_cnt, C3_cnt, C6_cnt

    N   = 100
    fn  = real(N,dp)
    ep  = epsilon(1.0_dp)
    ep  = 1.0D-12

    M   = 0.0D0
    dM  = D_2PI / fn
    ecc = 1.0D-1

    S_err = 0.0D0
    S_cnt = 0.0D0

    N_err = 0.0D0
    N_cnt = 0.0D0

    C3_err = 0.0D0
    C3_cnt = 0.0D0

    C6_err = 0.0D0
    C6_cnt = 0.0D0

100 continue
    !/ -----------------------------------------------------------
    E  = Simple( ecc, M, epsilon=ep, accit=cnt )
    rM = Kepler( ecc, E )
    diff = M - rM
    S_err = S_err + (diff*diff)
    S_cnt = S_cnt + real(cnt,dp)

    !/ -----------------------------------------------------------
    E  = K_Newton( ecc, M, epsilon=ep, accit=cnt )
    rM = Kepler( ecc, E )
    diff = M - rM
    N_err = N_err + (diff*diff)
    N_cnt = N_cnt + real(cnt,dp)

    !/ -----------------------------------------------------------
    E  = Center3( ecc, M, epsilon=ep, accit=cnt )
    rM = Kepler( ecc, E )
    diff = M - rM
    C3_err = C3_err + (diff*diff)
    C3_cnt = C3_cnt + real(cnt,dp)

    !/ -----------------------------------------------------------
    E  = Center6( ecc, M, epsilon=ep, accit=cnt )
    rM = Kepler( ecc, E )
    diff = M - rM
    C6_err = C6_err + (diff*diff)
    C6_cnt = C6_cnt + real(cnt,dp)

    !/ -----------------------------------------------------------
    M = M + dM
    if ( M .GT. D_2PI ) goto 200
    goto 100
200 continue

    S_err = S_err / fn
    S_cnt = S_cnt / fn

    N_err = N_err / fn
    N_cnt = N_cnt / fn

    C3_err = C3_err / fn
    C3_cnt = C3_cnt / fn

    C6_err = C6_err / fn
    C6_cnt = C6_cnt / fn

    write(*,*) 'Simple Iterator  ',sqrt(S_err),  S_cnt
    write(*,*) 'Newton Iterator  ',sqrt(N_err),  N_cnt
    write(*,*) 'Center 3rd Order ',sqrt(C3_err), C3_cnt
    write(*,*) 'Center 6th Order ',sqrt(C6_err), C6_cnt

  end subroutine Test_01



  !/ =====================================================================================
  function GEN(x) result(y)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)             :: y
    real(dp), intent(in) :: x
    y = bessel_J0(x)
  end function GEN

  !/ =====================================================================================
  function chebft( a, b, n ) result( c )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), allocatable, dimension(:) :: c !! Coefficient list
    real(dp), intent(in) :: a                !! Domain lower bound
    real(dp), intent(in) :: b                !! Domain upper bound
    integer,  intent(in) :: n                !! Number of coefficients
    !/ -----------------------------------------------------------------------------------
    integer  :: j, k
    real(dp) :: bma, bpa, fac, y, sum
    real(dp), allocatable, dimension(:) :: f
    !/ -----------------------------------------------------------------------------------
    allocate( c(n) )
    allocate( f(n) )
    bma = D_HALF * (b-a)
    bpa = D_HALF * (b+a)

    do k=1,n
       y    = cos( D_PI * (real(k,dp)-D_HALF)/real(n,dp))
       f(k) = GEN( y * bma + bpa )
    end do

    fac = D_TWO / real(n,dp)
    do j=1,n
       sum=D_ZERO
       do k=1,n
          sum = sum + f(k)*cos((D_PI*real(j-1,dp))*((real(k,dp)-D_HALF)/real(n,dp)))
       end do
       c(j) = fac*sum
    end do
    deallocate( f )
  end function chebft


  !/ =====================================================================================
  function chebev( a, b, cn, m, x ) result( ap )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)             :: ap     !! approximation
    real(dp), intent(in) :: a      !! Domain lower bound
    real(dp), intent(in) :: b      !! Domain upper bound
    real(dp), intent(in) :: cn(:)  !! Chebyshev coefficients
    integer,  intent(in) :: m      !! number of coefficients to use
    real(dp), intent(in) :: x      !! Independent variable
    !/ -----------------------------------------------------------------------------------
    integer  :: j
    real(dp) :: d,dd,sv,y,y2
    !/ -----------------------------------------------------------------------------------
    ap = D_ZERO
    if (((x-a)*(x-b)).gt.D_ZERO) then
       write(ERROR_UNIT,*) 'X is outside the domain'
    else
       d  = D_ZERO
       dd = D_ZERO
       y  = (D_TWO*x-a-b)/(b-a)  ! Change of variable
       y2 = D_TWO * y

       do j=m,2,-1    ! Clenshaw's recurrence
          sv = d
          d  = (y2*d) - dd + cn(j)
          dd = sv
       end do

       ap = (y*d) - dd + (D_HALF*cn(1))
    end if
  end function chebev
  
  
  !/ =====================================================================================
  subroutine Test_02
    !/ -----------------------------------------------------------------------------------
    implicit none

    real(dp), allocatable :: Cn(:)
    integer :: i
    real(dp) :: x, dx, y1, y2

    Cn = chebft( -1.0D0, 1.0D0, 100 )
    do i=1,10
       write(*,*) Cn(i)
    end do

    write(*,*)
    write(*,*)

    x  = -0.9D0
    dx =  0.1D0

100 continue
    y1 = GEN(x)
    y2 = chebev(-1.0D0, 1.0D0, Cn, 20, x )
    write(*,*) y1, y2, y1-y2
    x = x + dx
    if ( x.lt.1.0d0 ) goto 100






    


  end subroutine Test_02
    
end module solve_kepler

!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use solve_kepler
  implicit none

  call Test_02
end program main


!/ =======================================================================================
!/ **                              S O L V E _ K E P L E R                              **
!/ =========================================================================== END FILE ==
