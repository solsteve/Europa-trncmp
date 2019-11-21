!/ ====================================================================== BEGIN FILE =====
!/ **                                  G I S _ T E S T                                  **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2017, Stephen W. Soliday                                           **
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
module gis_test
  !/ -------------------------------------------------------------------------------------
  use gis_mod
  implicit none

  
  real(dp), parameter :: mX = 5.0d3
  real(dp), parameter :: sX = 1.0d2
  
  real(dp), parameter :: mY = 5.0d3
  real(dp), parameter :: sY = 1.0d2


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  !/ =====================================================================================
  function randDist( mu, std ) result( m )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: mu
    real(dp), intent(in) :: std
    real(dp)             :: m
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    call random_number(x)
    m = mu + std*(D_TWO*x-D_ONE)
  end function randDist
    
  
  !/ =====================================================================================
  subroutine randAngle(r)
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: r
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    call random_number(x)
    r = D_2PI*x
  end subroutine randAngle
    
  
  !/ =====================================================================================
  function randInt(a,b) result(n)
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: a
    integer, intent(in) :: b
    integer             :: n
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    call random_number(x)
    n = a + int(floor(x*real(b-a+1,dp) + 0.5))
    if ( n.lt.a ) n = a
    if ( n.gt.b ) n = b
  end function randInt
    
  !/ =====================================================================================
  subroutine randLat( x )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: x
    !/ -----------------------------------------------------------------------------------
    integer :: n
    !/ -----------------------------------------------------------------------------------
    x = real(5*randInt( -16, 16 ), dp)   
  end subroutine randLat


  !/ =====================================================================================
  subroutine randLon( x )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: x
    !/ -----------------------------------------------------------------------------------
    integer :: n
    !/ -----------------------------------------------------------------------------------
    x = real(5*randInt( -72, 72 ), dp)   
  end subroutine randLon


  !/ =====================================================================================
  subroutine TEST01
    !/ -----------------------------------------------------------------------------------
    implicit none

    integer :: hist(10)
    integer :: i, n

    hist = 0
    
    do i=1,10000
       n = randInt(3,8)
       hist(n) = hist(n) + 1
    end do

    print *, hist
    
  end subroutine TEST01
  
  !/ =====================================================================================
  subroutine TEST02
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp) :: x1, x2, x3, x4, y1, y2, y3, y4, a1, a2, area
    real(dp) :: rx1, rx2, rx3, rx4, ry1, ry2, ry3, ry4
    real(dp) :: rot, lon, lat, mlat, mlon
    real(dp) :: lonUL, lonUR, lonLL, lonLR
    real(dp) :: latUL, latUR, latLL, latLR
    integer  :: i
    !/ -----------------------------------------------------------------------------------

    write(*,50)

    do i=1,500

    x1 = -randDist( mX, sX )
    x2 =  randDist( mX, sX )
    x3 =  randDist( D_ZERO, sX )
    x4 =  randDist( D_ZERO, sX )

    y1 =  D_ZERO
    y2 =  D_ZERO
    y3 =  randDist( mY, sY )
    y4 = -randDist( mY, sY )

    a1 = D_HALF*y3*(x2-x1)
    a2 = D_HALF*y4*(x1-x2)

    area = a1 + a2

    call randAngle(rot)

    call rotate( rx1, ry1, x1, y1, rot )
    call rotate( rx2, ry2, x2, y2, rot )
    call rotate( rx3, ry3, x3, y3, rot )
    call rotate( rx4, ry4, x4, y4, rot )

    call randLon( lon )
    call randLat( lat )

    mlat = meters_per_degree_latitude( lat )
    mlon = meters_per_degree_longitude( lat )

    lonLL = lon + rx1 / mlon
    lonUR = lon + rx2 / mlon
    lonUL = lon + rx3 / mlon
    lonLR = lon + rx4 / mlon

    latLL = lat + ry1 / mlat
    latUR = lat + ry2 / mlat
    latUL = lat + ry3 / mlat
    latLR = lat + ry4 / mlat

    write(*,130) area*1.0e-6, lon, lat, &
         &       lonUL,latUL, lonUR,latUR, lonLL,latLL, lonLR,latLR, &
         &       rx3,ry3, rx2,ry2, rx1,ry1, rx4,ry4

 end do
 
50 format( '"area","clon","clat"', &
        &  ',"lonUL","latUL","lonUR","latUR","lonLL","latLL","lonLR","latLR"', &
        &  ',"xUL","yUL","xUR","yUR","xLL","yLL","xLR","yLR"')
    
130 format( F10.6,5(',',F10.5,',',F9.5),8(',',F10.3) )

  end subroutine TEST02
  
end module gis_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use gis_test
  implicit none

  call random_seed()
  
  call TEST02
  
end program main

!/ =======================================================================================
!/ **                                  G I S _ T E S T                                  **
!/ =========================================================================== END FILE ==
