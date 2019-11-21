!/ ====================================================================== BEGIN FILE =====
!/ **                                   G I S _ M O D                                   **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module gis_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a collection of tools for geographical information services.
  !!
  !! author:  Stephen W. Soliday
  !! date:    20198-11-15
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  !! the folowing constants are from K6-K7 of the 2016 Astronomical Almanac
  
  real(dp), parameter :: EARTH_RAD_EQU  = 6.3781366d6    !! (m) earth equitorial radius
  real(dp), parameter :: EARTH_RAD_POL  = 6.3567516005d6 !! (m) earth polar radius
  real(dp), parameter :: EARTH_MU       = 3.986004418d14 !! (m^3/s^2) earth gravity
  real(dp), parameter :: EARTH_RFLAT    = 2.9825642d2    !! earth recipricol flattening


  !/ derived constants

  !! earth flattening
  real(dp), parameter :: EARTH_FLAT = D_ONE/EARTH_RFLAT

  !! earth eccentricity squared 2f(1-f) = 0.00669439799586579
  real(dp), parameter :: EARTH_EC2 = D_TWO*EARTH_FLAT*(D_ONE- EARTH_FLAT)

  !! earth eccentricity = 0.0818193008761734
  real(dp), parameter :: EARTH_ECC = sqrt(EARTH_EC2)
  
  ! (1 - ecc**2) = 0.993305602004134
  real(dp), parameter :: EARTH_OME2 = D_ONE - EARTH_EC2

  ! R(mean) = (2*RE + RP)/3   (m) earth mean  radius
  real(dp), parameter :: EARTH_RAD_MEAN = (D_TWO*EARTH_RAD_EQU + EARTH_RAD_POL)/D_THREE


  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  function geocentric_latitude( lat_gd ) result( lat_gc )
    !/ -----------------------------------------------------------------------------------
    !! Compute the geocentric latitude from the geodetic latitude
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in)  :: lat_gd !!  geodetic   latitude in degrees
    real(dp)              :: lat_gc !!  geocentric latitude in degrees
    !/ -----------------------------------------------------------------------------------
    lat_gc = atan( tan( lat_gd * DEG2RAD ) * EARTH_OME2 ) * RAD2DEG
  end function geocentric_latitude

  
  !/ =====================================================================================
  function geodetic_latitude( lat_gc ) result( lat_gd )
    !/ -----------------------------------------------------------------------------------
    !! Compute the geocentric latitude from the geodetic latitude
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in)  :: lat_gc !!  geocentric latitude in degrees
    real(dp)              :: lat_gd !!  geodetic   latitude in degrees
    !/ -----------------------------------------------------------------------------------
    lat_gd = atan( tan( lat_gc * DEG2RAD ) / EARTH_OME2 ) * RAD2DEG
  end function geodetic_latitude

  
  !/ =====================================================================================
  subroutine auxillary_constants( lat_gd, C, S, RSITE, RD, RK, HGT )
    !/ -----------------------------------------------------------------------------------
    !! Compute auxillary constants
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(in)  :: lat_gd !!  geodetic latitude in degrees
    real(dp),           intent(out) :: C      !!  radius of curvature in the meridian
    real(dp),           intent(out) :: S
    real(dp), optional, intent(out) :: RSITE  !!  geocentric site radius
    real(dp), optional, intent(out) :: RD     !!  semimajor axis component
    real(dp), optional, intent(out) :: RK     !!  semiminor axis component
    real(dp), optional, intent(in)  :: HGT    !!  height above the reference ellipsoid
    !/ -----------------------------------------------------------------------------------
    real(dp) :: cs, sn, x, y, Hellp
    !/ -----------------------------------------------------------------------------------
    cs = cos( lat_gd*DEG2RAD )
    sn = sin( lat_gd*DEG2RAD )

    C = EARTH_RAD_EQU / sqrt(D_ONE - EARTH_EC2*sn*sn)
    S = C * EARTH_OME2

    Hellp = D_ZERO
    if ( present( HGT ) ) Hellp = HGT

    !/ ----- compute optional constnts ---------------------------------------------------
    if ( present( RSITE ) ) then
       x = (C+Hellp)*cs
       y = (S+Hellp)*sn
       RSITE = sqrt( x*x + y*y )
    end if
    
    !/ ----- compute this individually if RSIGHT was not requested -----------------------
    if ( present( RD ) ) then
       if ( present( RSITE ) ) then
          RD = x
       else
          RD = (C+Hellp)*cs
       end if
    end if
    
    !/ ----- compute this individually if RSIGHT was not requested -----------------------
    if ( present( RK ) ) then
        if ( present( RSITE ) ) then
          RK = y
       else
          RK = (S+Hellp)*sn
       end if
    end if

  end subroutine auxillary_constants


  !/ =====================================================================================
  subroutine geodetic_to_rectangle( Rijk, lat_gd, lon, HGT )
    !/ -----------------------------------------------------------------------------------
    !! Compute rectangular coordinates from longitude and geodetic latitude
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(out) :: Rijk(3) !!  rectangle components
    real(dp),           intent(in)  :: lat_gd  !!  geodetic latitude in degrees
    real(dp),           intent(in)  :: lon     !!  longitude in degrees
    real(dp), optional, intent(in)  :: HGT     !!  height above the reference ellipsoid
    !/ -----------------------------------------------------------------------------------
    real(dp) :: C, S, rd, rk
    !/ -----------------------------------------------------------------------------------
    call auxillary_constants( lat_gd, C, S, RD=rd, RK=rk, HGT=HGT )
    Rijk(1) = rd * cos( lon * DEG2RAD )
    Rijk(2) = rd * sin( lon * DEG2RAD )
    Rijk(3) = rk
  end subroutine geodetic_to_rectangle


  !/ =====================================================================================
  function great_circle_distance( lon1, lat1, lon2, lat2, ANGLE ) result( meter )
    !/ -----------------------------------------------------------------------------------
    !! Compute great circle distance using the haversine method
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(in)  :: lon1   !!  first  longitude in degrees
    real(dp),           intent(in)  :: lat1   !!  first  latitude  in degrees
    real(dp),           intent(in)  :: lon2   !!  second longitude in degrees
    real(dp),           intent(in)  :: lat2   !!  second latitude  in degrees
    real(dp), optional, intent(out) :: ANGLE  !!  central angle
    real(dp)                        :: meter
    !/ -----------------------------------------------------------------------------------
    
    real(dp) :: Cx1, Cx2, Sx1, Sx2, ang
    real(dp) :: Cy1, Cy2, Sy1, Sy2
    !/ -----------------------------------------------------------------------------------
    Cx1 = cos(lon1*DEG2RAD)
    Cx2 = cos(lon2*DEG2RAD)
    Cy1 = cos(lat1*DEG2RAD)
    Cy2 = cos(lat2*DEG2RAD)
    Sx1 = sin(lon1*DEG2RAD)
    Sx2 = sin(lon2*DEG2RAD)
    Sy1 = sin(lat1*DEG2RAD)
    Sy2 = sin(lat2*DEG2RAD)

    ang   = acos((Cx1*Cx2 + Sx1*Sx2)*Cy1*Cy2 + Sy1*Sy2)
    meter = ang * EARTH_RAD_MEAN

    if ( present( ANGLE ) ) ANGLE = ang * RAD2DEG
  end function great_circle_distance
  

 !/ =====================================================================================
  function ellipsoid_radius( ref_lat ) result( meter )
    !/ -----------------------------------------------------------------------------------
    !! Radius of the mean elipsoid at the reference latitude.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: ref_lat !!  reference latitude in degrees
    real(dp)             :: meter   !!  earth radius at the reference latitude in meters.
    !/ -----------------------------------------------------------------------------------
    real(dp), parameter :: A0 = 6.36748980392568d+06
    real(dp), parameter :: A2 = 1.069258979408d+04
    real(dp), parameter :: A4 = 2.244474504d+01
    real(dp), parameter :: A6 = 5.102528d-02
    !/ -----------------------------------------------------------------------------------
    real(dp) :: r1, r2, r4, r6

    r1 = DEG2RAD * ref_lat
    r2 = r1 + r1
    r4 = r2 + r2
    r6 = r4 + r2
   
    !     = 6367489.8 + 10692.6*cos(r2) - 22.4*cos(r4)

    meter = A0 + A2*cos(r2) - A4*cos(r4) + A6*cos(r6)

  end function ellipsoid_radius

  
  !/ =====================================================================================
  function meters_per_degree_latitude( ref_lat, ALT ) result( meter )
    !/ -----------------------------------------------------------------------------------
    !! Meters per degree of latitude in the vacinity of the reference latitude.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(in) :: ref_lat !!  reference latitude in degrees
    real(dp), optional, intent(in) :: ALT     !!  altitude above MSL (default: 0)
    real(dp)                       :: meter   !!  meters per degree latitude
    !/ -----------------------------------------------------------------------------------
    real(dp) :: r1, r2, r4

    r1 = DEG2RAD * ref_lat
    r2 = r1 + r1
    r4 = r2 + r2

    !       111133.35 - 559.84*cos(r2) + 1.17*cos(r4)

    meter = 1.1113335d5 - 5.5984d2*cos(r2) + 1.17d0*cos(r4)

    if ( present( ALT ) ) then
       r1 = ellipsoid_radius( ref_lat )
       meter = meter * (r1 + ALT) / r1
    end if

  end function meters_per_degree_latitude
  

  !/ =====================================================================================
  function meters_per_degree_longitude( ref_lat, ALT ) result( meter )
    !/ -----------------------------------------------------------------------------------
    !! Meters per degree of longitude in the vacinity of the reference latitude.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),           intent(in) :: ref_lat !!  reference latitude in degrees
    real(dp), optional, intent(in) :: ALT     !!  altitude above MSL (default: 0)
    real(dp)                       :: meter   !!  meters per degree longitude
    !/ -----------------------------------------------------------------------------------
    real(dp) :: r1, r3, r5

    r1 = DEG2RAD * ref_lat
    r3 = r1 + r1 + r1
    r5 = r3 + r1 + r1

    !       111413.28*cos(r1) - 93.51*cos(r3) + 0.12*cos(r5)

    meter = 1.1141328d5*cos(r1) - 9.351d1*cos(r3) + 1.2d-1*cos(r5)
    
    if ( present( ALT ) ) then
       r1 = ellipsoid_radius( ref_lat )
       meter = meter * (r1 + ALT) / r1
    end if

  end function meters_per_degree_longitude
  
  
end module gis_mod


!/ =======================================================================================
!/ **                                   G I S _ M O D                                   **
!/ =========================================================================== END FILE ==
