!/ ====================================================================== BEGIN FILE =====
!/ **                                    V S C O P E                                    **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module VScope
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use statistics_mod
  use app_options_mod
  use configdb_mod
  use string_tools
  use psgraph_mod
  implicit none

  real(dp), parameter :: mag_rad(8) = [ 0.12500D0, 0.11161D0, 0.09821D0, 0.08482D0, &
       &                                0.07143D0, 0.05804D0, 0.04464D0, 0.03125D0 ]


  !/ =====================================================================================
  type :: Star_t
     !/ ----------------------------------------------------------------------------------
     integer  :: no       = -1      !! Record Number
     real(dp) :: V        = D_ZERO  !! V   Apparent Magnitude
     real(dp) :: CI       = D_ZERO  !! B-V Color Index
     real(dp) :: bearing  = D_ZERO  !! reletive position angle
     real(dp) :: radius   = D_ZERO  !! reletive distance
  end type Star_t




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function magRadius( min_mag, max_mag, mag ) result( rad )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: min_mag
    real(dp), intent(in) :: max_mag
    real(dp), intent(in) :: mag
    real(dp)             :: rad
    !/ -----------------------------------------------------------------------------------
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------

    t = ( mag - min_mag ) / ( max_mag - min_mag )

    rad = mag_rad(1)*(D_ONE-t) + mag_rad(8)*t

  end function magRadius


  !/ =====================================================================================
  subroutine drawStar( pd, red, green, blue, cx, cy, rad )
    !/ -----------------------------------------------------------------------------------
    !! Draw a star
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PSDraw), intent(inout) :: pd
    real(dp),     intent(in)    :: red     !! red   intensity
    real(dp),     intent(in)    :: green   !! green intensity
    real(dp),     intent(in)    :: blue    !! blue  intensity
    real(dp),     intent(in)    :: cx      !! center x
    real(dp),     intent(in)    :: cy      !! center y
    real(dp),     intent(in)    :: rad     !! radius
    !/ -----------------------------------------------------------------------------------
    call pd%saveColor
    call pd%setRGB( D_ONE, D_ONE, D_ONE )
    call pd%drawCircleInch( cx, cy, rad*1.1D0 )

    call pd%setRGB( red, green, blue )
    call pd%fillCircleInch( cx, cy, rad )

    call pd%restoreColor

  end subroutine drawStar


  !/ =====================================================================================
  function ra_diff( ra, ref_ra ) result( diff )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: ra     !! first  angle
    real(dp), intent(in) :: ref_ra    !! second angle
    real(dp)             :: diff   !! difference
    !/ -----------------------------------------------------------------------------------
    diff = ref_ra - ra
    if (  1.8d2 .lt. diff) diff = 3.6D2 - diff
    if ( -1.8d2 .gt. diff) diff = 3.6D2 + diff
  end function ra_diff


  !/ =====================================================================================
  function dec_diff( dec, ref_dec ) result( diff )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: dec    !! first  angle
    real(dp), intent(in) :: ref_dec    !! second angle
    real(dp)             :: diff   !! difference
    !/ -----------------------------------------------------------------------------------
    diff = ref_dec - dec
    if (  1.8d2 .lt. diff) diff = 3.6D2 - diff
    if ( -1.8d2 .gt. diff) diff = 3.6D2 + diff
  end function dec_diff


  !/ =====================================================================================
  function greatCircle( ra1, dec1, ra2, dec2 ) result( d )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: ra1     !! first  right ascension
    real(dp), intent(in) :: dec1    !! first  declination
    real(dp), intent(in) :: ra2     !! second right ascension
    real(dp), intent(in) :: dec2    !! second declination
    real(dp)             :: d       !! distance
    !/ -----------------------------------------------------------------------------------
    real(dp) :: hav_ra, hav_dc, hav_d
    !/ -----------------------------------------------------------------------------------

    hav_ra = D_HALF*(D_ONE - cos(ra2  - ra1))
    hav_dc = D_HALF*(D_ONE - cos(dec2 - dec1))

    hav_d = hav_dc + cos(dec1)*cos(dec2)*hav_ra

    d = acos(D_ONE - D_TWO*hav_d)

  end function greatCircle


  !/ =====================================================================================
  function positionAngle( ra1, dec1, ra2, dec2 ) result( rad )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: ra1     !! from  right ascension
    real(dp), intent(in) :: dec1    !! from  declination
    real(dp), intent(in) :: ra2     !! to    right ascension
    real(dp), intent(in) :: dec2    !! to    declination
    real(dp)             :: rad     !! angle in radians
    !/ -----------------------------------------------------------------------------------
    real(dp) :: num, den, del

    del = ra2 - ra1

    num = sin(del)*cos(dec2)

    den = cos(dec1)* sin(dec2) - sin(dec1) * cos(dec2) * cos(del)

    rad = D_PI_2 - atan2(num,den)

    if (rad.lt.D_ZERO) then
       rad = D_2PI + rad
    end if

  end function positionAngle


  !/ =====================================================================================
  subroutine testHaversine
    !/ -----------------------------------------------------------------------------------
    implicit none

    integer  :: i,j
    real(dp) :: dist

    real(8), parameter :: ref_ra(4) = [ 3.0D0, 357.0D0, 357.0D0,  3.0D0 ]
    real(8), parameter :: ref_dc(4) = [ 3.0D0,  3.0D0, -3.0D0, -3.0D0 ]

    real(8), parameter :: tst_ra(4) = [ 10.0D0, 350.0D0, 350.0D0,  10.0D0 ]
    real(8), parameter :: tst_dc(4) = [ 10.0D0,  10.0D0, -10.0D0, -10.0D0 ]

    print *,''
    print *,'Great Circle'
    print *,''

    do j=1,4
       do i=1,4
          dist = greatCircle( DEG2RAD*ref_ra(j), DEG2RAD*ref_dc(j), &
               &              DEG2RAD*tst_ra(i), DEG2RAD*tst_dc(i) ) * RAD2DEG

          print *,j,i,dist
       end do
    end do

  end subroutine testHaversine


  !/ =====================================================================================
  subroutine testPosAngle
    !/ -----------------------------------------------------------------------------------
    implicit none

    integer  :: i,j
    real(dp) :: posa

    real(8), parameter :: ref_ra = 5.0d0
    real(8), parameter :: ref_dc = 4.0d0

    real(8), parameter :: tst_ra(8) = [ 6.0d0, 6.0d0, 5.0d0, 4.0d0, 4.0d0, 4.0d0, 5.0d0, 6.0d0 ]
    real(8), parameter :: tst_dc(8) = [ 4.0d0, 5.0d0, 5.0d0, 5.0d0, 4.0d0, 3.0d0, 3.0d0, 3.0d0 ]

    real(8), parameter :: expect(8) = [ 0.0d0, 45.0d0, 90.0d0, 135.0d0, &
         &                              180.0d0, 225.0d0, 270.0d0, 310.0d0 ]


    print *,''
    print *,'Position Angle'
    print *,''

    do i=1,8
       posa = positionAngle( DEG2RAD*ref_ra, DEG2RAD*ref_dc, &
            &              DEG2RAD*tst_ra(i), DEG2RAD*tst_dc(i) ) * RAD2DEG

       print *,i, posa, abs(posa - expect(i))
    end do

  end subroutine testPosAngle


  !/ =====================================================================================
  subroutine testFunctions
    !/ -----------------------------------------------------------------------------------
    implicit none

    call testHaversine
    call testPosAngle

  end subroutine testFunctions


  !/ =====================================================================================
  subroutine scale( sx, sy, fov, dist, bear  )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: sx
    real(dp), intent(out) :: sy

    real(dp), intent(in)  :: fov
    real(dp), intent(in)  :: dist  !! angular distance from the reference point
    real(dp), intent(in)  :: bear  !! bearing counter clockwise from the equitoral plane
    !/ -----------------------------------------------------------------------------------
    real(dp) :: ra, dc
    !/ -----------------------------------------------------------------------------------

    ra = 7.0d0*dist*cos(bear)/fov
    dc = 7.0d0*dist*sin(bear)/fov

    sx = 3.5d0 - dc
    sy = 3.5d0 + ra

  end subroutine scale

  
  !/ =====================================================================================
  subroutine draw_page( sec, pd_title, pd_image, pd_mag, pd_data )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t),    intent(inout) :: sec
    type(PSDraw), intent(inout) :: pd_title
    type(PSDraw), intent(inout) :: pd_image
    type(PSDraw), intent(inout) :: pd_mag
    type(PSDraw), intent(inout) :: pd_data
    !/ -----------------------------------------------------------------------------------
    integer  :: i, imag
    real(dp) :: pos, rad
    character(3) :: cmag
    !/ -----------------------------------------------------------------------------------

    !call pd_title%drawRectangle(  D_ZERO, D_ZERO, D_ONE, D_ONE )

    !call pd_mag%drawBorder
    call pd_data%drawRectangle(   D_ZERO, D_ZERO, D_ONE, D_ONE )

    !call pd_image%drawCircle( 0.0D0, 0.0D0, 1.0D0, parts=128)
    call pd_image%drawCircleInch( 3.5d0, 3.5d0, 3.5d0 )

    call pd_title%write_inch( '2020-MAY-26 12:34:14.000  12h 15m 23.022s / -13o 15m 28.322s', &
         0.3D0, 0.0D0, 6.0D0, 0.35D0, 90.0D0 )

    imag = -2

    do i=1,8
       pos = real(i,dp)*0.5D0
       rad = mag_rad(i)
       call drawStar( pd_mag, D_ZERO, D_ZERO, D_ZERO, 0.5D0, pos, rad )
       write(cmag,'(I3)') imag
       call pd_mag%write_inch( cmag, 0.75D0, pos, 0.15D0, 0.1D0, 90.0D0 )
       imag = imag + 1
    end do

  end subroutine draw_page


  !/ =====================================================================================
  subroutine load_stars( sec, stars )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t),    intent(inout) :: sec
    type(Star_t), allocatable, intent(inout) :: stars(:)
    !/ -----------------------------------------------------------------------------------
    integer                   :: i, idx, fh, num_rec, recno, ierr, max_star
    real(sp)                  :: fBTmag, fVTmag
    real(dp)                  :: ra, dec, ref_ra, ref_dec, half_fov
    real(dp)                  :: dist, bear, bt, vt, v, bmv
    character(:), allocatable :: star_file
    type(running_stats)       :: RS_v, RS_bmv, RS_out, RS_in
    !/ -----------------------------------------------------------------------------------

    star_file = sec%getString( 'sfile' )
    ref_ra      = sec%getReal8(  'ra' )
    ref_dec     = sec%getReal8(  'dec' )
    half_fov    = sec%getReal8(  'fov' ) * DEG2RAD * D_HALF
    max_star  = sec%getInt32(  'max' )

    if ( allocated( stars ) ) deallocate( stars )
    allocate( stars( max_star ) )

    !/ -----------------------------------------------------------------------------------

    open( NEWUNIT=fh, FILE=star_file, FORM='unformatted', &
         &            ACTION='READ', IOSTAT=ierr, ACCESS='stream' )
    read(fh) num_rec
    
     idx = 1
    do i=1,num_rec
       read(fh) recno, fBTmag, fVTmag, ra, dec

       dist = greatCircle( ref_ra, ref_dec, ra, dec )
          bt = real(fBTmag,dp)
          vt = real(fVTmag,dp)
          V   = vt - 0.090d0 * ( bt - vt )

          call RS_out%sample( dist )
       if (( dist.lt.half_fov ).and.(V.lt.4.0D0)) then
          bear = positionAngle( ref_ra, ref_dec, ra, dec )
          bmv = 0.890D0 * ( bt - vt )
          call RS_in%sample( dist )
          call RS_v%sample( V )
          call RS_bmv%sample( bmv )
          stars(idx)%radius  = dist
          stars(idx)%bearing = bear
          stars(idx)%V       = V
          stars(idx)%CI      = bmv
          idx = idx + 1
          if ( idx.gt.max_star ) then
             write( ERROR_UNIT, 100 ) max_star
             exit
          end if
       end if
    end do

    call sec%set( 'numstar', toString( idx-1 ) )

    close( fh )

    print *, ''
    print *, 'All Star Distance from Ref'
    call RS_out%report( UNIT=OUTPUT_UNIT )

    print *, ''
    print *, 'Selected dist from Ref'
    call RS_in%report( UNIT=OUTPUT_UNIT )

    !print *, ''
    !print *, 'V mag'
    !call RS_v%report( UNIT=OUTPUT_UNIT )

    !print *, ''
    !print *, 'B-V mag'
    !call RS_bmv%report( UNIT=OUTPUT_UNIT )

    call sec%set( 'minv',  toString( RS_v%minv() ) )
    call sec%set( 'maxv',  toString( RS_v%maxv() ) )
    call sec%set( 'minci', toString( RS_bmv%minv() ) )
    call sec%set( 'maxci', toString( RS_bmv%maxv() ) )

100 format( 'Increase max= to someting greater than ',I0 )

  end subroutine load_stars


  !/ =====================================================================================
  subroutine populate_stars( sec, stars, pd_image, pd_mag, pd_data )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: sec
    type(Star_t),           intent(inout) :: stars(:)
    type(PSDraw),           intent(inout) :: pd_image
    type(PSDraw),           intent(inout) :: pd_mag
    type(PSDraw),           intent(inout) :: pd_data
    !/ -----------------------------------------------------------------------------------
    real(dp) :: ref_ra, ref_dec, t_fov, minv, maxv, minci, maxci
    real(dp) :: dist, bear, V, CI, rad, sx, sy
    real(dp) :: minsx, minsy, maxsx, maxsy, minr, maxr
    integer  :: i, nstar
    !/ -----------------------------------------------------------------------------------

    ref_ra  = sec%getReal8( 'ra' )
    ref_dec = sec%getReal8( 'dec' )
    t_fov = sec%getReal8( 'fov' ) * DEG2RAD

    minv  = sec%getReal8( 'minv' )
    maxv  = sec%getReal8( 'maxv' )

    minci = sec%getReal8( 'minci' )
    maxci = sec%getReal8( 'maxci' )

    nstar = sec%getInt32( 'numstar' )

    !/ -----------------------------------------------------------------------------------

    print *, 'Number of records ', nstar
    print *, 'Field of view     ', t_fov
    print *, 'RA                ', ref_ra
    print *, 'DEC               ', ref_dec
    print *, 'Min V             ', minv
    print *, 'Max V             ', maxv
    print *, 'Min CI            ', minci
    print *, 'Max CI            ', maxci

    !/ -----------------------------------------------------------------------------------

    minsx =  1.0d10
    minsy =  1.0d10
    minr  =  1.0d10
    maxsx = -1.0d10
    maxsy = -1.0d10
    maxr  = -1.0d10

    do i=1,nstar
       
       dist = stars(i)%radius
       bear = stars(i)%bearing
       V    = stars(i)%V
       CI   = stars(i)%CI

       rad = magRadius( minv, maxv, V )

       call scale( sx, sy, t_fov, dist, bear )
 
       call drawStar( pd_image, D_ZERO, D_ZERO, D_ZERO, sx, sy, rad )

       !write(*,100) sx, sy, rad

       if ( sx.lt.minsx ) minsx = sx
       if ( sy.lt.minsy ) minsy = sy
       if ( rad.lt.minr ) minr  = rad
       if ( sx.gt.maxsx ) maxsx = sx
       if ( sy.gt.maxsy ) maxsy = sy
       if ( rad.gt.maxr ) maxr  = rad
    end do

100 format( F6.3,1X,F6.3,3X,F6.3 )

    print *, 'X', minsx, maxsx
    print *, 'Y', minsy, maxsy
    print *, 'R', minr, maxr

    !call pd_image%drawRectangle( minsx, minsy, maxsx, maxsy )

  end subroutine populate_stars

  
  !/ =====================================================================================
  subroutine process( sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t),    intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    type(Star_t),   allocatable :: stars(:)
    class(PSGraph), pointer     :: ps
    class(PSDraw),  pointer     :: pd_title
    class(PSDraw),  pointer     :: pd_image
    class(PSDraw),  pointer     :: pd_mag
    class(PSDraw),  pointer     :: pd_data

    call load_stars( sec, stars )

    pd_title => PSDraw( 0.5D0, 6.0D0, D_ZERO, D_ZERO, D_ONE, D_ONE )
    pd_image => PSDraw( 7.0D0, 7.0D0, D_ZERO, D_ZERO, 7.0D0, 7.0D0 )
    pd_mag   => PSDraw( 1.0D0, 4.0D0, 0.0D0, 0.0D0, 1.0D0, 4.0D0 )
    pd_data  => PSDraw( 1.5D0, 7.0D0, D_ZERO, D_ZERO, D_ONE, D_ONE )

    call draw_page( sec, pd_title, pd_image, pd_mag, pd_data )

    call populate_stars( sec, stars, pd_image, pd_mag, pd_data )

    ps => PSGraph( 1 )

    call ps%add( pd_title, 1, 0.5D0, 1.5D0 )
    call ps%add( pd_image, 1, 1.0D0, 1.0D0 )
    call ps%add( pd_mag,   1, 8.0D0, 2.25D0 )
    call ps%add( pd_data,  1, 9.0D0, 1.0D0 )

    call ps%pswrite( 'star.ps' )

    deallocate( stars )

    !call testFunctions

  end subroutine process

end module VScope




!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use VScope
  implicit none
  !/ -------------------------------------------------------------------------------------
  type(cli_map_t)                 :: cli
  type(configdb_t)                :: cfg
  integer                         :: ierr
  type(config_section_t), pointer :: sec
  !/ -------------------------------------------------------------------------------------

  call tlogger_set( CONSOLE=tlogger_info )

  call cli%add( 'stars', 'APP', 'sfile', .true., '',    'path to the star database' )
  call cli%add( 'ra',    'APP', 'ra',    .true., '',    'right ascension (degree)' )
  call cli%add( 'dec',   'APP', 'dec',   .true., '',    'declination     (degree)' )
  call cli%add( 'fov',   'APP', 'fov',   .true., '',    'field of view   (degree)' )
  call cli%add( 'max',   'APP', 'max',   .true., '500', 'maximum number of stars to plot' )

  call AppOptions%init( cli )
  call AppOptions%setHelp( 'help' )
  call AppOptions%setOptConfigFilename( 'cfg' )
  call AppOptions%setTitleLine( 'Virtual Telescope * v1.0' )
  call AppOptions%setExampleLine( 'stars=../data/Astro/StarData/Tycho2/Tycho2-telescope.bin' // &
       & ' ra=84.05333 dec=-1.20191667 fov=45.0' )
  call AppOptions%addUsageText( 'Generate synthetic telecope photograpy' )

  ierr = 0
  call AppOptions%getConfigDB( cfg, STATUS=ierr )

  if ( 0 .eq. ierr ) then
     sec  => cfg%get( 'APP', STATUS=ierr )
     if ( 0.eq.ierr ) then
        call process( sec )
     else
        call log_error( 'Section APP not found' )
     end if
  else
     call log_error( 'Configuration failed' )
  end if

end program main


!/ =======================================================================================
!/ **                                    V S C O P E                                    **
!/ =========================================================================== END FILE ==
