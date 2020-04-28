!/ ====================================================================== BEGIN FILE =====
!/ **                            T I M E _ T O O L S _ M O D                            **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module time_tools_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-01-27
  !! license: GPL
  !!
  !!## Date and Time Tools.
  !!
  !! Collection of procedures for manipulating date and time.
  !! Inspired by fundamentals of astrodynamics and applications.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none

  character(3), private, parameter :: MonthTitle(12) = [ 'Jan', 'Feb', 'Mar', 'Apr',  &
       &                                        'May', 'Jun', 'Jul', 'Aug',  &
       &                                        'Sep', 'Oct', 'Nov', 'Dec' ]

  character(3), private, parameter :: DayTitle(7) = [ 'Sun', 'Mon', 'Tue', 'Wed', &
       &                                     'Thr', 'Fri', 'Sat' ]

  integer, private, parameter :: LMonth(12) = [ 31, 28, 31, 30, 31, 30,  &
       &                               31, 31, 30, 31, 30, 31 ]


  integer, private, parameter :: MaxDay(12,2) = reshape( [ &
       &                   31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31,   &
       &                   31, 29, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31 ], [12,2] )

  integer, private, parameter :: AccumDay(12,2) = reshape( [ &
       &                   0, 31, 59, 90, 120, 151, 181, 212, 243, 273, 304, 334, &
       &                   0, 31, 60, 91, 121, 152, 182, 213, 244, 274, 305, 335 ], [12,2] )

  real(dp), parameter :: JD_0001       = 1.721423500d6  ! Saturday(7)
  real(dp), parameter :: JD_1700       = 2.341972500d6  ! Friday(6)
  real(dp), parameter :: JD_1800       = 2.378496500d6  ! Wednesday(4)
  real(dp), parameter :: JD_1900       = 2.415020500d6  ! Monday(2)
  real(dp), parameter :: JD_UNIX_EPOCH = 2.440587500d6  ! Thursday(5)
  real(dp), parameter :: JD_J2000      = 2.451544500d6  ! Saturday(7)
  real(dp), parameter :: JD_2100       = 2.488069500d6  ! Friday(6)
  real(dp), parameter :: JD_MJD        = 2.400000500d6  ! Wednesday(4) 1858-Nov-17 00:00:00

  real(dp), parameter :: JD_GREG1      = 2.299160500d6  ! Friday 1582-OCT-15




  !/ =====================================================================================
  type, public :: Epoch
     !/ ----------------------------------------------------------------------------------
     real(dp) :: JD1 = D_ZERO !! Julian Date first  part
     real(dp) :: JD2 = D_ZERO !! Julian Date second part

   contains

     procedure :: makeJD       => epc_make_jd
     procedure :: makeJ2000    => epc_make_j2000
     procedure :: makeMJD      => epc_make_mjd
     procedure :: makeDateTime => epc_make_date_time

     procedure :: add          => epc_add_integer_time
     procedure :: addf         => epc_add_double_time

     procedure :: toCal        => epc_to_calendar
     procedure :: fromCal      => epc_from_calendar

     procedure :: difference   => epc_get_difference

  end type Epoch




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  !/ =====================================================================================
  function dateString( Y, M, D, month_name ) result( dstr )
    !/ -----------------------------------------------------------------------------------
    !! Create an ISO 8601 compliant string containing the date portion.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,           intent(in) :: Y
    integer,           intent(in) :: M
    integer,           intent(in) :: D
    logical, optional, intent(in) :: month_name
    character(:), allocatable     :: dstr
    !/ -----------------------------------------------------------------------------------
    character(16) :: buffer
    !/ -----------------------------------------------------------------------------------

    if ( present( month_name ) ) then
       if ( month_name ) then
          write( buffer, 100 ) Y, MonthTitle(M), D
          goto 999
       end if
    end if

    write( buffer, 200 ) Y, M, D

100 format( I4,'-',A3,'-',I2 )
200 format( I4,'-',I2,'-',I2 )

999 continue
    dstr = trim(adjustl(buffer))

  end function dateString


  !/ =====================================================================================
  function timeString( H, M, S ) result( tstr )
    !/ -----------------------------------------------------------------------------------
    !! Create an ISO 8601 compliant string containing the time portion.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,           intent(in) :: H  !! Hour number
    integer,           intent(in) :: M  !! Minute number
    real(dp),          intent(in) :: S  !! Decimal Seconds
    character(:), allocatable     :: tstr
    !/ -----------------------------------------------------------------------------------
    character(16) :: buffer
    !/ -----------------------------------------------------------------------------------

    write( buffer, 100 ) H, M, S

    tstr = trim(adjustl(buffer))

100 format( I4.4,':',I2.2,':',F6.3 )

  end function timeString










  !/ =====================================================================================
  subroutine epc_make_jd( dts, JD )
    !/ -----------------------------------------------------------------------------------
    !! Convert this epoch internals to Julian date method.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts !! reference to this Epoch.
    real(dp), optional, intent(in)    :: JD  !! used to set  this Epoch.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp
    !/ -----------------------------------------------------------------------------------
    if ( present( JD ) ) then
       temp = JD
    else
       temp = dts%JD1 + dts%JD2
    end if

    dts%JD1 = temp
    dts%JD2 = D_ZERO
  end subroutine epc_make_jd


  !/ =====================================================================================
  subroutine epc_make_j2000( dts, JD )
    !/ -----------------------------------------------------------------------------------
    !! Convert this epoch internals to J2000 method.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts !! reference to this Epoch.
    real(dp), optional, intent(in)    :: JD  !! used to set  this Epoch.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp
    !/ -----------------------------------------------------------------------------------
    if ( present( JD ) ) then
       temp = JD
    else
       temp = dts%JD1 + dts%JD2
    end if

    dts%JD1 = JD_J2000
    dts%JD2 = temp - dts%JD1
  end subroutine epc_make_j2000


  !/ =====================================================================================
  subroutine epc_make_mjd( dts, JD )
    !/ -----------------------------------------------------------------------------------
    !! Convert this epoch internals to Modified Julian date method.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts !! reference to this Epoch.
    real(dp), optional, intent(in)    :: JD  !! used to set  this Epoch.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp
    !/ -----------------------------------------------------------------------------------
    if ( present( JD ) ) then
       temp = JD
    else
       temp = dts%JD1 + dts%JD2
    end if

    dts%JD1 = JD_MJD
    dts%JD2 = temp - dts%JD1
  end subroutine epc_make_mjd


  !/ =====================================================================================
  subroutine epc_make_date_time( dts, JD )
    !/ -----------------------------------------------------------------------------------
    !! Convert this epoch internals to date and time method.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts !! reference to this Epoch.
    real(dp), optional, intent(in)    :: JD  !! used to set  this Epoch.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp
    integer  :: ip
    !/ -----------------------------------------------------------------------------------
    if ( present( JD ) ) then
       temp = JD
    else
       temp = dts%JD1 + dts%JD2
    end if

    ip = floor( temp )

    dts%JD1 = real(ip,dp) + D_HALF
    dts%JD2 = temp - dts%JD1
  end subroutine epc_make_date_time



  !/ =====================================================================================
  function isLeapYear( year ) result( lp )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: year
    logical             :: lp
    !/ -----------------------------------------------------------------------------------
    lp = .false.
    if ( 0.eq.modulo( year,   4 ) ) lp = .true.

    ! These are the post Gregorian corrections
    if ( 1582.lt.year ) then
       if ( 0.eq.modulo( year, 100 ) ) lp = .false.
       if ( 0.eq.modulo( year, 400 ) ) lp = .true.
    end if
  end function isLeapYear


  !/ =====================================================================================
  function isGregorian( Y, M, D ) result( greg )
    !/ -----------------------------------------------------------------------------------
    !! Determine if a year is in the Gregorian or Julian calendar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: Y    !! year number
    integer, intent(in) :: M    !! month in th year
    integer, intent(in) :: D    !! day in the month
    logical             :: greg !! .true. if Gregorian, .false. if Julian
    !/ -----------------------------------------------------------------------------------
    greg = .true.
    if ( Y.lt.1582 ) goto 888
    if ( Y.gt.1582 ) goto 999
    if ( M.lt.10 )   goto 888
    if ( M.gt.10 )   goto 999
    if ( D.lt.5 )    goto 888
    if ( D.gt.14 )   goto 999

    call log_error( 'that week is in limbo', STR=dateString(Y,M,D) )
    goto 999

888 continue
    greg = .false.
999 continue
  end function isGregorian



  !/ =====================================================================================
  subroutine epc_from_calendar( dts, year, MONTH, DAY, DAYF, IERR )
    !/ -----------------------------------------------------------------------------------
    !! Set this epoch from a calendar date.
    !! Page 61, Astronomical Algorithms, Jean Meeus.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts   !! reference to this Epoch.
    integer,            intent(in)    :: year  !! year number
    integer,  optional, intent(in)    :: MONTH !! month number (1-12)
    integer,  optional, intent(in)    :: DAY   !! day of the month (1-31)
    real(dp), optional, intent(in)    :: DAYF  !! day fraction
    integer,  optional, intent(out)   :: IERR  !! error return 0=ok, -1=unacceptable date
    !/ -----------------------------------------------------------------------------------
    integer  :: er, Y, M, D, A, B, p1, p2
    real(dp) :: temp
    logical  :: isGreg
    !/ -----------------------------------------------------------------------------------

    er = 0
    Y  = year
    M  = 1
    D  = 1

    if ( present( MONTH ) )  M = MONTH
    if ( present( DAY ) )    D = DAY

    if ( .not.CheckDate( Y, M, D ) ) then
       call log_error( 'Invalid Date', STR=dateString(Y,M,D) )
       er = -1
       goto 999
    end if

    isGreg = isGregorian( Y, M, D )

    if ( M.lt.3 ) then
       Y = Y -  1
       M = M + 12
    end if

    if ( isGreg ) then
       A =         floor( real(Y,dp) * 1.0d-2 )
       B = 2 - A + floor( real(A,dp) * 2.5d-1 )
    else
       A = 0
       B = 0
    end if

    p1   = floor( 3.65250D2 * real(Y+4716, dp) )
    p2   = floor( 3.06001D1 * real(M+1,    dp) )

    temp = real(p1 + p2 + D + B, dp) - 1.5245D3

    dts%JD1 =        JD_MJD
    dts%JD2 = temp - JD_MJD

    if ( present( DAYF ) ) dts%JD2 = dts%JD2 + DAYF

999 continue
    if ( present( IERR ) ) IERR = er

    !print *, A, B, p1, p2, D, dts%JD1 + dts%JD2

  end subroutine epc_from_calendar


  !/ =====================================================================================
  subroutine epc_to_calendar( dts, year, MONTH, DAY, DAYF, IERR )
    !/ -----------------------------------------------------------------------------------
    !! Convert this epoch into calendar date.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts   !! reference to this Epoch.
    integer,            intent(out)   :: year  !! year number
    integer,  optional, intent(out)   :: MONTH !! month number (1-12)
    integer,  optional, intent(out)   :: DAY   !! day of the month (1-31)
    real(dp), optional, intent(out)   :: DAYF  !! day fraction
    integer,  optional, intent(out)   :: IERR  !! error return 0=ok, -1=unacceptable date
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp, F
    integer  :: Z, A, alpha, B, C, D, E, M
    !/ -----------------------------------------------------------------------------------

    temp = dts%JD1 + dts%JD2 + D_HALF

    Z = floor(temp)
    F = temp - real(Z,dp)

    if ( z.lt.2299161 ) then
       A = Z
    else
       alpha = floor((real(Z,dp) - 1.86721625d6) / 3.652425d4)
       A = Z + 1 + alpha - floor( real(alpha,dp) * 2.5d-1 )
    end if

    B = A + 1524

    C = floor( (real(B,dp) - 1.221d2) / 3.6525d2)

    D = floor( 3.6525d2 * real(C,dp) )

    E = floor( real( B - D, dp ) / 3.06001d1 )

    temp = real( B - D - floor(3.06001d1 * real(E,dp)), dp ) + F

    if ( E.lt.14 ) then
       M = E - 1
    else
       M = E - 13
    end if

    if ( M.gt.2 ) then
       year = C - 4716
    else
       year = C - 4715
    end if

    if ( present( MONTH ) ) MONTH = M
    if ( present( DAY ) )   DAY   = floor( temp )
    if ( present( DAYF ) )  DAYF  = temp - real(floor( temp ), dp)

    if ( present( IERR ) ) IERR = 0

  end subroutine epc_to_calendar


  !/ =====================================================================================
  subroutine epc_add_integer_time( dts, DAYS, HOURS, MINUTES, SECONDS )
    !/ -----------------------------------------------------------------------------------
    !! Add time to this increment, any combination may be used.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts     !! reference to this Epoch.
    integer,  optional, intent(in)    :: DAYS    !! additional whole days
    integer,  optional, intent(in)    :: HOURS   !! additional whole hours
    integer,  optional, intent(in)    :: MINUTES !! additional whole minutes
    real(dp), optional, intent(in)    :: SECONDS !! additional real seconds
    !/ -----------------------------------------------------------------------------------

    if ( present( DAYS ) )    dts%JD2 = dts%JD2 +   real( DAYS, dp )
    if ( present( HOURS ) )   dts%JD2 = dts%JD2 + ( real( HOURS, dp )   / 2.4d1 )
    if ( present( MINUTES ) ) dts%JD2 = dts%JD2 + ( real( MINUTES, dp ) / 1.440d3 )
    if ( present( SECONDS ) ) dts%JD2 = dts%JD2 + (       SECONDS       / 8.640d4 )

  end subroutine epc_add_integer_time


  !/ =====================================================================================
  subroutine epc_add_double_time( dts, DAYS, HOURS, MINUTES, SECONDS )
    !/ -----------------------------------------------------------------------------------
    !! Add time to this increment, any combination may be used.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts     !! reference to this Epoch.
    real(dp), optional, intent(in)    :: DAYS    !! additional real days
    real(dp), optional, intent(in)    :: HOURS   !! additional real hours
    real(dp), optional, intent(in)    :: MINUTES !! additional real minutes
    real(dp), optional, intent(in)    :: SECONDS !! additional real seconds
    !/ -----------------------------------------------------------------------------------

    if ( present( DAYS ) )    dts%JD2 = dts%JD2 +   DAYS
    if ( present( HOURS ) )   dts%JD2 = dts%JD2 + ( HOURS   / 2.4d1 )
    if ( present( MINUTES ) ) dts%JD2 = dts%JD2 + ( MINUTES / 1.440d3 )
    if ( present( SECONDS ) ) dts%JD2 = dts%JD2 + ( SECONDS / 8.640d4 )

  end subroutine epc_add_double_time


  !/ =====================================================================================
  subroutine epc_get_difference( dts, epc, DAYS, HOURS, MINUTES, SECONDS )
    !/ -----------------------------------------------------------------------------------
    !! Find the difference between this epoch and another    diff = this - epc
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Epoch),       intent(inout) :: dts     !! reference to this Epoch.
    type(Epoch),        intent(in)    :: epc     !! reference to this Epoch.
    real(dp), optional, intent(out)   :: DAYS    !! additional real days
    real(dp), optional, intent(out)   :: HOURS   !! additional real hours
    real(dp), optional, intent(out)   :: MINUTES !! additional real minutes
    real(dp), optional, intent(out)   :: SECONDS !! additional real seconds
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff
    !/ -----------------------------------------------------------------------------------

    diff = dts%JD1 + dts%JD2 - epc%JD1 - epc%JD2

    if ( present( DAYS ) )    DAYS    = diff
    if ( present( HOURS ) )   HOURS   = diff * 2.4d1
    if ( present( MINUTES ) ) MINUTES = diff * 1.440d3
    if ( present( SECONDS ) ) SECONDS = diff * 8.640d4

  end subroutine epc_get_difference






  !/ =====================================================================================
  function HMS2DAY( H, M, S ) result( day )
    !/ -----------------------------------------------------------------------------------
    !! Convert hours, minutes, and seconds into days.
    !/ -----------------------------------------------------------------------------------
    real(dp), intent(in) :: H   !! hours
    real(dp), intent(in) :: M   !! minutes
    real(dp), intent(in) :: S   !! seconds
    real(dp)             :: day !! degrees
    !/ -----------------------------------------------------------------------------------

    day = (H + (M + S/6.0d1)/6.0d1)/2.4d1

    !    write(*,100) H, M, S, day
    !   100 format('( ',F5.1,'h ',G0,'m ',G0,'s ) ==> ',F10.6,' days')

  end function HMS2DAY


  !/ =====================================================================================
  function HMS2DEG( H, M, S ) result( deg )
    !/ -----------------------------------------------------------------------------------
    !! Convert hours, minutes, and seconds into degrees.
    !/ -----------------------------------------------------------------------------------
    real(dp), intent(in) :: H   !! hours
    real(dp), intent(in) :: M   !! minutes
    real(dp), intent(in) :: S   !! seconds
    real(dp)             :: deg !! degrees
    !/ -----------------------------------------------------------------------------------

    deg = ((9.0d2 * H) + M + (S/6.0d1))/6.0d1

    !write(*,100) H, M, S, deg
    !100 format('( ',F5.1,'h ',F4.1,'m ',F6.3,'s ) ==> ',F10.6,' degrees')

  end function HMS2DEG


  !/ =====================================================================================
  function DMS2DEG( D, M, S ) result( deg )
    !/ -----------------------------------------------------------------------------------
    !! Convert degrees, minutes, and seconds into degrees.
    !/ -----------------------------------------------------------------------------------
    real(dp), intent(in) :: D   !! degrees
    real(dp), intent(in) :: M   !! minutes
    real(dp), intent(in) :: S   !! seconds
    real(dp)             :: deg !! degrees
    !/ -----------------------------------------------------------------------------------

    deg = D + (M + (S/6.0d1))/6.0d1

    !write(*,100) D, M, S, deg
    !100 format('( ',F5.1,'d ',F4.1,'m ',F6.3,'s ) ==> ',F10.6,' degrees')

  end function DMS2DEG




  !/ =====================================================================================
  subroutine DAY2HMS( H, M, S, day )
    !/ -----------------------------------------------------------------------------------
    !! Convert days into hours, minutes, and seconds.
    !/ -----------------------------------------------------------------------------------
    real(dp), intent(out) :: H   !! hours
    real(dp), intent(out) :: M   !! minutes
    real(dp), intent(out) :: S   !! seconds
    real(dp), intent(in)  :: day !! days
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp1, temp2
    !/ -----------------------------------------------------------------------------------

    call fraction( H, temp1, day,  MULTIPLY=2.4d1 )
    call fraction( M, temp2, temp1, MULTIPLY=6.0d1 )
    temp1 = temp2 * 6.0d1


    if ( temp1.lt.6.0d1 ) then
       S = temp1
    else
       S = temp1 - 6.0d1
       M = M + 1.0d0
    end if


    !    write(*,100) H, M, S, day
    !    100 format('( ',F5.1,'h ',G0,'m ',G0,'s ) <== ',F10.6,' days')


  end subroutine DAY2HMS


  !/ =====================================================================================
  subroutine DEG2HMS( H, M, S, deg )
    !/ -----------------------------------------------------------------------------------
    !! Convert degrees into hours, minutes, and seconds.
    !/ -----------------------------------------------------------------------------------
    real(dp), intent(out) :: H   !! hours
    real(dp), intent(out) :: M   !! minutes
    real(dp), intent(out) :: S   !! seconds
    real(dp), intent(in)  :: deg !! degrees
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp1, temp2
    !/ -----------------------------------------------------------------------------------

    call fraction( H, temp1, deg, DIVIDE=1.5d1 )
    call fraction( M, temp2, temp1, MULTIPLY=9.0d2 )
    S = temp2 * 6.0d1

    !write(*,100) H, M, S, deg
    !100 format('( ',F5.1,'h ',F4.1,'m ',F6.3,'s ) ==> ',F10.6,' degrees')

  end subroutine DEG2HMS


  !/ =====================================================================================
  subroutine DEG2DMS( D, M, S, deg )
    !/ -----------------------------------------------------------------------------------
    !! Convert degrees into degrees, minutes, and seconds.
    !/ -----------------------------------------------------------------------------------
    real(dp), intent(out) :: D   !! degrees
    real(dp), intent(out) :: M   !! minutes
    real(dp), intent(out) :: S   !! seconds
    real(dp), intent(in)  :: deg !! degrees
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp1, temp2
    !/ -----------------------------------------------------------------------------------

    call fraction( D, temp1, deg )
    call fraction( M, temp2, temp1, MULTIPLY=6.0d1 )
    S = temp2 * 6.0d1

    !write(*,100) D, M, S, deg
    !100 format('( ',F5.1,'d ',F4.1,'m ',F6.3,'s ) ==> ',F10.6,' degrees')

  end subroutine DEG2DMS








  !/ =====================================================================================
  function GetMonthNumber( MonStr ) result( MonNum )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(3), intent(in) :: MonStr
    integer                  :: MonNum
    !/ -----------------------------------------------------------------------------------
    MonNum = 0

    if ('Jan'.eq.MonStr) MonNum = 1
    if ('jan'.eq.MonStr) MonNum = 1
    if ('JAN'.eq.MonStr) MonNum = 1

    if ('Feb'.eq.MonStr) MonNum = 2
    if ('feb'.eq.MonStr) MonNum = 2
    if ('FEB'.eq.MonStr) MonNum = 2

    if ('Mar'.eq.MonStr) MonNum = 3
    if ('mar'.eq.MonStr) MonNum = 3
    if ('MAR'.eq.MonStr) MonNum = 3

    if ('Apr'.eq.MonStr) MonNum = 4
    if ('apr'.eq.MonStr) MonNum = 4
    if ('APR'.eq.MonStr) MonNum = 4

    if ('May'.eq.MonStr) MonNum = 5
    if ('may'.eq.MonStr) MonNum = 5
    if ('MAY'.eq.MonStr) MonNum = 5

    if ('Jun'.eq.MonStr) MonNum = 6
    if ('jun'.eq.MonStr) MonNum = 6
    if ('JUN'.eq.MonStr) MonNum = 6

    if ('Jul'.eq.MonStr) MonNum = 7
    if ('jul'.eq.MonStr) MonNum = 7
    if ('JUL'.eq.MonStr) MonNum = 7

    if ('Aug'.eq.MonStr) MonNum = 8
    if ('aug'.eq.MonStr) MonNum = 8
    if ('AUG'.eq.MonStr) MonNum = 8

    if ('Sep'.eq.MonStr) MonNum = 9
    if ('sep'.eq.MonStr) MonNum = 9
    if ('SEP'.eq.MonStr) MonNum = 9

    if ('Oct'.eq.MonStr) MonNum = 10
    if ('oct'.eq.MonStr) MonNum = 10
    if ('OCT'.eq.MonStr) MonNum = 10

    if ('Nov'.eq.MonStr) MonNum = 11
    if ('nov'.eq.MonStr) MonNum = 11
    if ('NOV'.eq.MonStr) MonNum = 11

    if ('Dec'.eq.MonStr) MonNum = 12
    if ('dec'.eq.MonStr) MonNum = 12
    if ('DEC'.eq.MonStr) MonNum = 12
  end function GetMonthNumber


  !/ =====================================================================================
  function GetDayNumber( DayStr ) result( DayNum )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(3), intent(in) :: DayStr
    integer                  :: DayNum
    !/ -----------------------------------------------------------------------------------
    DayNum = 0

    if ('Sun'.eq.DayStr) DayNum = 1
    if ('sun'.eq.DayStr) DayNum = 1
    if ('SUN'.eq.DayStr) DayNum = 1

    if ('Mon'.eq.DayStr) DayNum = 2
    if ('mon'.eq.DayStr) DayNum = 2
    if ('MON'.eq.DayStr) DayNum = 2

    if ('Tue'.eq.DayStr) DayNum = 3
    if ('tue'.eq.DayStr) DayNum = 3
    if ('TUE'.eq.DayStr) DayNum = 3

    if ('Wed'.eq.DayStr) DayNum = 4
    if ('wed'.eq.DayStr) DayNum = 4
    if ('WED'.eq.DayStr) DayNum = 4

    if ('Thr'.eq.DayStr) DayNum = 5
    if ('thu'.eq.DayStr) DayNum = 5
    if ('THU'.eq.DayStr) DayNum = 5

    if ('Fri'.eq.DayStr) DayNum = 6
    if ('fri'.eq.DayStr) DayNum = 6
    if ('FRI'.eq.DayStr) DayNum = 6

    if ('Sat'.eq.DayStr) DayNum = 7
    if ('sat'.eq.DayStr) DayNum = 7
    if ('SAT'.eq.DayStr) DayNum = 7
  end function GetDayNumber


  !/ =====================================================================================
  pure function DayOfWeek( JD ) result( dow )
    !/ -----------------------------------------------------------------------------------
    !! Compute the day of the week from a Julian date. This function finds the day of
    !! the week. Integers are used for the days, 1 = 'SUN', 2 = 'Mon', ... 7 = 'Sat'.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: JD
    integer              :: dow
    !/ -----------------------------------------------------------------------------------
    dow = modulo( floor(JD + 5.00001d-1) - 1721425, 7 ) + 1
  end function DayOfWeek


  !/ =====================================================================================
  function DayOfYear( year, mon, day ) result( doy )
    !/ -----------------------------------------------------------------------------------
    !! Compute the day of the year from a calandar date.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: year
    integer, intent(in) :: mon
    integer, intent(in) :: day
    integer             :: doy
    !/ -----------------------------------------------------------------------------------
    integer :: ly
    !/ -----------------------------------------------------------------------------------
    if ( isLeapYear( year ) ) then
       ly = 2
    else
       ly = 1
    end if
    doy = AccumDay(mon,ly) + day
  end function DayOfYear


  !/ =====================================================================================
  function CheckTime( hour, minute, second ) result( v )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: hour   !! hour   number (0-23)
    integer,  intent(in) :: minute !! minute number (0-59)
    real(dp), intent(in) :: second !! seconds       (0-60.999....)
    logical              :: v      !! validity
    !/ -----------------------------------------------------------------------------------
    v = .true.

    if ( 0.gt.hour) goto 800
    if (23.lt.hour) goto 800

    if ( 0.gt.minute) goto 800
    if (59.lt.minute) goto 800

    if ( 0.0d0.gt.second) goto 800
    if ( 6.0d1.lt.second) goto 800


    goto 999
800 continue
    v = .false.
999 continue
  end function CheckTime



  !/ =====================================================================================
  function CheckDate( year, month, day ) result( v )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: year  !! year  number (almost any)
    integer, intent(in) :: month !! month number (1-12)
    integer, intent(in) :: day   !! day   number (1-31, depends on the month)
    logical             :: v     !! validity
    !/ -----------------------------------------------------------------------------------
    v = .true.
    if (  1.gt.day )   goto 800
    if (  1.gt.month ) goto 800
    if ( 12.lt.month ) goto 800

    if (     0.eq.year ) goto 800
    if ( -4713.gt.year ) goto 800

    if ( 2.eq.month ) then
       if ( isLeapYear( year ) ) then
          if ( 29.lt.day ) goto 800
       else
          if ( 28.lt.day ) goto 800
       end if
    else
       if ( day.gt.MaxDay(month,1) ) goto 800
    end if

    if ( 1582.eq.year ) then
       if ( 10.eq.month ) then
          if ( (4.lt.day).and.(15.gt.day) ) goto 800
       end if
    end if

    goto 999
800 continue
    v = .false.
999 continue
  end function CheckDate



  !/ =====================================================================================
  subroutine IncrementDate( year, month, day, INCR )
    !/ -----------------------------------------------------------------------------------
    !! Increment a yar, month, day triplet. Note: not optimized for large Increments.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,           intent(inout) :: year  !! year  number (any)
    integer,           intent(inout) :: month !! month number (1-12)
    integer,           intent(inout) :: day   !! day   number (1-31, depends on the month)
    integer, optional, intent(in)    :: INCR  !! day   increment (default: 1)
    !/ -----------------------------------------------------------------------------------
    integer i, n, mxday
    !/ -----------------------------------------------------------------------------------

    if ( .not.CheckDate( year, month, day ) ) then
       write(ERROR_UNIT,*) 'date (', year,month, day, ') is invalid'
       goto 200
    end if

    n = 1
    if ( present( INCR ) ) n= INCR
    do i=1,n

       if (( 1582.eq.year ).and.( 10.eq.month ).and.( 4.eq.day )) then
          day = 15
          goto 100
       end if

       if (( -1.eq.year ).and.( 12.eq.month ).and.( 31.eq.day )) then
          year  = 1
          month = 1
          day   = 1
          goto 100
       end if

       if ( isLeapYear( year ) ) then
          mxday = MaxDay( month, 2 )
       else
          mxday = MaxDay( month, 1 )
       end if
       day = day + 1
       if ( day.gt.mxday ) then
          day = 1
          month = month + 1
          if ( month.gt.12 ) then
             month = 1
             year = year + 1
          end if
       end if

100    continue

    end do

200 continue

  end subroutine IncrementDate


  !/ =====================================================================================
  subroutine DecrementDate( year, month, day, DECR )
    !/ -----------------------------------------------------------------------------------
    !! Decrement a yar, month, day triplet. Note: not optimized for large decrements.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,           intent(inout) :: year  !! year  number (any)
    integer,           intent(inout) :: month !! month number (1-12)
    integer,           intent(inout) :: day   !! day   number (1-31, depends on the month)
    integer, optional, intent(in)    :: DECR  !! day   decrement (default: 1)
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------
    if ( .not.CheckDate( year, month, day ) ) then
       write(ERROR_UNIT,*) 'date (', year,month, day, ') is invalid'
       goto 200
    end if

    n = 1
    if ( present( DECR ) ) n= DECR
    do i=1,n

       if (( 1582.eq.year ).and.( 10.eq.month ).and.( 15.eq.day )) then
          day = 4
          goto 100
       end if

       if (( 1.eq.year ).and.( 1.eq.month ).and.( 1.eq.day )) then
          year  = -1
          month =  12
          day   =  31
          goto 100
       end if

       day = day - 1
       if ( day.lt.1 ) then
          month = month - 1
          if ( month.lt.1 ) then
             year  = year - 1
             month = 12
          end if
          if ( isLeapYear( year ) ) then
             day = MaxDay( month, 2 )
          else
             day = MaxDay( month, 1 )
          end if
       end if
100    continue
    end do
200 continue
  end subroutine DecrementDate


end module time_tools_mod


!/ =======================================================================================
!/ **                            T I M E _ T O O L S _ M O D                            **
!/ =========================================================================== END FILE ==
