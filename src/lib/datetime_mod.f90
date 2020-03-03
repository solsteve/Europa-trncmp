!/ ====================================================================== BEGIN FILE =====
!/ **                          A B S O L U T E D A T E _ M O D                          **
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
module datetime_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-01-27
  !! license: GPL
  !!
  !!## Date and Time Procedures.
  !!
  !! Collection of procedures for manipulating date and time. Inspired by OreKit.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none


  !/ =====================================================================================
  type :: TimeComponents
     !/ ----------------------------------------------------------------------------------
     !/ Individual hour minute second components. Internal Representation is UTC
     !/ ----------------------------------------------------------------------------------

     integer,   private :: hour   = 0     !! Hour number.
     integer,   private :: minute = 0     !! Minute number.
     real(dp),  private :: second = 0.0d0 !! Second number.

   contains

     procedure, private :: tc_correct_parameters

     procedure :: clear           => tc_clear
     procedure :: set             => tc_set_components
     procedure :: get             => tc_get_components
     procedure :: toString        => tc_to_string

     procedure :: getHours        => tc_get_hours
     procedure :: getMinutes      => tc_get_minutes
     procedure :: getSeconds      => tc_get_seconds

     procedure :: getHoursInDay   => tc_get_hours_in_a_day
     procedure :: getMinutesInDay => tc_get_minutes_in_a_day
     procedure :: getSecondsInDay => tc_get_seconds_in_a_day

  end type TimeComponents


  !/ -------------------------------------------------------------------------------------
  interface toHours
     !/ ----------------------------------------------------------------------------------
     module procedure :: tc_HMS2hours
  end interface toHours


  !/ -------------------------------------------------------------------------------------
  interface toMinutes
     !/ ----------------------------------------------------------------------------------
     module procedure :: tc_HMS2minutes
  end interface toMinutes


  !/ -------------------------------------------------------------------------------------
  interface toSeconds
     !/ ----------------------------------------------------------------------------------
     module procedure :: tc_HMS2seconds
  end interface toSeconds




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine tc_correct_parameters( dts )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts !! reference to this TimeComponents object.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: hid, x
    !/ -----------------------------------------------------------------------------------

    ! extract the number of hours in the day
    hid = toHours( H=real(dts%hour,dp), M=real(dts%minute,dp), S=dts%second )

    ! MOD this by 24 hours
    hid = hid - ( 2.4d1 * real(floor( hid / 2.4d1 ), dp) )

    x = real(floor( hid ), dp)
    dts%hour = int(x)

    hid = (hid - x) * 6.0d1
    x = real(floor( hid ), dp)
    dts%minute = int(x)

    dts%second = (hid - x) * 6.0d1

  end subroutine tc_correct_parameters


  !/ =====================================================================================
  function tc_HMS2hours( H, M, S ) result( hr )
    !/ -----------------------------------------------------------------------------------
    !! Convert components to seconds.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), optional, intent(in) :: H  !! Hour number.
    real(dp), optional, intent(in) :: M  !! Minute number.
    real(dp), optional, intent(in) :: S  !! Second number.
    real(dp)                       :: hr !! return seconds.
    !/ -----------------------------------------------------------------------------------
    hr = D_ZERO
    if ( present( H ) ) hr = hr +   H
    if ( present( M ) ) hr = hr + ( M / 6.0d1 )
    if ( present( S ) ) hr = hr + ( S / 3.600d3 )
    hr = hr - ( 2.4d1 * real(floor( hr / 2.4d1 ), dp) )
  end function tc_HMS2hours


  !/ =====================================================================================
  function tc_HMS2minutes( H, M, S ) result( mn )
    !/ -----------------------------------------------------------------------------------
    !! Convert components to minutes.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), optional, intent(in) :: H   !! Hour number.
    real(dp), optional, intent(in) :: M   !! Minute number.
    real(dp), optional, intent(in) :: S   !! Second number.
    real(dp)                       :: mn  !! return minutes in a day.
    !/ -----------------------------------------------------------------------------------
    mn = D_ZERO
    if ( present( H ) ) mn = mn + ( H * 3.600d3 )
    if ( present( M ) ) mn = mn +   M
    if ( present( S ) ) mn = mn + ( S / 6.0d1 )
    mn = mn - ( 1.440d3 * real(floor( mn / 1.440d3 ), dp) )
  end function tc_HMS2minutes


  !/ =====================================================================================
  function tc_HMS2seconds( H, M, S ) result( sc )
    !/ -----------------------------------------------------------------------------------
    !! Convert components to seconds.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), optional, intent(in) :: H  !! Hour number.
    real(dp), optional, intent(in) :: M  !! Minute number.
    real(dp), optional, intent(in) :: S  !! Second number.
    real(dp)                       :: sc !! return seconds in a day.
    !/ -----------------------------------------------------------------------------------
    sc = D_ZERO
    if ( present( H ) ) sc = sc + ( H * 3.6d3 )
    if ( present( M ) ) sc = sc + ( M * 6.0d1 )
    if ( present( S ) ) sc = sc +   S
    sc = sc - ( 8.6400d4 * real(floor( sc / 8.6400d4 ), dp) )
  end function tc_HMS2seconds







  !/ =====================================================================================
  subroutine tc_clear( dts )
    !/ -----------------------------------------------------------------------------------
    !! Clear al of the fields in this TimeComponent
    !/ -----------------------------------------------------------------------------------
    class(TimeComponents), intent(inout) :: dts    !! reference to this TimeComponents object.
    !/ -----------------------------------------------------------------------------------
    dts%hour   = 0
    dts%minute = 0
    dts%second = D_ZERO
  end subroutine tc_clear


  !/ =====================================================================================
  subroutine tc_set_components( dts, H, M, S, OFFSET )
    !/ -----------------------------------------------------------------------------------
    !! Set the components.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts    !! reference to this TimeComponents object.
    integer,  optional,    intent(in)    :: H      !! Hour number.
    integer,  optional,    intent(in)    :: M      !! Minute number.
    real(dp), optional,    intent(in)    :: S      !! Second number.
    integer,  optional,    intent(in)    :: OFFSET !! Minutes from UTC.
    !/ -----------------------------------------------------------------------------------
    if ( present( H ) )      dts%hour   = H
    if ( present( M ) )      dts%minute = M
    if ( present( S ) )      dts%second = S
    if ( present( OFFSET ) ) dts%minute = dts%minute + OFFSET

    call dts%tc_correct_parameters

  end subroutine tc_set_components


  !/ =====================================================================================
  subroutine tc_add_components( dts, H, M, S, OFFSET )
    !/ -----------------------------------------------------------------------------------
    !! Add to the components.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts    !! reference to this TimeComponents object.
    integer,  optional,    intent(in)    :: H      !! increment to the Hour number.
    integer,  optional,    intent(in)    :: M      !! increment to the Minute number.
    real(dp), optional,    intent(in)    :: S      !! increment to the Second number.
    integer,  optional,    intent(in)    :: OFFSET !! increment to the Minutes from UTC.
    !/ -----------------------------------------------------------------------------------
    if ( present( H ) )      dts%hour   = dts%hour   + H
    if ( present( M ) )      dts%minute = dts%minute + M
    if ( present( S ) )      dts%second = dts%second + S
    if ( present( OFFSET ) ) dts%minute = dts%minute + OFFSET

    call dts%tc_correct_parameters

  end subroutine tc_add_components


  !/ =====================================================================================
  subroutine tc_get_components( dts, H, M, S )
    !/ -----------------------------------------------------------------------------------
    !! Get the components.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(in)  :: dts    !! reference to this TimeComponents object.
    integer,  optional,    intent(out) :: H      !! Hour number.
    integer,  optional,    intent(out) :: M      !! Minute number.
    real(dp), optional,    intent(out) :: S      !! Second number.
    !/ -----------------------------------------------------------------------------------
    if ( present( H ) ) H = dts%hour
    if ( present( M ) ) M = dts%minute
    if ( present( S ) ) S = dts%second
  end subroutine tc_get_components



  !/ =====================================================================================
  function tc_to_string( dts, OFFSET ) result( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts !! reference to this TimeComponents object.
    integer, optional,     intent(in)    :: OFFSET !! apply an optional offset
    character(:), allocatable            :: str
    !/ -----------------------------------------------------------------------------------
    character(24) :: buffer
    integer       :: off
    !/ -----------------------------------------------------------------------------------

    if ( present( OFFSET ) ) then
       off = OFFSET
    else
       off = 0
    end if

    if ( dts%second.lt.1.0d1 ) then
       if ( 0.gt.off ) then
          write( buffer, 100 ) dts%hour, dts%minute, dts%second, -off
       else if ( 0.lt.off ) then
          write( buffer, 110 ) dts%hour, dts%minute, dts%second,  off
       else
          write( buffer, 120 ) dts%hour, dts%minute, dts%second
       end if
    else
       if ( 0.gt.off ) then
          write( buffer, 105 ) dts%hour, dts%minute, dts%second, -off
       else if ( 0.lt.off ) then
          write( buffer, 115 ) dts%hour, dts%minute, dts%second,  off
       else
          write( buffer, 125 ) dts%hour, dts%minute, dts%second
       end if
    end if

    str = trim(adjustl(buffer))

100 format( I2.2,':',I2.2,':0',F5.3,'-',I4.4 )
105 format( I2.2,':',I2.2,':',F6.3,'-',I4.4 )
110 format( I2.2,':',I2.2,':0',F5.3,'+',I4.4 )
115 format( I2.2,':',I2.2,':',F6.3,'+',I4.4 )
120 format( I2.2,':',I2.2,':0',F5.3 )
125 format( I2.2,':',I2.2,':',F6.3 )

  end function tc_to_string


  !/ =====================================================================================
  function tc_get_hours( dts ) result( hr )
    !/ -----------------------------------------------------------------------------------
    !! Get the internal hour number.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(in) :: dts !! reference to this TimeComponents object.
    integer                           :: hr  !! hour number.
    !/ -----------------------------------------------------------------------------------
    hr = dts%hour
  end function tc_get_hours


  !/ =====================================================================================
  function tc_get_minutes( dts ) result( mn )
    !/ -----------------------------------------------------------------------------------
    !! Get the internal minute number.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(in) :: dts !! reference to this TimeComponents object.
    integer                           :: mn  !! minute number.
    !/ -----------------------------------------------------------------------------------
    mn = dts%minute
  end function tc_get_minutes


  !/ =====================================================================================
  function tc_get_seconds( dts ) result( sc )
    !/ -----------------------------------------------------------------------------------
    !! Get the internal seconds value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(in) :: dts !! reference to this TimeComponents object.
    real(dp)                          :: sc  !! seconds value.
    !/ -----------------------------------------------------------------------------------
    sc = dts%second
  end function tc_get_seconds






  !/ =====================================================================================
  function tc_get_hours_in_a_day( dts ) result( hr )
    !/ -----------------------------------------------------------------------------------
    !! Convert the internal representation into hours in a day. Mod(24)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts !! reference to this TimeComponents object.
    real(dp)                             :: hr  !! hours in a day.
    !/ -----------------------------------------------------------------------------------
    hr = toHours( H=real(dts%hour,dp), M=real(dts%minute,dp), S=dts%second )
  end function tc_get_hours_in_a_day

  
  !/ =====================================================================================
  function tc_get_minutes_in_a_day( dts ) result( mn )
    !/ -----------------------------------------------------------------------------------
    !! Convert the internal representation into minutes in a day. Mod(1440)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts !! reference to this TimeComponents object.
    real(dp)                             :: mn  !! minutes in a day.
    !/ -----------------------------------------------------------------------------------
    mn = toMinutes( H=real(dts%hour,dp), M=real(dts%minute,dp), S=dts%second )
  end function tc_get_minutes_in_a_day

  
  !/ =====================================================================================
  function tc_get_seconds_in_a_day( dts ) result( sc )
    !/ -----------------------------------------------------------------------------------
    !! Convert the internal representation into seconds in a day. Mod(86400)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TimeComponents), intent(inout) :: dts !! reference to this TimeComponents object.
    real(dp)                             :: sc  !! seconds in a day.
    !/ -----------------------------------------------------------------------------------
    sc = toSeconds( H=real(dts%hour,dp), M=real(dts%minute,dp), S=dts%second )
  end function tc_get_seconds_in_a_day


end module datetime_mod
