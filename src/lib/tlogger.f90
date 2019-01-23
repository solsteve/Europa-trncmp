!/ ====================================================================== BEGIN FILE =====
!/ **                                   T L O G G E R                                   **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
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
module tlogger
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2015-12-04
  !! license: GPL
  !!
  !!##TRAN-COMP Logging
  !!
  !! Provides console and file logging system with custom abort.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  public ::  tlogger_abort_handler, tlogger_ah_ptr, tlogger_set, tlogger_set_abort_handler, &
       &     tlogger_enable, tlogger_disable, tlogger_enable_file, tlogger_disable_file,    &
       &     tlogger_enable_console, tlogger_disable_console, log_critical, log_error,      &
       &     log_warn, log_info, log_debug, log_timestamp

  integer, public,  parameter :: tlogger_unset    = 0 !! level not set
  integer, public,  parameter :: tlogger_critical = 1 !! level set for only critical
  integer, public,  parameter :: tlogger_error    = 2 !! level set for error and above
  integer, public,  parameter :: tlogger_warn     = 3 !! level set for warning and above
  integer, public,  parameter :: tlogger_info     = 4 !! level set for info and above
  integer, public,  parameter :: tlogger_debug    = 5 !! level set for debug and above

  integer :: tlogger_abort_level = tlogger_critical
  !! set the default abort level to critical

  integer :: tlogger_file_level = tlogger_warn
  !! set the default file level to warning

  integer :: tlogger_console_level = tlogger_info
  !! set the default console level to info

  integer :: tlogger_err_unit = ERROR_UNIT
  !! assign the error unit to stderr

  character(len=256) :: tlogger_log_file_name = 'proxima.log'
  !! set the default log file name

  logical :: tlogging_enabled = .true.
  !! enable logging by default

  logical :: tlogging_console_enabled = .true.
  !! enable console logging by default

  logical :: tlogging_file_enabled = .false.
  !! disable file logging by default


  !/ -------------------------------------------------------------------------------------
  interface
     !/ ----------------------------------------------------------------------------------
     subroutine tlogger_abort_handler( level )
       integer, intent(in) :: level
     end subroutine tlogger_abort_handler
  end interface

  procedure(tlogger_abort_handler), pointer :: tlogger_ah_ptr => tlogger_default_abort_handler




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine tlogger_default_abort_handler( level )
    !/ -----------------------------------------------------------------------------------
    !! Provide a default message for application abort. 
    !/ -----------------------------------------------------------------------------------
    integer, intent(in) :: level !! exit code.
    !/ -----------------------------------------------------------------------------------

    write(ERROR_UNIT,*) 'Performing a default TLogger abort with exit code:', level
    stop

  end subroutine tlogger_default_abort_handler


  !/ =====================================================================================
  subroutine tlogger_set_abort_handler( proc, new_level )
    !/ -----------------------------------------------------------------------------------
    !! Register a new abort handler with the logging system. Set the new level for
    !! generating an abort.
    !/ -----------------------------------------------------------------------------------
    procedure(), pointer,  intent(in) :: proc      !! pointer to the abort handler class.
    integer,     optional, intent(in) :: new_level !! new minimum level to generate an abort.
    !!                                                (default: no change)
    !/ -----------------------------------------------------------------------------------

    tlogger_ah_ptr => proc

    if ( present(new_level) ) then
       tlogger_abort_level = new_level
    end if
  end subroutine tlogger_set_abort_handler


  !/ =====================================================================================
  function log_timestamp() result( ts )
    !/ -----------------------------------------------------------------------------------
    !! Generate a formated time stamp. YYYYMM.DD hh:mm:ss.sss
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=:), allocatable :: ts !! character string of the time stamp.
    !/ -----------------------------------------------------------------------------------
    integer       :: dt(8)
    character(32) :: temp
    !/ -----------------------------------------------------------------------------------

    call DATE_AND_TIME(values=dt)
    write(temp,100) dt(1), dt(2), dt(3), dt(5), dt(6), dt(7), dt(8)
    ts = trim(adjustl(temp))
100 format(I4,I2.2,'.',I2.2,' ',I2.2,':',I2.2,':',I2.2,'.',I3.3)

  end function log_timestamp


  !/ =====================================================================================
  subroutine tlogger_set( fspc, console, file, abort, unit, reset )
    !/ -----------------------------------------------------------------------------------
    !! Set individual or groups of parameters. Including and optional named argument
    !! alters that value. A missing argument leaves that parameter unchanged. 
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*), optional, intent(in) :: fspc    !! optional string  log fle name.
    integer,          optional, intent(in) :: console !! optional integer console level to report.
    integer,          optional, intent(in) :: file    !! integer log file level to report.
    integer,          optional, intent(in) :: abort   !! integer abort level to activate.
    integer,          optional, intent(in) :: unit    !! unit to report console messages on.
    logical,          optional, intent(in) :: reset   !! if set to true all missing arguments
    !!                                                   are reset to thier default values.
    !/ -----------------------------------------------------------------------------------

    if ( present(reset) ) then
       tlogger_abort_level   = tlogger_critical
       tlogger_file_level    = tlogger_warn
       tlogger_console_level = tlogger_info
       tlogger_err_unit      = ERROR_UNIT
       tlogger_log_file_name = 'trncmp.log'
       tlogging_file_enabled = .false.
    end if

    if ( present(fspc) ) then
       tlogger_log_file_name=trim(fspc)
       tlogging_file_enabled = .true.
    end if

    if ( present(console) ) then
       tlogger_console_level = console
    end if

    if ( present(file) ) then
       tlogger_file_level = file
    end if

    if ( present(abort) ) then
       tlogger_abort_level = abort
    end if

    if ( present( unit ) ) then
       tlogger_err_unit = unit
    end if

  end subroutine tlogger_set


  !/ =====================================================================================
  subroutine tlogger_enable
    !/ -----------------------------------------------------------------------------------
    !! Turn the logging on.
    !/ -----------------------------------------------------------------------------------
    implicit none
    tlogging_enabled = .true.
  end subroutine tlogger_enable


  !/ =====================================================================================
  subroutine tlogger_disable
    !/ -----------------------------------------------------------------------------------
    !! Turn the logging off with out altering the logger's current properties.
    !/ -----------------------------------------------------------------------------------
    implicit none
    tlogging_enabled = .false.
  end subroutine tlogger_disable


  !/ =====================================================================================
  subroutine tlogger_enable_file
    !/ -----------------------------------------------------------------------------------
    !! Turn the file logging on.
    !/ -----------------------------------------------------------------------------------
    implicit none
    tlogging_file_enabled = .true.
  end subroutine tlogger_enable_file


  !/ =====================================================================================
  subroutine tlogger_disable_file
    !/ -----------------------------------------------------------------------------------
    !! Turn the file logging off with out altering the logger's current properties.
    !/ -----------------------------------------------------------------------------------
    implicit none
    tlogging_file_enabled = .false.
  end subroutine tlogger_disable_file


  !/ =====================================================================================
  subroutine tlogger_enable_console
    !/ -----------------------------------------------------------------------------------
    !! Turn the console logging on.
    !/ -----------------------------------------------------------------------------------
    implicit none
    tlogging_console_enabled = .true.
  end subroutine tlogger_enable_console


  !/ =====================================================================================
  subroutine tlogger_disable_console
    !/ -----------------------------------------------------------------------------------
    !! Turn the console logging off with out altering the logger's current properties.
    !/ -----------------------------------------------------------------------------------
    implicit none
    tlogging_console_enabled = .false.
  end subroutine tlogger_disable_console


  !/ =====================================================================================
  subroutine tlogger_write_message( level, msg, R8, R4, I4, I2, STR )
    !/ -----------------------------------------------------------------------------------
    !! Generate a log message. 
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,                       intent(in) :: level !! message severity level.
    character(len=*),              intent(in) :: msg   !! message.
    real(dp),            optional, intent(in) :: R8    !! optional 64 bit float   value.
    real(kind=real32),   optional, intent(in) :: R4    !! optional 32 bit float   value.
    integer(kind=int32), optional, intent(in) :: I4    !! optional 32 bit integer value.
    integer(kind=int16), optional, intent(in) :: I2    !! optional 16 bit integer value.
    character(len=*),    optional, intent(in) :: str   !! optional string         value.
    !/ -----------------------------------------------------------------------------------
    character(len=1), parameter   :: EL(0:5) = [ 'U', 'C', 'E', 'W', 'I', 'D' ]
    integer                       :: un
    character(len=:), allocatable :: ts
    logical                       :: extras
    character(len=32)             :: AR8, AR4, AI4, AI2
    character(len=256)            :: ebuf
    !/ -----------------------------------------------------------------------------------

    if ( tlogging_console_enabled.or.tlogging_file_enabled ) then

       extras = .false.

       ts = log_timestamp()

       !/ --------------------------------------------------------------------------------

       ebuf = ''

       if ( present(R8) ) then
          write(ar8,'(ES12.4)') R8
          ebuf = trim(ebuf) // ': ' // trim(AR8)
          extras = .true.
       end if

       if ( present(R4) ) then
          write(ar4,'(ES11.4)') R4
          ebuf = trim(ebuf) // ': ' // trim(AR4)
          extras = .true.
       end if

       if ( present(I4) ) then
          write(ai4,'(I0)') I4
          ebuf = trim(ebuf) // ': ' // trim(AI4)
          extras = .true.
       end if

       if ( present(I2) ) then
          write(ai2,'(I0)') I2
          ebuf = trim(ebuf) // ': ' // trim(AI2)
          extras = .true.
       end if

       if ( present(STR) ) then
          ebuf = trim(ebuf) // ': ' // trim(STR)
          extras = .true.
       end if

       !/ ----- Console Logging ----------------------------------------------------------

       if (tlogging_console_enabled) then
          if ( level.le.tlogger_console_level ) then
             if ( extras ) then
                write(tlogger_err_unit,100) trim(ts),EL(level),trim(msg),trim(ebuf)
             else
                write(tlogger_err_unit,110) trim(ts),EL(level),trim(msg)
             end if
             flush(tlogger_err_unit)
          end if
       end if

       !/ ----- File Logging -------------------------------------------------------------

       if (tlogging_file_enabled) then
          if ( level.le.tlogger_file_level ) then
             open(newunit=un, file=tlogger_log_file_name, position="append")
             if ( extras ) then
                write(un,100) trim(ts),EL(level),trim(msg),trim(ebuf)
             else
                write(un,110) trim(ts),EL(level),trim(msg)
             end if
             flush(un)
             close(un)
          end if
       end if

    end if

    !/ ----- Abort Handler ---------------------------------------------------------------

    if ( level.le.tlogger_abort_level ) then
       call tlogger_ah_ptr(level)
    end if

100 format('[',A,'] **',A1,'** ',A,A)
110 format('[',A,'] **',A1,'** ',A)

  end subroutine tlogger_write_message


  !/ =====================================================================================
  subroutine log_critical( msg, R8, R4, I4, I2, STR )
    !/ -----------------------------------------------------------------------------------
    !! Generate a critical log message.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),              intent(in) :: msg !! message.
    real(dp),            optional, intent(in) :: R8  !! optional 64 bit float   display value.
    real(kind=real32),   optional, intent(in) :: R4  !! optional 32 bit float   display value.
    integer(kind=int32), optional, intent(in) :: I4  !! optional 32 bit integer display value.
    integer(kind=int16), optional, intent(in) :: I2  !! optional 16 bit integer display value.
    character(len=*),    optional, intent(in) :: str !! optional string         display value.
    !/ -----------------------------------------------------------------------------------

    if ( tlogging_enabled ) then
       call tlogger_write_message( tlogger_critical, msg, R8, R4, I4, I2, STR )
    end if

  end subroutine log_critical


  !/ =====================================================================================
  subroutine log_error( msg, R8, R4, I4, I2, STR )
    !/ -----------------------------------------------------------------------------------
    !! Generate a error log message.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),              intent(in) :: msg !! message.
    real(dp),            optional, intent(in) :: R8  !! optional 64 bit float   display value.
    real(kind=real32),   optional, intent(in) :: R4  !! optional 32 bit float   display value.
    integer(kind=int32), optional, intent(in) :: I4  !! optional 32 bit integer display value.
    integer(kind=int16), optional, intent(in) :: I2  !! optional 16 bit integer display value.
    character(len=*),    optional, intent(in) :: str !! optional string         display value.
    !/ -----------------------------------------------------------------------------------

    if ( tlogging_enabled ) then
       call tlogger_write_message( tlogger_error, msg, R8, R4, I4, I2, STR )
    end if

  end subroutine log_error


  !/ =====================================================================================
  subroutine log_warn( msg, R8, R4, I4, I2, STR )
    !/ -----------------------------------------------------------------------------------
    !! Generate a warning log message.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),              intent(in) :: msg !! message.
    real(dp),            optional, intent(in) :: R8  !! optional 64 bit float   display value.
    real(kind=real32),   optional, intent(in) :: R4  !! optional 32 bit float   display value.
    integer(kind=int32), optional, intent(in) :: I4  !! optional 32 bit integer display value.
    integer(kind=int16), optional, intent(in) :: I2  !! optional 16 bit integer display value.
    character(len=*),    optional, intent(in) :: str !! optional string         display value.
    !/ -----------------------------------------------------------------------------------

    if ( tlogging_enabled ) then
       call tlogger_write_message( tlogger_warn, msg, R8, R4, I4, I2, STR )
    end if

  end subroutine log_warn


  !/ =====================================================================================
  subroutine log_info( msg, R8, R4, I4, I2, STR )
    !/ -----------------------------------------------------------------------------------
    !! Generate a info log message.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),              intent(in) :: msg !! message.
    real(dp),            optional, intent(in) :: R8  !! optional 64 bit float   display value.
    real(kind=real32),   optional, intent(in) :: R4  !! optional 32 bit float   display value.
    integer(kind=int32), optional, intent(in) :: I4  !! optional 32 bit integer display value.
    integer(kind=int16), optional, intent(in) :: I2  !! optional 16 bit integer display value.
    character(len=*),    optional, intent(in) :: str !! optional string         display value.
    !/ -----------------------------------------------------------------------------------

    if ( tlogging_enabled ) then
       call tlogger_write_message( tlogger_info, msg, R8, R4, I4, I2, STR )
    end if

  end subroutine log_info


  !/ =====================================================================================
  subroutine log_debug( msg, R8, R4, I4, I2, STR )
    !/ -----------------------------------------------------------------------------------
    !! Generate a debug log message.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*),              intent(in) :: msg !! message.
    real(dp),            optional, intent(in) :: R8  !! optional 64 bit float   display value.
    real(kind=real32),   optional, intent(in) :: R4  !! optional 32 bit float   display value.
    integer(kind=int32), optional, intent(in) :: I4  !! optional 32 bit integer display value.
    integer(kind=int16), optional, intent(in) :: I2  !! optional 16 bit integer display value.
    character(len=*),    optional, intent(in) :: str !! optional string         display value.
    !/ -----------------------------------------------------------------------------------

    if ( tlogging_enabled ) then
       call tlogger_write_message( tlogger_debug, msg, R8, R4, I4, I2, STR )
    end if

  end subroutine log_debug


end module tlogger

!/ =======================================================================================
!/ **                                   T L O G G E R                                   **
!/ =========================================================================== END FILE ==
