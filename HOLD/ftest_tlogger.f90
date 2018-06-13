!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ T L O G G E R                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
program main
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2015-11-22
  !! license: GPL
  !!
  !!##Test of the TLogger utility.
  !!
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use tlogger
  implicit none
  !/ -------------------------------------------------------------------------------------

  !  used is the abort function is external
  !  interface
  !     subroutine my_abort( level )
  !       integer, intent(in) :: level
  !     end subroutine my_abort
  !  end interface

  character(len=*), parameter :: fspc = '/var/log/test/log'

  procedure(tlogger_abort_handler), pointer :: mp => my_abort

  call tlogger_set(console=tlogger_debug, file=tlogger_warning, fspc='mytest.log')
  call tlogger_set_abort_handler( mp )

  call log_debug(    "This is a debug message", I4=35 )
  call log_info(     "This is an info message" )
  call log_warning(  "This is a warning message number", STR=fspc, R8=3.4d0 )
  call log_error(    "This is an error message" )

  call log_critical( "This is a  critical message" )

contains

  subroutine my_abort( n )
    use trncmp_env
    implicit none
    integer, intent(in) :: n
    write(ERROR_UNIT,*) 'Using my handler with no abort, code:', n
  end subroutine my_abort

end program main

!/ =======================================================================================
!/ **                             F T E S T _ T L O G G E R                             **
!/ =========================================================================== END FILE ==
