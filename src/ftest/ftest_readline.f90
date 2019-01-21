!/ ====================================================================== BEGIN FILE =====
!/ **                            F T E S T _ R E A D L I N E                            **
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
module ftest_readline_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-19
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use ISO_FORTRAN_ENV
  use trncmp_env
  use file_tools
  use posix_error_mod
  implicit none



  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine SHOWI4( lab, val )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: lab
    integer,      intent(in) :: val
    !/ -----------------------------------------------------------------------------------
    write ( *, "(I7,': ',A)" ) val, lab
  end subroutine SHOWI4


  
  !/ =====================================================================================
  subroutine test01
    !/ -----------------------------------------------------------------------------------


call SHOWI4( 'ERROR_UNIT',                      ERROR_UNIT )
call SHOWI4( 'FILE_STORAGE_SIZE',               FILE_STORAGE_SIZE )
call SHOWI4( 'INPUT_UNIT',                      INPUT_UNIT )
call SHOWI4( 'INT8',                            INT8 )
call SHOWI4( 'INT16',                           INT16 )
call SHOWI4( 'INT32',                           INT32 )
call SHOWI4( 'INT64',                           INT64 )
call SHOWI4( 'IOSTAT_END',                      IOSTAT_END )
call SHOWI4( 'IOSTAT_EOR',                      IOSTAT_EOR )
call SHOWI4( 'IOSTAT_INQUIRE_INTERNAL_UNIT',    IOSTAT_INQUIRE_INTERNAL_UNIT )
call SHOWI4( 'NUMERIC_STORAGE_SIZE',            NUMERIC_STORAGE_SIZE )
!call SHOWI4( 'LOGICAL_KINDS',                   LOGICAL_KINDS )
call SHOWI4( 'REAL32',                          REAL32 )
call SHOWI4( 'REAL128',                         REAL128 )
!call SHOWI4( 'REAL_KINDS',                      REAL_KINDS )
call SHOWI4( 'STAT_LOCKED',                     STAT_LOCKED )
call SHOWI4( 'STAT_LOCKED_OTHER_IMAGE',         STAT_LOCKED_OTHER_IMAGE )
call SHOWI4( 'STAT_STOPPED_IMAGE',              STAT_STOPPED_IMAGE )
call SHOWI4( 'STAT_FAILED_IMAGE',               STAT_FAILED_IMAGE )
call SHOWI4( 'STAT_UNLOCKED',                   STAT_UNLOCKED )
!call SHOWI4( 'LOCK_TYPE',                       LOCK_TYPE )


  end subroutine test01

  
  !/ =====================================================================================
  subroutine test02
    !/ -----------------------------------------------------------------------------------
    use, INTRINSIC :: ISO_FORTRAN_ENV, only: IOSTAT_END    
    implicit none
    !/ -------------------------------------------------------------------------------------
    character(*), parameter   :: fspc = '../test.dat'
    integer                   :: un, ios
    character(:), allocatable :: buffer
    !/ -------------------------------------------------------------------------------------

    open( FILE=fspc, NEWUNIT=un, ACTION='READ', IOSTAT=ios )

    if (0.eq.ios) then
       readloop: do
          call ReadLine(un, buffer, IOSTAT=ios)
          if ( ios.eq.IOSTAT_END ) exit readloop
          write(*,100) buffer
       end do readloop
    end if
    
100 format( '[',A,']' )

  end subroutine test02

  
    
end module ftest_readline_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_readline_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test01
  call test02
  
end program main

!/ =======================================================================================
!/ **                            F T E S T _ R E A D L I N E                            **
!/ =========================================================================== END FILE ==
