!/ ====================================================================== BEGIN FILE =====
!/ **                                F I L E _ T O O L S                                **
!! =======================================================================================
!! **                                                                                   **
!! **  Copyright (c) 2017, Stephen W. Soliday                                           **
!! **                      stephen.soliday@trncmp.org                                   **
!! **                      http://research.trncmp.org                                   **
!! **                                                                                   **
!! **  -------------------------------------------------------------------------------  **
!! **                                                                                   **
!! **  This program is free software: you can redistribute it and/or modify it under    **
!! **  the terms of the GNU General Public License as published by the Free Software    **
!! **  Foundation, either version 3 of the License, or (at your option)                 **
!! **  any later version.                                                               **
!! **                                                                                   **
!! **  This program is distributed in the hope that it will be useful, but WITHOUT      **
!! **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
!! **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
!! **                                                                                   **
!! **  You should have received a copy of the GNU General Public License along with     **
!! **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
!! **                                                                                   **
!! =======================================================================================
module file_tools
  !! -------------------------------------------------------------------------------------
  !! Provides a collection of tools for manipulating files.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2017-03-27
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use string_tools
  implicit none

  ! POSIX File Modes

  integer, public, parameter :: S_IFMT   = 61440 !! (0x0170000) These bits determine file type.

  integer, public, parameter :: S_IFDIR  = 16384 !! (0x0040000) Directory.
  integer, public, parameter :: S_IFCHR  =  8192 !! (0x0020000) Character device.
  integer, public, parameter :: S_IFBLK  = 24576 !! (0x0060000) Block device.
  integer, public, parameter :: S_IFREG  = 32768 !! (0x0100000) Regular file.
  integer, public, parameter :: S_IFIFO  =  4096 !! (0x0010000) FIFO.
  integer, public, parameter :: S_IFLNK  = 40960 !! (0x0120000) Symbolic link.
  integer, public, parameter :: S_IFSOCK = 49152 !! (0x0140000) Socket.

  integer, public, parameter :: S_ISUID  =  2048 !! (0x0004000) Set user  ID on execution.
  integer, public, parameter :: S_ISGID  =  1024 !! (0x0002000) Set group ID on execution.
  integer, public, parameter :: S_ISVTX  =   512 !! (0x0001000) Save swapped text after use (sticky).
  integer, public, parameter :: S_IREAD  =   256 !! (0x0000400) Read by owner.
  integer, public, parameter :: S_IWRITE =   128 !! (0x0000200) Write by owner.
  integer, public, parameter :: S_IEXEC  =    64 !! (0x0000100) Execute by owner.

  private :: S_ISTYPE

  public :: S_ISDIR
  public :: S_ISCHR
  public :: S_ISBLK
  public :: S_ISREG
  public :: S_ISFIFO
  public :: S_ISLNK
  public :: S_ISSOCK

  public :: dirExists
  public :: fileExists
  public :: findFile
  public :: timeStamp
  public :: getEnvironment

  public :: ReadUnit
  public :: WriteUnit
  public :: LineCount




  !! =====================================================================================
contains !!**                   P R O C E D U R E   S E C T I O N                       **
  !! =====================================================================================




  !! =====================================================================================
  function S_ISTYPE( mode, mask ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Type.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! file type portion of the bit mask
    integer, intent(in) :: mode !! POSIX file mode.
    integer, intent(in) :: mask !! file mode mask
    !! -----------------------------------------------------------------------------------

    chk = .false.

    if ( mask.eq.AND( mode, S_IFMT) ) then
       chk = .true.
    end if

  end function S_ISTYPE


  !! =====================================================================================
  function S_ISDIR( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Directory.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a Directory.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFDIR )

  end function S_ISDIR


  !! =====================================================================================
  function S_ISCHR( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Character device.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a Character device.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFCHR )

  end function S_ISCHR


  !! =====================================================================================
  function S_ISBLK( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Block device.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a  Block device.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFBLK )

  end function S_ISBLK


  !! =====================================================================================
  function S_ISREG( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Regular file.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a Regular file.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFREG )

  end function S_ISREG


  !! =====================================================================================
  function S_ISFIFO( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is FIFO.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a FIFO.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFIFO )

  end function S_ISFIFO


  !! =====================================================================================
  function S_ISLNK( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Symbolic link.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a Symbolic link.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFLNK )

  end function S_ISLNK


  !! =====================================================================================
  function S_ISSOCK( mode ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Is Socket.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical             :: chk  !! true if this mode is a socket.
    integer, intent(in) :: mode !! POSIX file mode.
    !! -----------------------------------------------------------------------------------

    chk = S_ISTYPE( mode, S_IFSOCK )

  end function S_ISSOCK


  !! =====================================================================================
  function getEnvironment( key ) result( val )
    !! -----------------------------------------------------------------------------------
    !! Get Environment Variable.
    !! If no key is availble return NULL
    !! -----------------------------------------------------------------------------------
    character(len=:), allocatable :: val !! environment variable designated by key.
    character(len=*),  intent(in) :: key !! key
    !! -----------------------------------------------------------------------------------
    character(len=256) :: work
    integer            :: ios
    !! -----------------------------------------------------------------------------------

    call get_environment_variable(trim(adjustl(key)), work, STATUS=ios, TRIM_NAME=.true.)

    if ( 0.eq.ios ) then
       val = trim(adjustl(work))
    else
       val = ''
    end if

  end function getEnvironment


  !! =====================================================================================
  function fixPath( fspc ) result( path )
    !! -----------------------------------------------------------------------------------
    !! Fix Path.
    !!
    !! This routine replaces the leading tilda(~) with the value of the environment
    !! variable HOME
    !!
    !! Example:   the environment HOME=/home/user5
    !!            fspc = ~/test/file.dat
    !!
    !!            path = /home/user/test/file.dat
    !! -----------------------------------------------------------------------------------
    implicit none
    character(len=:), allocatable :: path !! fixed path
    character(len=*), intent(in)  :: fspc !! unmodified path
    !! -----------------------------------------------------------------------------------
    integer            :: ios
    character(len=255) :: home
    !! -----------------------------------------------------------------------------------

    path = trim(adjustl(fspc))

    if ( '~'.eq.path(1:1) ) then
       if ( '/'.eq.path(2:2) ) then
          call GET_ENVIRONMENT_variable('HOME', home, STATUS=ios, TRIM_NAME=.true.)
          if ( 0.eq.ios ) then
             path = trim(home) // path(2:)
          end if
       end if
    end if

  end function fixPath


  !! =====================================================================================
  function dirExists( fspc ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! Directory Exists.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical                      :: chk  !! true if the directory exists.
    character(len=*), intent(in) :: fspc !! full path to the directory.
    !! -----------------------------------------------------------------------------------
    integer, dimension(13) :: buff
    integer                :: status
    !! -----------------------------------------------------------------------------------

    chk = .false.

    call stat(fixPath(fspc), buff, status)

    if (0.eq.status) then
       chk = S_ISDIR( buff(3) )
    end if

  end function dirExists


  !! =====================================================================================
  function fileExists( fspc ) result( chk )
    !! -----------------------------------------------------------------------------------
    !! File Exists.
    !! -----------------------------------------------------------------------------------
    implicit none
    logical                      :: chk  !! true if the file exists.
    character(len=*), intent(in) :: fspc !! full path to the file.
    !! -----------------------------------------------------------------------------------
    integer, dimension(13) :: buff
    integer                :: status
    !! -----------------------------------------------------------------------------------

    chk = .false.

    call stat(fixPath(fspc), buff, status)

    if (0.eq.status) then
       chk = S_ISREG( buff(3) )
    end if

  end function fileExists


  !! =====================================================================================
  function findFile( name, path ) result( fspc )
    !! -----------------------------------------------------------------------------------
    !! Path Search.
    !! -----------------------------------------------------------------------------------
    implicit none
    character(len=:), allocatable :: fspc !! full path of the file if found, otherwise a NULL.
    character(len=*),  intent(in) :: name !! file name to search for.
    character(len=*),  intent(in) :: path !! ':' delimeted list of paths.
    !! -----------------------------------------------------------------------------------
    integer                        :: i, n
    type(string_splitter)          :: plist
    character(len=:),  allocatable :: test
    !! -----------------------------------------------------------------------------------

    call split( plist, path, ':' )

    n = plist%count()

    fspc = ''

    chk_all: do i=1,n
       test = plist%get(i) // '/' // trim(adjustl(name))
       if ( fileExists( test ) ) then
          fspc = test
          exit chk_all
       end if
    end do chk_all

  end function findFile


  !! =====================================================================================
  function timeStamp( seconds ) result( ts )
    !! -----------------------------------------------------------------------------------
    !! Timestamp.
    !!
    !! Generate a formated time stamp. YYYYMMDD-hhmmss or YYYYMMDD-hhmm
    !! -----------------------------------------------------------------------------------
    character(len=:), allocatable :: ts      !! pointer to a character array containing the formated time stamp.
    logical, optional, intent(in) :: seconds !! flag to add seconds.
    !! -----------------------------------------------------------------------------------
    character(len=8)  :: date
    character(len=10) :: time
    logical           :: us
    !! -----------------------------------------------------------------------------------

    us = .false.

    if ( present( seconds ) ) then
       if ( seconds ) then
          us = .true.
       end if
    end if

    call date_and_time(DATE=date,TIME=time)

    if ( us ) then
       ts = date // 'T' // time(:6) // 'Z'
    else
       ts = date // 'T' // time(:4) // 'Z'
    end if

  end function timeStamp




  !! =====================================================================================
  subroutine ReadLine( unit, line, IOSTAT )
    !! -----------------------------------------------------------------------------------
    !! 
    !! -----------------------------------------------------------------------------------
    use, INTRINSIC :: ISO_FORTRAN_ENV, only: IOSTAT_END    
    use, INTRINSIC :: ISO_FORTRAN_ENV, only: IOSTAT_EOR    
    implicit none
    integer,                   intent(in)  :: unit   !! 
    character(:), allocatable, intent(out) :: line   !! 
    integer, optional,         intent(out) :: IOSTAT !! 
    !! -------------------------------------------------------------------------------------
    integer           :: nr, ios
    character(len=32) :: buffer
    !! -------------------------------------------------------------------------------------
    line = ''
    do
       read (unit, '(A)', ADVANCE='NO', IOSTAT=ios, SIZE=nr ) buffer

       if ( ios.eq.IOSTAT_END ) goto 999

       if ( ios.gt.0 ) goto 999

       line = line // buffer(:nr)

       if ( ios.lt.0 ) then
          if ( ios.eq.IOSTAT_EOR ) then
             ios = 0
             goto 999
          end if
       end if
    end do

999 continue

    if ( present( IOSTAT ) ) IOSTAT = ios    

  end subroutine ReadLine





  !! =====================================================================================
  function ReadUnit( FILE, UNIT, IOSTAT ) result( newun )
    !! -----------------------------------------------------------------------------------
    !! Read Unit.
    !!
    !! Meant to pass optional dummy arguments FILE, UNIT, IOSTAT from a user procedure
    !! and return a unit number of an input device. Use if present(UNIT) to determine
    !! if the file unit should be closed.
    !! -----------------------------------------------------------------------------------
    use tlogger, only : log_error
    implicit none
    integer                                 :: newun  !! unit for an input device.
    character(len=*), optional, intent(in)  :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)  :: UNIT   !! unit for an open unit
    integer,          optional, intent(out) :: IOSTAT !! error return.
    !! -----------------------------------------------------------------------------------
    integer            :: ios, tp
    logical            :: report
    character(len=255) :: emsg
    !! -----------------------------------------------------------------------------------

    tp     = 0
    ios    = 0
    newun  = 0
    report = .true.

    if ( present( FILE   ) ) tp = tp + 1
    if ( present( UNIT   ) ) tp = tp + 2
    if ( present( IOSTAT ) ) report = .false.

    select case(tp)

    case(0)
       if ( report ) then
          call log_error( 'labeled dummy argument FSPC= or UNIT= is required' )
       end if
       ios = 6000

    case(1)
       open( FILE=trim(adjustl(file)), NEWUNIT=newun, ACTION='READ', IOSTAT=ios )
       call gerror(emsg)
       if ( 0.ne.ios ) then
          if ( report ) then
             call log_error( 'Cannot open file for reading', STR=file )
          end if
       end if

    case(2)
       newun=UNIT

    case default
       if ( report ) then
          call log_error( 'labeled dummy arguments FSPC= and UNIT= cannont both be present' )
       end if
       ios = 6001

    end select

    if ( present( IOSTAT ) ) iostat = ios

  end function ReadUnit


  !! =====================================================================================
  function WriteUnit( FILE, UNIT, IOSTAT ) result( newun )
    !! -----------------------------------------------------------------------------------
    !! Write Unit.
    !!
    !! Meant to pass optional dummy arguments FILE, UNIT, IOSTAT from a user procedure
    !! and return a unit number of an input device. Use if present(UNIT) to determine
    !! if the file unit should be closed.
    !! -----------------------------------------------------------------------------------
    use tlogger, only : log_error
    implicit none
    integer                                 :: newun  !! 
    character(len=*), optional, intent(in)  :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)  :: UNIT   !! file unit for an open unit
    integer,          optional, intent(out) :: IOSTAT !! error return.
    !! -----------------------------------------------------------------------------------
    integer :: ios, tp
    logical :: report
    !! -----------------------------------------------------------------------------------

    tp     = 0
    ios    = 0
    newun  = 0
    report = .true.

    if ( present( FILE   ) ) tp = tp + 1
    if ( present( UNIT   ) ) tp = tp + 2
    if ( present( IOSTAT ) ) report = .false.

    select case(tp)

    case(0)
       if ( report ) then
          call log_error( 'labeled dummy argument FSPC= or UNIT= is required' )
       end if
       ios = 6000

    case(1)
       open( FILE=trim(adjustl(file)), NEWUNIT=newun, ACTION='WRITE', STATUS='REPLACE', IOSTAT=ios )
       if ( 0.ne.ios ) then
          if ( report ) then
             call log_error( 'Cannot open file for writing', STR=file )
          end if
       end if

    case(2)
       newun=UNIT

    case default
       if ( report ) then
          call log_error( 'labeled dummy arguments FSPC= and UNIT= cannont both be present' )
       end if
       ios = 6001

    end select

    if ( present( IOSTAT ) ) iostat = ios

  end function WriteUnit


  !! =====================================================================================
  function LineCount( FILE, UNIT, IOSTAT ) result( N )
    !! -----------------------------------------------------------------------------------
    !! 
    !! -----------------------------------------------------------------------------------
    implicit none
    integer                                 :: N      !! 
    character(len=*), optional, intent(in)  :: FILE   !! 
    integer,          optional, intent(in)  :: UNIT   !! 
    integer,          optional, intent(out) :: IOSTAT !! 
    !! -----------------------------------------------------------------------------------
    integer :: inf, ios, rios
    character(128) :: buffer
    logical :: report
    !! -----------------------------------------------------------------------------------
    N      = 0
    ios    = 0
    report = .true.
    if ( present( IOSTAT ) ) report = .false.
    inf = ReadUnit( FILE=FILE, UNIT=UNIT, IOSTAT=ios )
    if ( 0.eq.ios ) then
100    continue
       read(inf,*,END=120, ERR=110, IOSTAT=rios) buffer
       if ( rios.ne.0 ) then
          write (*,*) 'IOS=', ios
          goto 120
       end if
       N = N + 1
       goto 100
110    continue
       if ( report ) then
          write (*,*) 'Read failed'
       end if
120    continue
       close(inf)
    else
       if ( report ) then
          write (*,*) 'Open Failed'
       end if
    end if

    if ( present( IOSTAT ) ) IOSTAT=ios

  end function LineCount


end module file_tools


!! =======================================================================================
!! **                                F I L E _ T O O L S                                **
!! =========================================================================== END FILE ==
