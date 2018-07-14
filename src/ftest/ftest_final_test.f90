!/ ====================================================================== BEGIN FILE =====
!/ **                          F T E S T _ F I N A L _ T E S T                          **
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
module ftest_final_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-25
  !! license: GPL
  !!
  !!##Test of .
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private


  !/ =====================================================================================
  type :: ftest
     !/ ----------------------------------------------------------------------------------

     integer                            :: num = 0
     integer, allocatable, dimension(:) :: data

   contains

     procedure, public :: init   => ftest_init
     procedure, public :: size   => ftest_size
     procedure, public :: get    => ftest_get
     procedure, public :: set    => ftest_set
     procedure, public :: delete => ftest_delete

     final :: ftest_destroy

  end type ftest


  !/ -------------------------------------------------------------------------------------
  interface create
     !/ ----------------------------------------------------------------------------------
     module procedure :: ftest_create
  end interface create


  !/ -------------------------------------------------------------------------------------
  interface ftest
     !/ ----------------------------------------------------------------------------------
     module procedure :: ftest_alloc
  end interface ftest

  public :: create
  public :: ftest

  !/ -------------------------------------------------------------------------------------
  interface nmtest
     !/ ----------------------------------------------------------------------------------
     module procedure :: ftest_namelist_test
  end interface nmtest

  public :: nmtest

  public :: h5test

  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  !/ =====================================================================================
  function build_command_string() result( buffer )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=:), allocatable :: buffer
    !/ -----------------------------------------------------------------------------------
    character(len=64) :: argv
    integer :: argc, i

    argc = COMMAND_ARGUMENT_COUNT()
    write(*,*) 'cmd=',argc
    
    call GET_COMMAND_ARGUMENT( 1, argv )
    buffer = trim(adjustl(argv))
    
    do i=2,argc
       call GET_COMMAND_ARGUMENT( i, argv )
       buffer = buffer // ", " // trim(adjustl(argv))
    end do
    
  end function build_command_string

  !/ =====================================================================================
  subroutine ftest_namelist_test
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------

    real(dp) :: pcross, pmutate
    integer  :: npop, ngen, u, cmdlen
    integer  :: list(4)
    character(4) :: xx
    character(len=256) :: cstr
    character(len=256) :: message
    character(len=:), allocatable :: cmd
    integer :: ios
    
    NAMELIST /uga/ pcross, pmutate, npop, ngen, list, xx

    
    cmd = build_command_string()
    write(*,*) 'cmd=[',cmd,']'

    
    open ( newunit=u, FILE='/data/fdata/nametest.dat', STATUS='OLD' )

    read( u, uga )

    cmd = "&uga " // cmd // " //"
    write(*,*) 'nml=[',cmd,']'
    READ( cmd, NML=uga, iostat=ios, iomsg=message )
    if(ios.ne.0)then
      write(*,*)'error: ',ios
      write(*,*)message
   endif
   
    write( *, uga )

  end subroutine ftest_namelist_test


  !/ =====================================================================================
  subroutine h5test
    !/ -----------------------------------------------------------------------------------
    use hdf5
    implicit none

    CHARACTER(LEN=8), PARAMETER :: filename = "filef.h5" ! File name
    INTEGER(HID_T) :: file_id                            
    INTEGER     ::   error  
    CALL h5open_f(error)               
    CALL h5fcreate_f(filename, H5F_ACC_TRUNC_F, file_id, error)
    CALL h5fclose_f(file_id, error) 
    CALL h5close_f(error)               


  end subroutine h5test
  
  !/ =====================================================================================
  subroutine ftest_create( obj, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ftest), intent(inout) :: obj
    integer,     intent(in)    :: n
    !/ -----------------------------------------------------------------------------------
    write(*,*) '  v-- ftest_create', n
    call obj%init( n )
    write(*,*) '  ^-- ftest_create', n

  end subroutine ftest_create


  !/ =====================================================================================
  function ftest_alloc( n ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,      intent(in) :: n
    class(ftest), pointer    :: ptr
    !/ -----------------------------------------------------------------------------------

    write(*,*) '  v-- ftest_alloc', n
    write(*,*) '    v-- allocate'
    allocate( ptr )
    write(*,*) '      ^-- allocate/create --v'
    call create( ptr, n )
    write(*,*) '    ^-- create'
    write(*,*) '  ^-- ftest_alloc', n

  end function ftest_alloc


  !/ =====================================================================================
  subroutine ftest_destroy( obj )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ftest), intent(inout) :: obj
    !/ -----------------------------------------------------------------------------------

    write(*,*) '  v-- ftest_destroy ** FINAL'
    call obj%delete
    write(*,*) '  ^-- ftest_destroy ** FINAL'

  end subroutine ftest_destroy


  !/ =====================================================================================
  subroutine ftest_init( self, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ftest), intent(inout) :: self
    integer,      intent(in)    :: n
    !/ -----------------------------------------------------------------------------------

    write(*,*) '  v-- ftest_init', n
    if ( allocated( self%data ) ) then
       write(*,*) '    v-- deallocate'
       deallocate( self%data )
       write(*,*) '    ^-- deallocate'
    end if
    self%num = n
    write(*,*) '    v-- allocate'
    allocate( integer :: self%data(n) )
    write(*,*) '    ^-- allocate'
    write(*,*) '  ^-- ftest_init', n

  end subroutine ftest_init


  !/ =====================================================================================
  function ftest_size( self ) result( n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ftest), intent(inout) :: self
    integer                     :: n
    !/ -----------------------------------------------------------------------------------

    n = 0

    write(*,*) '  v-- ftest_size'
    if ( allocated( self%data ) ) then
       n = self%num
       write(*,*) '  -- size', n
    else
       write(*,*) '  -- empty'
    end if
    write(*,*) '  ^-- ftest_size'

  end function ftest_size


  !/ =====================================================================================
  function ftest_get( self, n ) result( val )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ftest), intent(inout) :: self
    integer,      intent(in)    :: n
    integer                     :: val
    !/ -----------------------------------------------------------------------------------

    write(*,*) '  v-- ftest_get'
    if ( allocated( self%data ) ) then
       write(*,*) '  -- get'
       val = self%data(n)
    else
       write(*,*) '  -- empty'
       val = 0
    end if
    write(*,*) '  ^-- ftest_get', val

  end function ftest_get


  !/ =====================================================================================
  subroutine ftest_set( self, n, val )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ftest), intent(inout) :: self
    integer,      intent(in)    :: n
    integer,      intent(in)    :: val
    !/ -----------------------------------------------------------------------------------

    write(*,*) '  v-- ftest_set', n
    if ( allocated( self%data ) ) then
       write(*,*) '  -- set', val
       self%data(n) = val
    else
       write(*,*) '  -- empty'
    end if
    write(*,*) '  ^-- ftest_set', n

  end subroutine ftest_set


  !/ =====================================================================================
  subroutine ftest_delete( self )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ftest), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------

    write(*,*) '  v-- ftest_delete'
    if ( allocated( self%data ) ) deallocate( self%data )
    self%num = 0
    write(*,*) '  ^-- ftest_delete'

  end subroutine ftest_delete


end module ftest_final_test




!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_final_test
  implicit none

  write (*,*) '===================================================='

  block
    type(ftest) :: obj
    integer :: n
    write(*,*) '<<-- TYPE TEST'
    call create( obj, 3 )
    n = obj%size()
    write(*,*) '>>-- TYPE TEST'
  end block

  write (*,*) '----------------------------------------------------'

  block
    class(ftest), pointer :: ptr
    integer :: n
    write(*,*) '<<-- POINTER TEST NO DEALLOCATE'
    ptr => ftest(3)
    n = ptr%size()
    write(*,*) '>>-- POINTER TEST NO DEALLOCATE'
  end block

  write (*,*) '----------------------------------------------------'

  block
    class(ftest), pointer :: ptr
    integer :: n
    write(*,*) '<<-- POINTER TEST WITH DEALLOCATE'
    ptr => ftest(3)
    n = ptr%size()
    deallocate(ptr)
    write(*,*) '>>-- POINTER TEST WITH DEALLOCATE'
  end block

  write (*,*) '===================================================='

  call nmtest

  call h5test
  
end program main

!/ =======================================================================================
!/ **                          F T E S T _ F I N A L _ T E S T                          **
!/ =========================================================================== END FILE ==
