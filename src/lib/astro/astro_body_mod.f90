!/ ====================================================================== BEGIN FILE =====
!/ **                            A S T R O _ B O D Y _ M O D                            **
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
module astro_body_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-23
  !! license: GPL
  !!
  !!## Object.
  !!
  !! Object.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none

  !/ =====================================================================================
  type :: AstroBody
     !/ ----------------------------------------------------------------------------------
     character(16)         :: name       !! body name
     real(dp)              :: radP       !! polar radius
     real(dp)              :: radE       !! equitorial radius
     real(dp)              :: mass       !! mass
     real(dp)              :: mu         !! gravitational constant
     real(dp)              :: delta      !! time delta in seconds
     real(dp), allocatable :: table(:,:) !! vector table

   contains

     !final :: ab_destroy()

     procedure :: read  => ab_read_from_file
     procedure :: write => ab_write_to_file
     
     
  end type AstroBody



  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine ab_write_to_file( dts, fspc, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(AstroBody),           intent(inout) :: dts
    character(len=*), optional, intent(in)    :: fspc   !! path to an new or existing file.
    integer,          optional, intent(out)   :: IOSTAT !! error return.
    !/ -----------------------------------------------------------------------------------
    integer :: fh, ierr, i, j, m, n
    logical :: report
    !/ -----------------------------------------------------------------------------------

    fh     = 0
    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    open( NEWUNIT=fh, FILE=trim(adjustl(fspc)), FORM='unformatted', ACTION='WRITE', &
         &            STATUS='REPLACE', IOSTAT=ierr, ACCESS='stream' )

    if ( 0.eq.ierr ) then

       m = size( dts%table, DIM=1 )
       n = size( dts%table, DIM=2 )

       write( fh ) dts%name
       write( fh ) dts%radP, dts%radE, dts%mass, dts%mu

       write( fh ) m, n

       do j=1,n
          write( fh ) ( dts%table(i,j), i=1,m )
       end do

       close( fh )

    else
       if ( report ) then
          call log_error( 'Cannot open file for writting',  STR=fspc )
       end if
    end if

    if ( present( IOSTAT ) ) IOSTAT = ierr

  end subroutine ab_write_to_file


  !/ =====================================================================================
  subroutine ab_read_from_file( dts, fspc, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(AstroBody), intent(inout) :: dts
    character(len=*), optional, intent(in)  :: fspc   !! path to an new or existing file.
    integer,          optional, intent(out) :: IOSTAT !! error return.
    !/ -----------------------------------------------------------------------------------
    integer :: fh, ierr, i, j, m, n
    logical :: report
    !/ -----------------------------------------------------------------------------------

    fh     = 0
    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    !/ -----------------------------------------------------------------------------------

    open( NEWUNIT=fh, FILE=trim(adjustl(fspc)), FORM='unformatted', &
         &            ACTION='READ', IOSTAT=ierr, ACCESS='stream' )

    if ( 0.eq.ierr ) then

       read( fh ) dts%name
       read( fh ) dts%radP, dts%radE, dts%mass, dts%mu

       read( fh ) m, n

       if ( allocated( dts%table ) ) deallocate( dts%table )
       allocate( dts%table(m,n) )

       do j=1,n
          read( fh ) ( dts%table(i,j), i=1,m )
       end do

       close( fh )

    else
       if ( report ) then
          call log_error( 'Cannot open file for reading',  STR=fspc )
       end if
    end if

    if ( present( IOSTAT ) ) IOSTAT = ierr

  end subroutine ab_read_from_file


end module astro_body_mod


!/ =======================================================================================
!/ **                            A S T R O _ B O D Y _ M O D                            **
!/ =========================================================================== END FILE ==
