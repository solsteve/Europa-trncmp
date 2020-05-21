!/ ====================================================================== BEGIN FILE =====
!/ **                          H O R I Z O N _ D A T A _ M O D                          **
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
module horizon_data_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use file_tools
  use string_tools
  use tlogger
  implicit none

  character(5), parameter :: MARK1 = '$$SOE'
  character(5), parameter :: MARK2 = '$$EOE'

  real(dp), parameter :: ASTRO_UNIT = 1.49597870700D11 ! meters
  real(dp), parameter :: GRAV       = 6.67428D-11  ! m^3/kg s^2
  real(dp), parameter :: GRAV_KM    = 6.67428D-20  ! km^3/kg s^2
  
  !/ =====================================================================================
  type :: HorizonData
    !/ -----------------------------------------------------------------------------------
    !  JDTDB  Julian Day Number, Barycentric Dynamical Time
    !  del_T  Time-scale conversion difference TDB - UT (s)
    !  X      X-component of position vector (au)                               
    !  Y      Y-component of position vector (au)                               
    !  Z      Z-component of position vector (au)                               
    !  VX     X-component of velocity vector (au/day)                           
    !  VY     Y-component of velocity vector (au/day)                           
    !  VZ     Z-component of velocity vector (au/day)                           
    !/ -----------------------------------------------------------------------------------

     real(dp), allocatable :: state(:,:)
     real(dp), allocatable :: jdtdb(:)
     real(dp), allocatable :: deltaT(:)
     integer               :: num_rec = 0

   contains

     final     ::         hd_destroy
     procedure :: resize => hd_resize
     procedure :: read   => hd_read_csv

  end type HorizonData
  

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  
  !/ =====================================================================================
  subroutine hd_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(HorizonData), intent(inout) :: dts

    if ( 0.lt.dts%num_rec ) then
       deallocate( dts%state )
       deallocate( dts%jdtdb )
       deallocate( dts%deltaT )
    end if
    dts%num_rec = 0
  end subroutine hd_destroy
  

  !/ =====================================================================================
  subroutine hd_resize( dts, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HorizonData), intent(inout) :: dts
    integer,            intent(in)    :: n
    !/ -----------------------------------------------------------------------------------

    if ( n.ne.dts%num_rec ) then
       call hd_destroy( dts )
    end if
    
    allocate( dts%state(6,n) )
    allocate( dts%jdtdb(n) )
    allocate( dts%deltaT(n) )
    dts%num_rec = n

  end subroutine hd_resize

  
  !/ =====================================================================================
  subroutine hd_read_csv( dts, fspc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(HorizonData), intent(inout) :: dts
    character(*),       intent(in)    :: fspc
    !/ -----------------------------------------------------------------------------------
    integer :: fh, ierr, idx, SOE, EOE, n, i, fcount
    character(:), allocatable :: line
     type(string_splitter)     :: SP
   !/ -----------------------------------------------------------------------------------

    fh = ReadUnit( FILE=fspc, IOSTAT=ierr )
    idx = 1
    do
       call ReadLine( fh, line, IOSTAT=ierr )
       if ( LEQ( MARK1, line(1:5) ) ) then
          SOE = idx
       end if
       if ( LEQ( MARK2, line(1:5) ) ) then
          EOE = idx
          exit
       end if
       idx = idx + 1
    end do

    close(fh)
    
    !/ -----------------------------------------------------------------------------------

    n = EOE - SOE - 1

    write(*,'(A,1X,I0)') 'SOE found at line ',SOE
    write(*,'(A,1X,I0)') 'EOE found at line ',EOE
    write(*,'(A,1X,I0,1X,A)') 'Allocate space for',n,'records'

    call dts%resize(n)

    !/ -----------------------------------------------------------------------------------
    
    fh = ReadUnit( FILE=fspc, IOSTAT=ierr )

    do i=1,SOE
       call ReadLine( fh, line, IOSTAT=ierr )
    end do

    do i=1,n
       call ReadLine( fh, line, IOSTAT=ierr )
       call split( SP, line, ',', COUNT=fcount )
       if ( 10.eq.fcount ) then
          dts%jdtdb(i)   = asReal8( SP%get(1) )
          dts%deltaT(i)  = asReal8( SP%get(3) )
          dts%state(1,i) = asReal8( SP%get(4) )
          dts%state(2,i) = asReal8( SP%get(5) )
          dts%state(3,i) = asReal8( SP%get(6) )
          dts%state(4,i) = asReal8( SP%get(7) )
          dts%state(5,i) = asReal8( SP%get(8) )
          dts%state(6,i) = asReal8( SP%get(9) )
       else
          call log_error( 'Expected 10 fields got', I4=fcount )
       end if
   end do
    
    close(fh)

  end subroutine hd_read_csv


end module horizon_data_mod
