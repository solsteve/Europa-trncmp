!/ ====================================================================== BEGIN FILE =====
!/ **                              P L O T _ P L A N E T S                              **
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
module plot_planets
  !/ -------------------------------------------------------------------------------------
  use app_options_mod
  use configdb_mod
  use orrery_mod
  implicit none




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine process_plot( sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: data_dir, root_name, path, pname
    type(configdb_t)          :: cfg
    integer                   :: i, n
    class(config_section_t), pointer   :: psec
    !/ -----------------------------------------------------------------------------------

    call sec%get( 'data', VAL=data_dir )
    call sec%get( 'root', VAL=root_name )

    path = data_dir // '/' // root_name

    call cfg%readINI( path )

    n = size( cfg, 'section' )

    do i=1,n
       psec => cfg%get( i )
       pname = psec%getName()
       print *, pname
    end do
    
  end subroutine process_plot


end module plot_planets


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use plot_planets
  implicit none
  !/ -------------------------------------------------------------------------------------
  type(cli_map_t)                 :: cli
  type(configdb_t)                :: cfg
  integer                         :: ierr
  type(config_section_t), pointer :: sec
  !/ -------------------------------------------------------------------------------------

  call tlogger_set( CONSOLE=tlogger_info )

  call cli%add( 'ps',  'APP', 'pfile', .true.,  '',  'path to the output plot' )
  call cli%add( 'rt',  'APP', 'root',  .true.,  '',  'name of the root body file' )
  call cli%add( 't0',  'APP', 'start', .true.,  '',  'start time (JD)' )
  call cli%add( 'n',   'APP', 'num',   .true.,  '',  'number of plots' )
  call cli%add( 'dir', 'APP', 'data',  .true.,  '.', 'directory base for data' )
  call cli%add( 'tf',  'APP', 'stop',  .false., '',  'stop  time (JD)' )
  call cli%add( 'del', 'APP', 'delta', .false., '',  'delta time (JD)' )

  call AppOptions%init( cli )
  call AppOptions%setHelp( 'help' )
  call AppOptions%setOptConfigFilename( 'cfg' )
  call AppOptions%setTitleLine( 'Plot Planets * v1.0' )
  call AppOptions%setExampleLine( 'dir=../data/Orrery rt=sol.cfg ps=test.ps' )
  call AppOptions%addUsageText( 'Test the Orrery system.' )

  ierr = 0
  call AppOptions%getConfigDB( cfg, STATUS=ierr )

  if ( 0 .eq. ierr ) then
     sec  => cfg%get( 'APP', STATUS=ierr )
     if ( 0.eq.ierr ) then
        call process_plot( sec )
     else
        call log_error( 'Section APP not found' )
     end if
  else
     call log_error( 'Configuration failed' )
  end if

end program main


!/ =======================================================================================
!/ **                              P L O T _ P L A N E T S                              **
!/ =========================================================================== END FILE ==
