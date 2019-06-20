!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ A P P O P T S                             **
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

!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use app_options_mod
  use configdb_mod

  type(cli_map_t)  :: cli
  type(configdb_t) :: cfg
  type(config_section_t), pointer :: sec
  integer          :: ierr
  character(:), allocatable :: nstr
  
  !/ -------------------------------------------------------------------------------------

<<<<<<< HEAD
  call cli%add( 'if', 'APP', 'infile',  .true.,  'input.dat', 'path to an input  file' )
  call cli%add( 'of', 'APP', 'outfile', .true.,  'output.dat', 'path to an output file' )
  call cli%add( 'n',  'APP', 'count',   .false., '', 'number of records to process' )
=======
  call tlogger_set( CONSOLE=tlogger_debug )
  
  call cli%add( APP_OPT_FILENAME, 'APP', 'flag',    .true.,  '',  'operation: E, D, or G' )
  call cli%add( 'if',             'APP', 'infile',  .true.,  '',  'path to an input  file' )
  call cli%add( 'of',             'APP', 'outfile', .true.,  '',  'path to an output file' )
  call cli%add( 'n',              'APP', 'count',   .false., '9', 'number of records to process' )
>>>>>>> Added experiment in solving Kepler's equation

  call AppOptions%init( cli )
  call AppOptions%setConfigBase( 'ftest' )
  call AppOptions%setConfigPath( '~:..' )
  call AppOptions%setEnvSectionName( 'NewEnv' )
  call AppOptions%setOptSectionName( 'NewOPts' )
  call AppOptions%setEnvConfigFilename( 'ECFG' )
  call AppOptions%setOptConfigFilename( 'cfg' )
  call AppOptions%setHelp( 'help' )
  call AppOptions%getConfigDB( cfg, STATUS=ierr )

  if ( 0 .eq. ierr ) then
<<<<<<< HEAD
     write( ERROR_UNIT, * ) 'Success'
=======
     sec  => cfg%get( 'APP', STATUS=ierr )
     if ( 0.eq.ierr ) then
        call sec%get( 'count', VAL=nstr, STATUS=ierr )
        if ( 0.eq.ierr ) then
           write(*,*) 'count =', nstr
        else
           write(*,*) 'Key "count" not found'
        end if
     else
        write(*,*) 'Section "APP" not found'
     end if
>>>>>>> Added experiment in solving Kepler's equation
  else
     write( ERROR_UNIT, * ) 'Configuration failed'
  end if

  call cfg%writeINI( UNIT=6, IOSTAT=ierr )

  if ( 0 .eq. ierr ) then
     write( ERROR_UNIT, * ) 'Success'
  else
     write( ERROR_UNIT, * ) 'WriteINI failed'
  end if

end program main

!/ =======================================================================================
!/ **                             F T E S T _ A P P O P T S                             **
!/ =========================================================================== END FILE ==
