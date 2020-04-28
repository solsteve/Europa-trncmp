!/ ====================================================================== BEGIN FILE =====
!/ **                          F T E S T _ P L A N E T D A T A                          **
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
module ftest_planetdata
  !/ -------------------------------------------------------------------------------------
  use app_options_mod
  use configdb_mod
  use file_tools
  use astro_body_mod
  implicit none




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine process_object( dir, sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),           intent(in)    :: dir
    type(config_section_t), intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: in_file, out_file, line, temp, name
    integer                   :: i, j, n, ierr, fh, lc, count1
    type(AstroBody)           :: AB, BB
    type(string_splitter)     :: SP
    real(dp) :: mse, d, radP, radE, mass, mu, diff, avg1, avg2
    !/ -----------------------------------------------------------------------------------
    
    call sec%get( 'src',   VAL=temp )
    in_file = dir // '/' // temp
    
    call sec%get( 'table', VAL=temp )
    out_file = dir // '/' // temp

    print *, 'Process ', in_file, ' => ', out_file

    name = sec%getName() // '                  '
    
     call sec%get( 'polar', VAL=temp )
     radP = asReal8( temp )
    
     call sec%get( 'equator', VAL=temp )
    radE = asReal8( temp )
    
    call sec%get( 'mass', VAL=temp )
    mass = asReal8( temp )
    
    call sec%get( 'mu', VAL=temp )
    mu = asReal8( temp )

    ierr = 0
    
    n = LineCount( FILE=in_file, IOSTAT=ierr )

    if ( 0.eq.ierr ) then

       write( *, 100 ) n, in_file

       allocate( AB%table( 7, n ) )

       fh = ReadUnit( FILE=in_file, IOSTAT=ierr )

       if ( 0.eq.ierr ) then

          AB%name = name(1:16)
          AB%radP = radP
          AB%radE = radE
          AB%mass = mass
          AB%mu   = mu

          do i=1,n
             call ReadLine( fh, line, IOSTAT=ierr )
             call split( SP, line, ',', COUNT=lc )
             if ( 7.eq.lc ) then
                AB%table(1,i) = asReal8( SP%get(1) )
                AB%table(2,i) = asReal8( SP%get(2) ) * 1.0d3
                AB%table(3,i) = asReal8( SP%get(3) ) * 1.0d3
                AB%table(4,i) = asReal8( SP%get(4) ) * 1.0d3
                AB%table(5,i) = asReal8( SP%get(5) ) * 1.0d3
                AB%table(6,i) = asReal8( SP%get(6) ) * 1.0d3
                AB%table(7,i) = asReal8( SP%get(7) ) * 1.0d3
             else
                call log_error( 'Expected 7 fields got', I4=lc )
             end if
          end do
          close( fh )

          count1 = 0
          avg1   = 0.0d0
          do i=2,n
             avg1 = avg1 + ( AB%table(1,i) - AB%table(1,i-1) )
             count1 = count1 + 1
          end do

          avg1 = avg1 / real(count1,dp)

          avg2 = ( AB%table(1,n) - AB%table(1,1) ) / real(n-1,dp)

          print *,'Time Delta = ',avg1,avg2

          

          call AB%write( out_file )
          call BB%read( out_file )

          print *,''
          print *,'name = ', AB%name, BB%name
          print *,'radP = ', AB%radP, BB%radP, AB%radP - BB%radP
          print *,'radE = ', AB%radE, BB%radE, AB%radE - BB%radE
          print *,'mass = ', AB%mass, BB%mass, AB%mass - BB%mass
          print *,'mu   = ', AB%mu  , BB%mu  , AB%mu   - BB%mu  

          mse = D_ZERO
          
          do j=1,n
             do i=1,7
                d = AB%table(i,j) - BB%table(i,j)
                mse = mse + (d*d)
             end do
          end do
          
          print *, 'MSE = ', mse

          diff = BB%table(1,2) - BB%table(1,1)
          mse  = D_ZERO

          do i=2,n
             d = (BB%table(1,i) - BB%table(1,i-1)) - diff
             mse = mse + (d*d)
          end do

          print *, 'Time MSE = ', mse
         
       else

          call log_error( '1 Cannot open file', STR=in_file )

       end if

    else

       call log_error( '2 Cannot open file', STR=in_file )

    end if


100 format( 'Found ',I0,' lines in the file ',A )

  end subroutine process_object

  
  !/ =====================================================================================
  subroutine process_config( sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable        :: obj_dir, obj_file, path
    type(configdb_t)                 :: obj
    integer                          :: i, n, ierr
    class(config_section_t), pointer :: body
    !/ -----------------------------------------------------------------------------------

    call sec%get( 'dir', VAL=obj_dir )
    call sec%get( 'obj', VAL=obj_file )
    
    path = obj_dir // '/' // obj_file

    print *, 'attempt to read ', path

    call obj%readINI( FILE=path, IOSTAT=ierr )

    if ( 0.eq.ierr ) then
       n = size( obj, COUNT='s' )
       print *, 'number of objects', n

       do i=1,n
          body => obj%get( i )
          call process_object( obj_dir, body )
       end do
    else
       call log_error( 'Failed to read the Object config file.' )
    end if

  end subroutine process_config

  
end module ftest_planetdata


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_planetdata
  implicit none
  !/ -------------------------------------------------------------------------------------
  type(cli_map_t)                 :: cli
  type(configdb_t)                :: cfg
  integer                         :: ierr
  type(config_section_t), pointer :: sec
  !/ -------------------------------------------------------------------------------------

  call tlogger_set( CONSOLE=tlogger_info )

  call cli%add( 'dir', 'APP', 'dir', .true., '', 'path to the root directory' )
  call cli%add( 'obj', 'APP', 'obj', .true., '', 'name of the object file' )

  call AppOptions%init( cli )
  call AppOptions%setHelp( 'help' )
  call AppOptions%setTitleLine( 'Build Interpolation Tables * v1.0' )
  call AppOptions%setExampleLine( 'dir=../data/Orrery obj=orrey.cfg' )
  call AppOptions%addUsageText( 'This application will read the object file and' )
  call AppOptions%addUsageText( 'generate three fortram compatable data files' )
  call AppOptions%addUsageText( 'containing the meta data and interpolation data' )

  call AppOptions%getConfigDB( cfg, STATUS=ierr )

  if ( 0 .eq. ierr ) then
     sec  => cfg%get( 'APP', STATUS=ierr )
     if ( 0.eq.ierr ) then
        call process_config( sec )
     else
        call log_error( 'Sectionj APP not found' )
     end if
  else
     call log_error( 'Configuration failed' )
  end if

end program main


!/ =======================================================================================
!/ **                          F T E S T _ P L A N E T D A T A                          **
!/ =========================================================================== END FILE ==
