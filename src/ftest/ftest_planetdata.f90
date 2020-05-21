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
  use string_tools
  use astro_body_mod
  use interpolate_mod
  use statistics_mod
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


  !/ =====================================================================================
  subroutine verify_config( sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: obj_dir, crs_table, fin_table, fin_file, crs_file, fstr
    type(AstroBody)           :: fine, course
    type(Bug5)                :: bugX, bugY, bugZ
    real(dp) :: deltaTime, fr, fx, fy, fz, tr, tx, ty, tz, diff
    integer  :: idx, i, n, fp1, fp2
    logical  :: eot
    real(dp), allocatable :: X(:), Y(:), Z(:), T(:)
    type(running_stats) :: RS
    !/ -----------------------------------------------------------------------------------
   
    call sec%get( 'dir',    VAL=obj_dir  )
    call sec%get( 'fine',   VAL=fin_file )
    call sec%get( 'course', VAL=crs_file )

    fin_table = obj_dir // '/' // fin_file
    crs_table = obj_dir // '/' // crs_file

    call fine%read( fin_table )
    call course%read( crs_table )

    deltaTime = fine%delta

    n = course%num_entry

    allocate( T(n) )
    allocate( X(n) )
    allocate( Y(n) )
    allocate( Z(n) )

    do i=1,n
       T(i) = course%table(1,i)
       X(i) = course%table(2,i)
       Y(i) = course%table(3,i)
       Z(i) = course%table(4,i)
    end do

    call bugX%build( COPYX=T, COPYY=X, DELTA=deltaTime, X1=fine%table(1,14) )
    call bugY%build( COPYX=T, COPYY=Y, DELTA=deltaTime, X1=fine%table(1,14) )
    call bugZ%build( COPYX=T, COPYY=Z, DELTA=deltaTime, X1=fine%table(1,14) )

    print *, '---X---'
    call bugX%show()
    

    print *, '---Y---'
    call bugY%show()

    print *, '---Z---'
    call bugZ%show()

    idx = 14

    print *,'---course---'
    print *,course%table(1,2),course%table(2,2),course%table(3,2),course%table(4,2)

    fp1 = WriteUnit( FILE='test-r.dat' )
    fp2 = WriteUnit( FILE='test-5.dat' )

    call RS%reset
    
10  continue
    
    fx = fine%table( 2, idx )
    fy = fine%table( 3, idx )
    fz = fine%table( 4, idx )

    fr = sqrt( (fx*fx) + (fy*fy) + (fz*fz) )

    tx = bugX%get( ATEND=eot )
    ty = bugY%get( ATEND=eot )
    tz = bugZ%get( ATEND=eot )
    
    tr = sqrt( (tx*tx) + (ty*ty) + (tz*tz) )

    if ( eot ) goto 999
    diff = (fr-tr)
    call RS%sample(diff)
    print *, idx, fine%table(1,idx), fr, tr, diff

    write(fp1,*) fine%table(1,idx), fr
    write(fp2,*) fine%table(1,idx), tr
    
    idx = idx + 1
    !if ( 20.lt.idx ) goto 999
    goto 10

999 continue

    close( fp1 )
    close( fp2 )

    call RS%report(UNIT=OUTPUT_UNIT)

    deallocate( T )
    deallocate( X )
    deallocate( Y )
    deallocate( Z )

100 format( 'Read ',I0,' records from ',A )
200 format( I0,2X,F15.6,6X,G0,2X,G0,4X,G0 )

  end subroutine verify_config


  !/ =====================================================================================
  subroutine CONVERT
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(cli_map_t)                 :: cli
    type(configdb_t)                :: cfg
    integer                         :: ierr
    type(config_section_t), pointer :: sec
    !/ -----------------------------------------------------------------------------------

    call tlogger_set( CONSOLE=tlogger_debug )

    call cli%add( 'dir', 'APP', 'dir', .true., '', 'path to the root directory' )
    call cli%add( 'obj', 'APP', 'obj', .true., '', 'name of the object file' )

    call AppOptions%init( cli )
    call AppOptions%setHelp( 'help' )
    call AppOptions%setOptConfigFilename( 'cfg' )
    call AppOptions%setTitleLine( 'Build Interpolation Tables * v1.0' )
    call AppOptions%setExampleLine( 'dir=../data/Orrery obj=orrery.cfg' )
    call AppOptions%addUsageText( 'This application will read the object file and' )
    call AppOptions%addUsageText( 'generate three fortran compatable data files' )
    call AppOptions%addUsageText( 'containing the meta data and interpolation data' )

    ierr = 0
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

  
  end subroutine CONVERT


  !/ =====================================================================================
  subroutine TEST_INTERPOLATE
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(cli_map_t)                 :: cli
    type(configdb_t)                :: cfg
    integer                         :: ierr
    type(config_section_t), pointer :: sec
    !/ -----------------------------------------------------------------------------------

    call tlogger_set( CONSOLE=tlogger_info )

    call cli%add( 'dir',    'APP', 'dir',    .true., '',  'path to the root directory' )
    call cli%add( 'course', 'APP', 'course', .true., '',  'name of the course grain table' )
    call cli%add( 'fine',   'APP', 'fine',   .true., '',  'name of the fine gain table' )

    call AppOptions%init( cli )
    call AppOptions%setHelp( 'help' )
    call AppOptions%setOptConfigFilename( 'cfg' )
    call AppOptions%setTitleLine( 'Verify Interpolation Tables * v1.0' )
    call AppOptions%setExampleLine( 'dir=../data/Orrery course=Luna-Test-01H.data fine=Luna-Test-10M.data' )
    call AppOptions%addUsageText( 'This application will read the course and fine grain' )
    call AppOptions%addUsageText( 'data tables. It will interpolate from the course grain' )
    call AppOptions%addUsageText( 'and the fine grain spacing and then compare the two.' )

    ierr = 0
    call AppOptions%getConfigDB( cfg, STATUS=ierr )

    if ( 0 .eq. ierr ) then
       sec  => cfg%get( 'APP', STATUS=ierr )
       if ( 0.eq.ierr ) then
          call verify_config( sec )
       else
          call log_error( 'Sectionj APP not found' )
       end if
    else
       call log_error( 'Configuration failed' )
    end if

  
  end subroutine TEST_INTERPOLATE

  
end module ftest_planetdata


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_planetdata
  implicit none
  call TEST_INTERPOLATE
  
end program main


!/ =======================================================================================
!/ **                          F T E S T _ P L A N E T D A T A                          **
!/ =========================================================================== END FILE ==
