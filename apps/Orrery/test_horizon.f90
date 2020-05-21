!/ ====================================================================== BEGIN FILE =====
!/ **                              T E S T _ H O R I Z O N                              **
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
module test_horizon
  !/ -------------------------------------------------------------------------------------
  use app_options_mod
  use configdb_mod
  use horizon_data_mod
  use string_tools
  implicit none


  !/ =====================================================================================
  type :: Object
    !/ -----------------------------------------------------------------------------------
     character(:), allocatable :: name
     real(dp)                  :: mu     = -D_ONE
     real(dp)                  :: mass   = -D_ONE
     real(dp)                  :: radius = -D_ONE
     class(System), pointer    :: sub_system => null()

   contains

     procedure :: build   => obj_build
     procedure :: sum     => obj_sum
     procedure :: display => obj_display
     
  end type Object
  
  !/ =====================================================================================
  type :: System
    !/ -----------------------------------------------------------------------------------
     !character(:), allocatable :: name
     type(Object), allocatable :: sub_list(:)

   contains

     procedure :: build => sys_build
     
  end type System

  


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  subroutine obj_build( dts, sec, dir )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Object),          intent(inout) :: dts
    type(config_section_t), intent(inout) :: sec
    character(*),           intent(in)    :: dir
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable  :: temp, path
    class(configdb_t), pointer :: cfg
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    dts%name = sec%getName()
    
    call sec%get( 'mu', VAL=temp, STATUS=ier )
    if ( 0.eq.ier ) then
       dts%mu   = asReal8( temp ) * 1.0D9
       dts%mass = dts%mu / GRAV
    end if
    
    call sec%get( 'mass', VAL=temp, STATUS=ier )
    if ( 0.eq.ier ) then
       dts%mass = asReal8( temp )
       dts%mu   = dts%mass * GRAV
    end if
    
    call sec%get( 'radius', VAL=temp, STATUS=ier )
    if ( 0.eq.ier ) then
       dts%radius = asReal8( temp )
    end if
    
    call sec%get( 'sub', VAL=temp, STATUS=ier )
    if ( 0.eq.ier ) then
       path = dir // '/' // temp
       allocate( cfg )
       call cfg%readINI( FILE=path, IOSTAT=ier )
       if ( 0.eq.ier ) then
          allocate( dts%sub_system )
          call dts%sub_system%build( cfg, dir )
       else
          print *, 'FAILED ', path
       end if
       deallocate( cfg )
    end if

  end subroutine obj_build

  
  !/ =====================================================================================
  subroutine sys_build( dts, cfg, dir )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(System),    intent(inout) :: dts
    type(configdb_t), intent(inout) :: cfg
    character(*),     intent(in)    :: dir
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    class(config_section_t), pointer :: sec
    !/ -----------------------------------------------------------------------------------
    
    n = size( cfg, COUNT='s' )

    allocate( dts%sub_list(n) )

    do i=1,n
       sec => cfg%get( i )
       call dts%sub_list(i)%build(sec,dir)
    end do

  end subroutine sys_build


  !/ =====================================================================================
  subroutine obj_sum( dts )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Object), intent(inout) :: dts
    !/ -----------------------------------------------------------------------------------
    class(System), pointer :: sys
    integer :: i, n
    real(dp) :: tmu, tmass
    !/ -----------------------------------------------------------------------------------

    if ( dts%mass.lt.D_ZERO ) then
       if ( associated( dts%sub_system ) ) then
          sys => dts%sub_system
          n = size( sys%sub_list )
          tmu   = D_ZERO
          tmass = D_ZERO
          do i=1,n
             call sys%sub_list(i)%sum
             tmu   = tmu   + sys%sub_list(i)%mu
             tmass = tmass + sys%sub_list(i)%mass
          end do
          dts%mu   = tmu
          dts%mass = tmass
       end if
    end if
 
  end subroutine obj_sum
  


  !/ =====================================================================================
  subroutine obj_display( dts )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Object), intent(inout) :: dts
    !/ -----------------------------------------------------------------------------------
    class(System), pointer :: sys
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    

    write(*,'(A12,1X,ES21.14,1X,ES21.14,1X,ES21.14)') dts%name, dts%mu, dts%mass, dts%radius

    if ( associated( dts%sub_system ) ) then
       sys => dts%sub_system
       n = size( sys%sub_list )
       do i=1,n
          call sys%sub_list(i)%display
       end do
       print *,''
    end if
    
  end subroutine obj_display
  





  
  
  !/ =====================================================================================
  subroutine testnest( root_sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: root_sec
    !/ -----------------------------------------------------------------------------------
    type(configdb_t)          :: cfg
    type(config_section_t)    :: sec
    character(:), allocatable :: hdir, hfile, fspc
    integer                   :: ierr
    type(Object) :: root
    !/ -----------------------------------------------------------------------------------

    call root_sec%get( 'hdir',  VAL=hdir  )
    call root_sec%get( 'hfile', VAL=hfile )

    fspc = hdir // '/' // hfile

    call cfg%readINI( FILE=fspc, IOSTAT=ierr )

    allocate( root%sub_system )
    call root%sub_system%build( cfg, hdir )

    root%name = 'Sol-Jupiter'


    call root%sum

    call root%display

  end subroutine testnest

  
  !/ =====================================================================================
  subroutine testhz( sec )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: hdir, hfile, fspc
    type(HorizonData) :: data

    call sec%get( 'hdir',  VAL=hdir  )
    call sec%get( 'hfile', VAL=hfile )

    fspc = hdir // '/' // hfile

    call data%read( fspc )

    print *, data%jdtdb(1), data%deltaT(1)
    print *, data%jdtdb(data%num_rec), data%deltaT(data%num_rec)

    print *, data%state(:,1)
    print *, data%state(:,data%num_rec)

  end subroutine testhz

end module test_horizon


!/ =======================================================================================
program mainx
  !/ -------------------------------------------------------------------------------------
  use test_horizon
  implicit none
  !/ -------------------------------------------------------------------------------------
  type(cli_map_t)                 :: cli
  type(configdb_t)                :: cfg
  integer                         :: ierr
  type(config_section_t), pointer :: sec
  !/ -------------------------------------------------------------------------------------

  
  call tlogger_set( CONSOLE=tlogger_info )

  call cli%add( 'dir', 'APP', 'hdir',  .true.,  '', 'directory base for data' )
  call cli%add( 'hz',  'APP', 'hfile', .true.,  '', 'name of the horizon file' )

  call AppOptions%init( cli )
  call AppOptions%setHelp( 'help' )
  call AppOptions%setOptConfigFilename( 'cfg' )
  call AppOptions%setTitleLine( 'Test Horizon Files * v1.0' )
  call AppOptions%setExampleLine( 'dir=../data/Astro/Planets-2020 hz=Solar.ini' )
  call AppOptions%addUsageText( 'Test the Horizon system.' )

  ierr = 0
  call AppOptions%getConfigDB( cfg, STATUS=ierr )

  if ( 0 .eq. ierr ) then
     sec  => cfg%get( 'APP', STATUS=ierr )
     if ( 0.eq.ierr ) then
        call testnest( sec )
     else
        call log_error( 'Section APP not found' )
     end if
  else
     call log_error( 'Configuration failed' )
  end if

end program mainx


!/ =======================================================================================
!/ **                              T E S T _ H O R I Z O N                              **
!/ =========================================================================== END FILE ==
