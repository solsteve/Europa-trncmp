!/ ====================================================================== BEGIN FILE =====
!/ **                           A P P _ O P T I O N S _ M O D                           **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module app_options_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-02-09
  !! license: GPL
  !!
  !! Provides application options. Parse commandline and generate a configuration database
  !! file. 
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use configdb_mod
  use tlogger
  private

  integer, parameter :: DEFAULT_MAX = 128

  !/ =====================================================================================
  type :: cli_map_entry_t
     !/ ----------------------------------------------------------------------------------
     character(:), allocatable :: name               !! command line option key
     character(:), allocatable :: section            !! section name
     character(:), allocatable :: cfg_key            !! configdb file key
     logical                   :: required = .false. !! is this option required
     character(:), allocatable :: default            !! default value is option is missing
     character(:), allocatable :: description        !! description for usage function
  end type cli_map_entry_t


  !/ =====================================================================================
  type :: cli_map_entry_ptr_t
     !/ ----------------------------------------------------------------------------------
     class(cli_map_entry_t), pointer :: ptr => null()
  end type cli_map_entry_ptr_t


  !/ =====================================================================================
  type, public :: cli_map_t
     !/ ----------------------------------------------------------------------------------

     type(cli_map_entry_ptr_t), allocatable :: cli_map(:)      !! array of entries
     integer                                :: max_entries = 0 !! max array length
     integer                                :: cur_entry   = 0 !! number of entries

   contains

     procedure, public  :: setMax  => set_maximum_entry_count
     procedure, public  :: add     => add_command_line_description

     final :: destroy_map

  end type cli_map_t


  !/ =====================================================================================
  type :: app_option_t
     !/ ----------------------------------------------------------------------------------

     class(cli_map_t),  pointer :: climap   => null()
     class(configdb_t), pointer :: cfgdb    => null()
     logical                    :: req_init = .true.
     logical                    :: check_for_help = .false.

     character(:), allocatable  :: base_name !! base name for config files
     character(:), allocatable  :: cfg_key   !! command line key for config file
     character(:), allocatable  :: help_key  !! command line key for help screen
     character(:), allocatable  :: prog_name !! program name

     character(:), allocatable  :: env_secname  !! environment section name
     character(:), allocatable  :: opt_secname  !! command line section name
     
   contains

     procedure, private :: alloc_instance
     procedure, private :: parse_command_line
     procedure, private :: parse_environment
     
     procedure, public  :: usage                => display_usage_page
     procedure, public  :: init                 => initialize_with_cli_map
     procedure, public  :: setConfigBase        => set_base_config_name
     procedure, public  :: setOptConfigFilename => set_config_file_key
     procedure, public  :: setHelp              => set_help_key
     procedure, public  :: getConfigDB          => get_configdb




     ! setConfigDB
     ! setAppName
     ! setConfigPath
     ! setEnvSectionName
     ! setOptSectionName
     ! setEnvConfigFilename
     ! setCommandLine
     ! addOptions

     

     final :: destroy_app_options

  end type app_option_t


  type(app_option_t), public :: AppOptions

  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: get_number_cli_entries
  end interface size

  public :: size




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  function get_number_cli_entries( map ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                        :: n
    type(cli_map_t), intent(inout) :: map
    !/ -----------------------------------------------------------------------------------


  end function get_number_cli_entries

  
  !/ =====================================================================================
  subroutine destroy_map( map )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(cli_map_t), intent(inout) :: map
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( allocated( map%cli_map ) ) then
       do i=0,map%max_entries
          if ( associated( map%cli_map(i)%ptr ) ) then
             deallocate( map%cli_map(i)%ptr )
             nullify( map%cli_map(i)%ptr )
          end if
       end do
       deallocate( map%cli_map )
    end if
    
    map%max_entries = 0
    map%cur_entry   = 0

  end subroutine destroy_map


  !/ =====================================================================================
  subroutine set_maximum_entry_count( self, n )
    !/ -----------------------------------------------------------------------------------
    !! Set a specific size for the map.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(cli_map_t), intent(inout) :: self
    integer,          intent(in)    :: n
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( n.gt.self%max_entries ) then
       allocate( self%cli_map(n) )
       call log_info( 'Allocate map', I4=n )
       do i=1,n
          self%cli_map(i)%ptr => null()
       end do
       self%max_entries = n
       self%cur_entry   = 0
    end if

  end subroutine set_maximum_entry_count


  !/ =====================================================================================
  subroutine add_command_line_description( self, name, sec, cfgkey, req, def, desc )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(cli_map_t), intent(inout) :: self
    character(*),     intent(in)    :: name   !! command line option key
    character(*),     intent(in)    :: sec    !! section name
    character(*),     intent(in)    :: cfgkey !! configdb file key
    logical,          intent(in)    :: req    !! is this option required
    character(*),     intent(in)    :: def    !! default value is option is missing
    character(*),     intent(in)    :: desc   !! description for usage function
    !/ -----------------------------------------------------------------------------------
    integer :: dummy
    !/ -----------------------------------------------------------------------------------

    call self%setMax( DEFAULT_MAX )

    self%cur_entry = self%cur_entry + 1

    if ( self%cur_entry .gt. self%max_entries ) then
       call log_warn( 'CLI MAP Add exceeds max', I4=self%cur_entry )
       call log_warn( '   use:   setMax' )
    else

       allocate( self%cli_map(self%cur_entry)%ptr )

       self%cli_map(self%cur_entry)%ptr%name        = name
       self%cli_map(self%cur_entry)%ptr%section     = sec
       self%cli_map(self%cur_entry)%ptr%cfg_key     = cfgkey
       self%cli_map(self%cur_entry)%ptr%required    = req
       self%cli_map(self%cur_entry)%ptr%default     = def
       self%cli_map(self%cur_entry)%ptr%description = desc

       if ( 0.eq.LEN_TRIM(def) ) deallocate( self%cli_map(self%cur_entry)%ptr%default )

    end if

  end subroutine add_command_line_description


  !/ =====================================================================================
  subroutine destroy_app_options( appopt )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(app_option_t), intent(inout) :: appopt !! reference to an app option
    !/ -----------------------------------------------------------------------------------
   
  end subroutine destroy_app_options


  !/ =====================================================================================
  subroutine parse_environment( self, cfg )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    use config_entry_mod
    use config_section_mod
    implicit none
    class(app_option_t), intent(inout) :: self !! reference to this app option
    type(configdb_t),    intent(inout) :: cfg  !! reference to the configdb
    !/ -----------------------------------------------------------------------------------




  end subroutine parse_environment



  !/ =====================================================================================
  subroutine parse_command_line( self, cfg )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    use config_entry_mod
    use config_section_mod
    implicit none
    class(app_option_t), intent(inout) :: self !! reference to this app option
    type(configdb_t),    intent(inout) :: cfg  !! reference to the configdb
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: csec
    type(config_entry_t)            :: entry
    character(256)                  :: temp_arg
    character(16)                   :: file_key
    character(:), allocatable       :: key, val
    integer                         :: i, arg_len, sidx, file_index, count, istat
    !/ -----------------------------------------------------------------------------------
    
    csec => null()
    
    sidx = cfg%find( self%opt_secname, STATUS=istat )
    if ( 0.eq.istat ) then
       csec => cfg%get( sidx ) 
    else
       allocate( csec )
       call csec%setName( self%opt_secname )
       call cfg%add( csec )
    end if

    count = COMMAND_ARGUMENT_COUNT()

    call GET_COMMAND_ARGUMENT( NUMBER=0, VALUE=temp_arg, LENGTH=arg_len )
    !write(*,*) 0, TRIM(ADJUSTL(temp_arg)), itest, ierr

    self%prog_name = TRIM( ADJUSTL( temp_arg ) )

    call csec%set( KEY='progname', VAL=temp_arg )

    file_index = 1
    do i=1,count
       call GET_COMMAND_ARGUMENT( NUMBER=i, VALUE=temp_arg, LENGTH=arg_len )
       !write(*,*) 'Read [',TRIM(ADJUSTL(temp_arg)),']'
       call entry%clear
       call entry%fromString( temp_arg )
       if ( entry%isKVPair() ) then
          key = entry%getKey()
          val = entry%getValue()
          !write(*,200) temp_arg 
          call csec%set( KEY=key, VAL=val )
       else
          write(file_key,100) file_index
          file_index = file_index + 1
          call csec%set( KEY=file_key, VAL=temp_arg )
          !write(*,300) file_key, temp_arg 
       end if

    end do

100 format( 'file',I0 )

200 format( 'KV Pair:    ',A )
300 format( 'File Token: ',A,' ',A )
    
  end subroutine parse_command_line


  !/ =====================================================================================
  subroutine alloc_instance( self )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self !! reference to this app option
    !/ -----------------------------------------------------------------------------------
    if ( self%req_init ) then
       self%req_init = .false.
       call log_info( 'Set default values' )
       !/ ----- set default values --------------------
       self%env_secname = 'ENV'
       self%opt_secname = 'CLI'
       
    end if
  end subroutine alloc_instance


  !/ =====================================================================================
  subroutine initialize_with_cli_map( self, climap )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t),     intent(inout) :: self   !! reference to this app option
    type(cli_map_t), target, intent(in)    :: climap !! the command line map
    !/ -----------------------------------------------------------------------------------
    call self%alloc_instance

    self%climap => climap

  end subroutine initialize_with_cli_map


  !/ =====================================================================================
  subroutine set_base_config_name( self, base_name )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self       !! reference to this app option
    character(*),        intent(in)    :: base_name  !! base name for config files
    !/ -----------------------------------------------------------------------------------
    call self%alloc_instance

    self%base_name = TRIM( ADJUSTL( base_name ) )

  end subroutine set_base_config_name


  !/ =====================================================================================
  subroutine set_config_file_key( self, cfg_key )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self     !! reference to this app option
    character(*),        intent(in)    :: cfg_key  !! command line key for config file
    !/ -----------------------------------------------------------------------------------
    call self%alloc_instance

    self%cfg_key = TRIM( ADJUSTL( cfg_key ) )

  end subroutine set_config_file_key


  !/ =====================================================================================
  subroutine set_help_key( self, help_key )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self      !! reference to this app option
    character(*),        intent(in)    :: help_key  !! command line key for help screen
    !/ -----------------------------------------------------------------------------------
    call self%alloc_instance

    self%help_key       = TRIM( ADJUSTL( help_key ) )
    self%check_for_help = .true.
    
  end subroutine set_help_key


  !/ =====================================================================================
  subroutine display_usage_page( self, PNAME )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t),    intent(inout) :: self  !! reference to this app option
    character(*), optional, intent(in)    :: PNAME !! program name
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: pn

    if ( present( PNAME ) ) then
       allocate( pn, source=TRIM( ADJUSTL( PNAME ) ) )
    else
       if ( allocated( self%prog_name ) ) then
          allocate( pn, source=self%prog_name )
       else
          pn = 'PROGRAM'
       end if
    end if

    write( ERROR_UNIT, 100 ) pn


100 format( 'USAGE: ',A,' options' )

  end subroutine display_usage_page


  !/ =====================================================================================
  subroutine get_configdb( self, cfg, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t),      intent(inout) :: self   !! reference to this app option
    type(configdb_t), target, intent(inout) :: cfg    !! reference to the configdb
    integer, optional,        intent(out)   :: STATUS !! error return
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: csec, dsec
    integer :: i, n, ierr, arg_len
    character(256) :: temp_arg
    character(:), allocatable :: dkey, dval
    !/ -----------------------------------------------------------------------------------
    
    !/ ----- check for help --------------------------------------------------------------

    arg_len = 256
    if ( self%check_for_help ) then
       n = COMMAND_ARGUMENT_COUNT()
       do i=1,n
          call GET_COMMAND_ARGUMENT( NUMBER=i, VALUE=temp_arg, LENGTH=ARG_LEN, STATUS=ierr )
          if ( LEQ( TRIM(ADJUSTL(temp_arg)), self%help_key ) ) then
             call self%usage( self%prog_name )
             stop
          end if
       end do
    end if
    
    !/ ----- add user defaults -----------------------------------------------------------

    n = self%climap%cur_entry
    do i=1,n
       if ( allocated( self%climap%cli_map(i)%ptr%default ) ) then
          dsec => cfg%get( self%climap%cli_map(i)%ptr%section )
          dkey = self%climap%cli_map(i)%ptr%cfg_key
          dval = self%climap%cli_map(i)%ptr%default
          call dsec%set( KEY=dkey, VAL=dval )
       end if
    end do

    !/ ----- load command line and environment -------------------------------------------

    call self%parse_environment( cfg )
    call self%parse_command_line( cfg )

    !/ ----- guess where config files are ------------------------------------------------


    !/ ----- check if an environment variable requests a config --------------------------

    !/ ----- check if the command line requests a config ---------------------------------


    !/ ----- use command line to override keypairs ---------------------------------------



    !/ ----- validate the config database ------------------------------------------------

    
999 continue
    self%cfgdb => cfg

    
  end subroutine get_configdb


end module app_options_mod


!/ =======================================================================================
!/ **                           A P P _ O P T I O N S _ M O D                           **
!/ =========================================================================== END FILE ==
