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
  use string_tools
  private

  integer, parameter :: DEFAULT_MAX = 128
  character(12), parameter, public :: APP_OPT_FILENAME = 'WXYZFSPC1234'

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
     integer                                :: cur_file    = 0 !! number of entries

   contains

     procedure, public  :: setMax  => set_maximum_entry_count
     procedure, public  :: add     => add_command_line_description

     final :: destroy_map

  end type cli_map_t


  !/ =====================================================================================
  type :: app_option_t
     !/ ----------------------------------------------------------------------------------

     class(cli_map_t),  pointer :: climap         => null()
     class(configdb_t), pointer :: cfgdb          => null()
     logical                    :: req_init       = .true.
     logical                    :: check_for_help = .false.

     character(:), allocatable  :: base_name    !! base name for config files
     character(:), allocatable  :: cfg_key      !! command line key for config file
     character(:), allocatable  :: env_cfg_key  !! environment key for config file
     character(:), allocatable  :: help_key     !! command line key for help screen
     character(:), allocatable  :: prog_name    !! program name

     character(:), allocatable  :: env_secname  !! environment section name
     character(:), allocatable  :: opt_secname  !! command line section name
     
     character(:), allocatable  :: title        !! application title
     character(:), allocatable  :: path         !! search path

   contains

     procedure, private :: alloc_instance
     procedure, public  :: init                 => initialize_with_cli_map

     procedure, public  :: setTitleLine         => set_title_line
     procedure, public  :: setConfigBase        => set_base_config_name
     procedure, public  :: setConfigPath        => set_config_path
     procedure, public  :: setEnvSectionName    => set_env_section_name
     procedure, public  :: setOptSectionName    => set_config_section_name
     procedure, public  :: setOptConfigFilename => set_config_file_key
     procedure, public  :: setEnvConfigFilename => set_env_config_file_key
     procedure, public  :: setHelp              => set_help_key

     procedure, public  :: usage                => display_usage_page
   ! procedure, public  :: setUsageFunction     => set_usage_function

     procedure, private :: parse_command_line
     procedure, private :: parse_environment
   ! procedure, public  :: setCommandLine       => set_command_line
   ! procedure, public  :: addOptions           => add_options

     procedure, public  :: getConfigDB          => get_configdb
   ! procedure, public  :: setConfigDB          => set_configdb

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
    !! Return the number of entries in the CLI map.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                        :: n   !! return the number of entries.
    type(cli_map_t), intent(inout) :: map !! reference to a CLI map.
    !/ -----------------------------------------------------------------------------------

    n = map%cur_entry
    
  end function get_number_cli_entries

  
  !/ =====================================================================================
  subroutine destroy_map( map )
    !/ -----------------------------------------------------------------------------------
    !! Destructor
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(cli_map_t), intent(inout) :: map !! reference to a CLI map.
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
    class(cli_map_t), intent(inout) :: self !! reference to this CLI map.
    integer,          intent(in)    :: n    !! desired entry capacity
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( n.gt.self%max_entries ) then
       allocate( self%cli_map(n) )
       call log_debug( 'Allocate map', I4=n )
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
    !! Add a command reference.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(cli_map_t), intent(inout) :: self   !! reference to this CLI map.
    character(*),     intent(in)    :: name   !! command line option key
    character(*),     intent(in)    :: sec    !! section name
    character(*),     intent(in)    :: cfgkey !! configdb file key
    logical,          intent(in)    :: req    !! is this option required
    character(*),     intent(in)    :: def    !! default value is option is missing
    character(*),     intent(in)    :: desc   !! description for usage function
    !/ -----------------------------------------------------------------------------------
    integer       :: dummy
    character(16) :: FSPC
    !/ -----------------------------------------------------------------------------------

    call self%setMax( DEFAULT_MAX )

    self%cur_entry = self%cur_entry + 1

    if ( self%cur_entry .gt. self%max_entries ) then
       call log_warn( 'CLI MAP Add exceeds max', I4=self%cur_entry )
       call log_warn( '   use:   setMax' )
    else

       allocate( self%cli_map(self%cur_entry)%ptr )

       if ( LEQ( APP_OPT_FILENAME, name ) ) then
          self%cur_file = self%cur_file + 1
          write(FSPC,100) self%cur_file
100       format('file',I0)
          self%cli_map(self%cur_entry)%ptr%name = TRIM(ADJUSTL(FSPC))
       else
          self%cli_map(self%cur_entry)%ptr%name = name
       end if

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
    !! Destructor.
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
    !! Parse the command line for key=value pairs. When just a value appears on the line
    !! it will be interpreted as a file and given sequential keys: file1=, file2=,...
    !/ -----------------------------------------------------------------------------------
    use config_entry_mod
    use config_section_mod
    implicit none
    class(app_option_t), intent(inout) :: self !! reference to this app option
    type(configdb_t),    intent(inout) :: cfg  !! reference to the configdb
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: csec
    type(config_entry_t)             :: entry
    character(256)                   :: temp_arg
    character(16)                    :: file_key
    character(:), allocatable        :: key, val
    integer                          :: i, arg_len, sidx, file_index, count, istat
    !/ -----------------------------------------------------------------------------------

    csec => null()

    sidx = cfg%find( self%opt_secname, STATUS=istat )

    if ( 0.ne.istat ) then
       allocate( csec )
       call csec%setName( self%opt_secname )
       call cfg%add( csec )
       deallocate( csec )  ! This is because add==merge and merge==copy
    end if

    csec => cfg%get( self%opt_secname, STATUS=istat )
    
    count = COMMAND_ARGUMENT_COUNT()

    call GET_COMMAND_ARGUMENT( NUMBER=0, VALUE=temp_arg, LENGTH=arg_len )

    self%prog_name = TRIM( ADJUSTL( temp_arg ) )

    call csec%set( KEY='progname', VAL=temp_arg, STATUS=istat )

    file_index = 1
    do i=1,count
       call GET_COMMAND_ARGUMENT( NUMBER=i, VALUE=temp_arg, LENGTH=arg_len )

       call entry%clear
       call entry%fromString( temp_arg )
       if ( entry%isKVPair() ) then
          key = entry%getKey()
          val = entry%getValue()
          call csec%set( KEY=key, VAL=val )
       else
          write(file_key,100) file_index
          file_index = file_index + 1
          call csec%set( KEY=file_key, VAL=temp_arg )
       end if

    end do

100 format( 'file',I0 )
    
  end subroutine parse_command_line


  !/ =====================================================================================
  subroutine alloc_instance( self )
    !/ -----------------------------------------------------------------------------------
    !! Initialization.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self !! reference to this app option
    !/ -----------------------------------------------------------------------------------
    if ( self%req_init ) then
       self%req_init = .false.
       call log_info( 'Set default values' )
       
       !/ ----- set default values --------------------
       self%base_name   = 'configdb'
       self%env_secname = 'ENV'
       self%opt_secname = 'CLI'
       self%prog_name   = 'Application'
       self%title       = 'Application * ver 1.0 * 20xx'
       self%path        = '/etc:~:.'
    end if
  end subroutine alloc_instance


  !/ =====================================================================================
  subroutine initialize_with_cli_map( self, climap )
    !/ -----------------------------------------------------------------------------------
    !! Initialize with user supplied CLI map.
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
    !! Set the base config file name.
    !!
    !! three directories will be searched: /etc, ~, and .
    !! two file names will be derived:   .base_name and base_name.cfg
    !! this creates 6 possible config files.
    !! the order is /etc/.base /etc/base.cfg ~/.base ~/base.cfg ./.base ./base.cfg
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
    !! This is the optional command line key that points to a config file.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self     !! reference to this app option
    character(*),        intent(in)    :: cfg_key  !! command line key for config file
    !/ -----------------------------------------------------------------------------------
    call self%alloc_instance

    self%cfg_key = TRIM( ADJUSTL( cfg_key ) )

  end subroutine set_config_file_key


  !/ =====================================================================================
  subroutine set_env_config_file_key( self, env_cfg_key )
    !/ -----------------------------------------------------------------------------------
    !! This is the optional environment variable key that points to a config file.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self         !! reference to this app option
    character(*),        intent(in)    :: env_cfg_key  !! environment key for config file
    !/ -----------------------------------------------------------------------------------
    call self%alloc_instance

    self%env_cfg_key = TRIM( ADJUSTL( env_cfg_key ) )

  end subroutine set_env_config_file_key


  !/ =====================================================================================
  subroutine set_help_key( self, help_key )
    !/ -----------------------------------------------------------------------------------
    !! This is the command line word that will activate the usage page and abort.
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
  subroutine set_title_line( self, title )
    !/ -----------------------------------------------------------------------------------
    !! Set the title line above the usage statement.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self      !! reference to this app option
    character(*),        intent(in)    :: title     !! application title
    !/ -----------------------------------------------------------------------------------

    call self%alloc_instance
    self%title = TRIM( ADJUSTL( title ) )

  end subroutine set_title_line


  !/ =====================================================================================
  subroutine set_config_path( self, path )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self      !! reference to this app option
    character(*),        intent(in)    :: path      !! search path for config
    !/ -----------------------------------------------------------------------------------

    call self%alloc_instance
    self%path = TRIM( ADJUSTL( path ) )

  end subroutine set_config_path


  !/ =====================================================================================
  subroutine set_env_section_name( self, sname )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self      !! reference to this app option
    character(*),        intent(in)    :: sname
    !/ -----------------------------------------------------------------------------------

    call self%alloc_instance
    self%env_secname  = TRIM( ADJUSTL( sname ) )

  end subroutine set_env_section_name


  !/ =====================================================================================
  subroutine set_config_section_name( self, cname )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t), intent(inout) :: self      !! reference to this app option
    character(*),        intent(in)    :: cname
    !/ -----------------------------------------------------------------------------------

    call self%alloc_instance
    self%opt_secname  = TRIM( ADJUSTL( cname ) )

  end subroutine set_config_section_name













  
  !/ =====================================================================================
  subroutine display_usage_page( self, PNAME )
    !/ -----------------------------------------------------------------------------------
    !! Display the aoutomatic usage page.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t),    intent(inout) :: self  !! reference to this app option
    character(*), optional, intent(in)    :: PNAME !! program name
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: pn
    integer                   :: i, n, nOpt, nReq
    !/ -----------------------------------------------------------------------------------

    write( ERROR_UNIT, * ) self%title
    
    if ( present( PNAME ) ) then
       allocate( pn, source=TRIM( ADJUSTL( PNAME ) ) )
    else
       if ( allocated( self%prog_name ) ) then
          allocate( pn, source=self%prog_name )
       else
          pn = 'PROGRAM'
       end if
    end if

    write( ERROR_UNIT, * )
    write( ERROR_UNIT, 100 ) pn

    !/ ----- count required and optional -------------------------------------------------

    nReq = 0
    nOpt = 0
    n = self%climap%cur_entry
    do i=1,n
       if ( self%climap%cli_map(i)%ptr%required ) then
          nReq = nReq + 1
       else
          nOpt = nOpt + 1
       end if
    end do

    !/ ----- display required ------------------------------------------------------------

    if ( 0.lt.nReq ) then
       write( ERROR_UNIT, * )
       write( ERROR_UNIT, * ) '  Required:'
       do i=1,n
          if ( self%climap%cli_map(i)%ptr%required ) then
             write( ERROR_UNIT, 200 ) self%climap%cli_map(i)%ptr%name, &
                  &                   self%climap%cli_map(i)%ptr%description
          end if
       end do
    end if

    !/ ----- display optional ------------------------------------------------------------

    if ( 0.lt.nReq ) then
       write( ERROR_UNIT, * )
       write( ERROR_UNIT, * ) '  Optional:'
       do i=1,n
          if ( .not.self%climap%cli_map(i)%ptr%required ) then
             write( ERROR_UNIT, 200 ) self%climap%cli_map(i)%ptr%name, &
                  &                   self%climap%cli_map(i)%ptr%description
          end if
       end do
    end if

    write( ERROR_UNIT, * )

100 format( 'USAGE: ',A,' options' )
200 format( '     ',A,' - ',A )

    stop

  end subroutine display_usage_page


  !/ =====================================================================================
  subroutine get_configdb( self, cfg, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Compile and return the ConfigDB for this application.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(app_option_t),      intent(inout) :: self   !! reference to this app option
    type(configdb_t), target, intent(inout) :: cfg    !! reference to the configdb
    integer, optional,        intent(out)   :: STATUS !! error return
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: csec, dsec, cli
    integer                          :: i, j, n, m, ierr, arg_len, idx, fn
    character(256)                   :: temp_arg
    character(:), allocatable        :: ckey, dkey, cval, dval, cfg_file, sname, temp_path
    type(configDB_t)                 :: temp_cfg
    type(config_entry_t)             :: ent
    logical                          :: validated
    !/ -----------------------------------------------------------------------------------

    type(string_splitter) :: path_splitter
    
    character(20), parameter, dimension(8) :: cfg_fmt = [ &
         &    "(A,'/',A)           ",                     &
         &    "(A,'/',A,'.ini')    ",                     &
         &    "(A,'/',A,'.cfg')    ",                     &
         &    "(A,'/',A,'.config') ",                     &
         &    "(A,'/.',A)          ",                     &
         &    "(A,'/.',A,'.ini')   ",                     &
         &    "(A,'/.',A,'.cfg')   ",                     &
         &    "(A,'/.',A,'.config')" ]

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
    
    !/ ----- add user sections -----------------------------------------------------------

    n = self%climap%cur_entry
    do i=1,n
       sname = self%climap%cli_map(i)%ptr%section
       idx = cfg%find( sname, STATUS=ierr )
       if ( 0.eq.idx ) then
          call cfg%add( sname )
          call log_debug( 'User section added', STR=sname )
       end if
    end do
    
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

    cli => cfg%get( self%opt_secname, STATUS=ierr )
    if ( ierr.ne.0 ) then
       call log_critical( 'app_options%getConfigDB: The command line was not parsed' )
    end if

    !/ ----- guess where config files are ------------------------------------------------

    call split( path_splitter, self%path, ':' )
    n = path_splitter%count()
    m = size( cfg_fmt )
    do j=1,n
       temp_path = path_splitter%get(j)
       do i=1,m
          write( temp_arg, TRIM(cfg_fmt(i)) ) &
               & temp_path, TRIM(ADJUSTL(self%base_name))
          cfg_file = TRIM(ADJUSTL(temp_arg))
          call log_debug('Look for config file', STR=cfg_file )
          call temp_cfg%delete( STATUS=ierr )
          call temp_cfg%deleteComment( STATUS=ierr )
          call temp_cfg%readINI( FILE=cfg_file, IOSTAT=ierr )
          if ( 0.eq.ierr ) then
             call log_info('Located config file', STR=cfg_file )
             call cfg%merge( temp_cfg )   !! Merge the configuration file
          end if
       end do
    end do

    !/ ----- check if an environment variable requests a config --------------------------

    if ( allocated( self%env_cfg_key ) ) then
       call log_debug( 'Check the Environment for config key:', STR=self%env_cfg_key )

       call GET_ENVIRONMENT_VARIABLE( NAME=self%env_cfg_key, VALUE=temp_arg, STATUS=ierr )

       if ( 0.eq.ierr ) then
          cfg_file = TRIM(ADJUSTL(temp_arg))

          call log_debug( 'Config file was specified in the environment', STR=cfg_file )

          call temp_cfg%delete( STATUS=ierr )
          call temp_cfg%deleteComment( STATUS=ierr )

          call temp_cfg%readINI( FILE=cfg_file, IOSTAT=ierr )
          if ( 0.eq.ierr ) then
             call cfg%merge( temp_cfg )   !! Merge the configuration file
          else
             call log_warn( 'Config file could not be read', STR=cfg_file )
          end if
       else
          if ( 2.eq.ierr ) then
             call log_critical( 'This processor does not support environment variables' )
          end if
          call log_debug( 'the config key was not used' )
       end if
    else
       call log_debug( 'No config file key was setup for the CLI' )
    end if
     
    !/ ----- check if the command line requests a config ---------------------------------

    if ( allocated( self%cfg_key ) ) then
       call log_debug( 'Check the CLI for config key:', STR=self%cfg_key )
       idx = cli%find( self%cfg_key )
       if ( 0.lt.idx ) then
          call cli%get( self%cfg_key, VAL=cfg_file )
          call log_debug( 'Config file was specified on the command line', STR=cfg_file )

          call temp_cfg%delete( STATUS=ierr )
          call temp_cfg%deleteComment( STATUS=ierr )

          call temp_cfg%readINI( FILE=cfg_file, IOSTAT=ierr )
          if ( 0.eq.ierr ) then
             call cfg%merge( temp_cfg )   !! Merge the configuration file
          else
             call log_warn( 'Config file could not be read', STR=cfg_file )
          end if
       else
           call log_debug( 'the config key was not used' )
       end if
    else
       call log_debug( 'No config file key was setup for the CLI' )
    end if
    
    !/ ----- use command line to override keypairs ---------------------------------------

    !  call cli%add( 'if', 'APP', 'infile',  .true.,  '', 'path to an input  file' )

    n = self%climap%cur_entry
    do i=1,n
       ckey   = self%climap%cli_map(i)%ptr%name
       dkey   = self%climap%cli_map(i)%ptr%cfg_key
       dval   = self%climap%cli_map(i)%ptr%default

       dsec => cfg%get( self%climap%cli_map(i)%ptr%section )
       call cli%get( ckey, VAL=cval, STATUS=ierr )
       if ( 0.eq.ierr ) then
          call dsec%set( dkey, VAL=cval )
       else
          if ( .not.LEQ( '', dval ) ) then
             call dsec%set( dkey, VAL=dval )
          end if
       end if
    end do

    !/ ----- validate the config database ------------------------------------------------

    validated = .true.
    n = self%climap%cur_entry
    do i=1,n
       if ( self%climap%cli_map(i)%ptr%required ) then
          sname = self%climap%cli_map(i)%ptr%section
          dsec => cfg%get( sname )
          dkey = self%climap%cli_map(i)%ptr%cfg_key
          call dsec%get( dkey, STATUS=ierr )
          if ( 0.ne.ierr ) then
             ckey = self%climap%cli_map(i)%ptr%name
             write( temp_arg, 1000 ) ckey, sname, dkey
             call log_warn( 'required key not found', STR=TRIM(ADJUSTL(temp_arg)) )
             validated = .false.
          end if
       end if
    end do

    if ( .not.validated ) then
       call self%usage
    end if
        
    !/ -----------------------------------------------------------------------------------
    
    self%cfgdb => cfg

1000 format( 'CLI ',A,'= or Config ',A,'.',A )
    
  end subroutine get_configdb


end module app_options_mod


!/ =======================================================================================
!/ **                           A P P _ O P T I O N S _ M O D                           **
!/ =========================================================================== END FILE ==
