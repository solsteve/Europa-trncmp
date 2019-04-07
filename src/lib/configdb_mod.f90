!/ ====================================================================== BEGIN FILE =====
!/ **                              C O N F I G D B _ M O D                              **
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
module configdb_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-13
  !! license: GPL
  !!
  !! Provides a list of named sections
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use config_entry_mod
  use config_section_mod

  integer, parameter :: ADDITIONAL_PADDING = 16

  !/ =====================================================================================
  type :: section_pointer
     !/ ----------------------------------------------------------------------------------
     class(config_section_t), pointer :: ptr => null()
  end type section_pointer


  !/ =====================================================================================
  type :: comment_pointer
     !/ ----------------------------------------------------------------------------------
     character(:), allocatable :: str
  end type comment_pointer


  !/ =====================================================================================
  type, public :: configdb_t
     !/ ----------------------------------------------------------------------------------

     class(section_pointer), private, pointer :: sections(:) => null()
     class(comment_pointer), private, pointer :: comments(:) => null()

     integer, private :: max_section     = 0 !! maximum slots for sections
     integer, private :: max_comment     = 0 !! maximum slots for comments

     integer, private :: current_section = 0 !! last added section
     integer, private :: current_comment = 0 !! last added comment

     integer, private :: sindex = 1 !! section iterator
     integer, private :: cindex = 1 !! comment iterator

   contains

     !/ private bound procedures

     procedure :: grow_section_list
     procedure :: grow_comment_list
     
     procedure :: get_section_by_index
     procedure :: get_section_by_name

     procedure :: add_section_to_list
     procedure :: add_new_section_to_list

     !/ public bound procedures

     procedure, public :: delete         => delete_sections
     procedure, public :: deleteComment  => delete_comments
     procedure, public :: find           => find_index_by_name
     procedure, public :: getComment     => get_comment_by_index
     procedure, public :: addComment     => add_comment_to_list

     procedure, public :: readINI        => read_ini
     procedure, public :: writeINI       => write_ini

     procedure, public :: rewind         => rewind_section
     procedure, public :: hasNext        => has_next_section
     procedure, public :: next           => get_next_section

     procedure, public :: merge          => merge_configdb
     
     procedure, public :: rewindComment  => rewind_comment
     procedure, public :: hasNextComment => has_next_comment
     procedure, public :: nextComment    => get_next_comment
     
     generic,   public :: get            => get_section_by_index, get_section_by_name
     generic,   public :: add            => add_section_to_list, add_new_section_to_list

     final :: destroy_section_list

  end type configdb_t


  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: get_number_of_records
  end interface size

  public size




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine destroy_section_list( cdb )
    !/ -----------------------------------------------------------------------------------
    !! Clear the data from all of the section and comment records.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(configdb_t), intent(inout) :: cdb !! reference to this configdb.
    !/ -----------------------------------------------------------------------------------

    call cdb%delete
    call cdb%deleteComment

  end subroutine destroy_section_list


  !/ =====================================================================================
  function count_sections( cdb ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Count the number of sections that are not NULL
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                         :: n   !! number of entries stored.
    type(configdb_t), intent(inout) :: cdb !! reference to a configdb.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    n = 0
    do i=1,cdb%current_section
       if ( associated( cdb%sections(i)%ptr ) ) then
          n = n + 1
       end if
    end do

  end function count_sections


  !/ =====================================================================================
  function count_comments( cdb ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Count the number of commnets that are not NULL
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                         :: n   !! number of entries stored.
    type(configdb_t), intent(inout) :: cdb !! reference to a configdb.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    n = 0
    do i=1,cdb%current_section
       if ( allocated( cdb%comments(i)%str ) ) then
          if ( 0.lt.LEN(cdb%comments(i)%str) ) then
             n = n + 1
          end if
       end if
    end do

  end function count_comments


  !/ =====================================================================================
  function get_number_of_records( cdb, COUNT ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of records. SECTION=number of sections, COMMENT=number of records.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                               :: n     !! 
    type(configdb_t),       intent(inout) :: cdb   !! reference to a configdb.
    character(1), optional, intent(in)    :: COUNT !! type of count
    !/ -----------------------------------------------------------------------------------

    n = 0

    if ( present( COUNT ) ) then
       if ( ( 'S'.eq.COUNT ).or.( 's'.eq.COUNT ) ) then
          n = count_sections(cdb)
          goto 999
       end if
       if ( ( 'C'.eq.COUNT ).or.( 'c'.eq.COUNT ) ) then
          n = count_comments(cdb)
          goto 999
       end if
       call log_error( "size(COUNT=) should be 'NUMBER' or 'COMMENT'" )
    end if

999 continue
  end function get_number_of_records


  !/ =====================================================================================
  subroutine delete_sections( self, INDEX, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Delete section records.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !! |    2   |  section array null   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    integer, optional, intent(in)    :: INDEX  !! index.
    integer, optional, intent(out)   :: STATUS !! return condition.
    !/ -----------------------------------------------------------------------------------
    integer :: ier, i
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    !/ -----------------------------------------------------------------------------------

    if ( associated( self%sections ) ) then

       if ( present( INDEX ) ) then !/ ----- delete one section --------------------------
          if ( index.gt.self%max_section ) then
             ier = 1
             if ( report ) then
                call log_warn( 'Configdb%deleteSection: index out of bounds' )
             end if
          else
             if ( associated( self%sections(index)%ptr ) ) then
                deallocate( self%sections(index)%ptr )
                nullify( self%sections(index)%ptr )
             end if
          end if

       else !/ ----- delete all the sections ---------------------------------------------

          do i=1,self%max_section
             if ( associated( self%sections(i)%ptr ) ) then
                deallocate( self%sections(i)%ptr )
                nullify( self%sections(i)%ptr )
             end if
          end do

       end if

    else
       ier = 2
       if ( report ) then
          call log_warn( 'Configdb%deleteSection: section array not allocated' )
       end if
    end if

    !/ -----------------------------------------------------------------------------------

    if ( present( STATUS ) ) STATUS = ier

  end subroutine delete_sections


  !/ =====================================================================================
  subroutine delete_comments( self, INDEX, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Delete comment records.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !! |    2   |  comment array null   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    integer, optional, intent(in)    :: INDEX  !! index.
    integer, optional, intent(out)   :: STATUS !! return condition.
    !/ -----------------------------------------------------------------------------------
    integer :: ier, i
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    !/ -----------------------------------------------------------------------------------

    if ( associated( self%comments ) ) then

       if ( present( INDEX ) ) then !/ ----- delete one comment --------------------------
          if ( index.gt.self%max_comment ) then
             ier = 1
             if ( report ) then
                call log_warn( 'Configdb%deleteComment: index out of bounds' )
             end if
          else
             if ( allocated( self%comments(index)%str ) ) then
                deallocate( self%comments(index)%str )
             end if
          end if

       else !/ ----- delete all the comments ---------------------------------------------

          do i=1,self%max_section
             if ( allocated( self%comments(i)%str ) ) then
                deallocate( self%comments(i)%str )
             end if
          end do

       end if

    else
       ier = 2
       if ( report ) then
          call log_warn( 'Configdb%deleteComment: comment array not allocated' )
       end if
    end if

    !/ -----------------------------------------------------------------------------------

    if ( present( STATUS ) ) STATUS = ier

  end subroutine delete_comments


  !/ =====================================================================================
  subroutine grow_section_list( self, new_max )
    !/ -----------------------------------------------------------------------------------
    !! Grow the size of the section array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self    !! reference to this configdb.
    integer,           intent(in)    :: new_max !! new max size.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    class(section_pointer), pointer :: temp(:) => null()
    !/ -----------------------------------------------------------------------------------

    n = new_max
    if ( n.lt.1 ) then
       n = ADDITIONAL_PADDING
       call log_debug( 'ConfigDB:growSections: Added padding', I4=n )
    end if

    if ( associated( self%sections ) ) then
       temp          => self%sections
       self%sections => null()
       call log_debug( 'ConfigDB:growSections: Grow to', I4=n )
       allocate( self%sections(n) )
       do i=1,self%current_section
          self%sections(i)%ptr => temp(i)%ptr
       end do
       if ( self%current_section.lt.n ) then
          do i=self%current_section+1,n
             self%sections(i)%ptr => null()
          end do
       end if
    else
       call log_debug( 'ConfigDB:growSections: Inital alloc', I4=n )
       allocate( self%sections(n) )
       do i=1,n
          self%sections(i)%ptr => null()
       end do
    end if

    self%max_section  = n

  end subroutine grow_section_list


  !/ =====================================================================================
  subroutine grow_comment_list( self, new_max )
    !/ -----------------------------------------------------------------------------------
    !! Grow the size of the comment array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self    !! reference to this configdb.
    integer,           intent(in)    :: new_max !! new max size.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    class(comment_pointer), pointer :: temp(:) => null()
    !/ -----------------------------------------------------------------------------------

    n = new_max
    if ( n.lt.1 ) then
       n = ADDITIONAL_PADDING
       call log_debug( 'ConfigDB:growComments: Added padding', I4=n )
    end if

    if ( associated( self%comments ) ) then
       temp          => self%comments
       self%comments => null()
       call log_debug( 'ConfigDB:growComments: Grow to', I4=n )
       allocate( self%comments(n) )
       do i=1,self%current_comment
          self%comments(i)%str = temp(i)%str
       end do
    else
       call log_debug( 'ConfigDB:growComments: Inital alloc', I4=n )
       allocate( self%comments(n) )
    end if

    self%max_comment  = n

  end subroutine grow_comment_list


  !/ =====================================================================================
  subroutine add_section_to_list( self, sec )
    !/ -----------------------------------------------------------------------------------
    !! Append a section.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t),              intent(inout) :: self !! reference to this configdb.
    type(config_section_t), target, intent(inout) :: sec
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: temp
    integer                          :: index
    character(:), allocatable        :: sname
    !/ -----------------------------------------------------------------------------------

    sname = sec%getName()

    index = self%find( sname )

    if ( 0.eq.index ) then

       self%current_section = self%current_section + 1
       if ( self%current_section.ge.self%max_section ) then
          call self%grow_section_list( self%max_section*2 )
       end if

       if ( associated( self%sections(self%current_section)%ptr ) ) then
          deallocate( self%sections(self%current_section)%ptr )
          nullify( self%sections(self%current_section)%ptr )
       end if

       allocate( self%sections(self%current_section)%ptr )
       call self%sections(self%current_section)%ptr%setName( sname )
       temp => self%sections(self%current_section)%ptr
    
    else

       temp => self%sections(index)%ptr

    end if

    call temp%merge( sec )

    temp => null()

  end subroutine add_section_to_list


  !/ =====================================================================================
  subroutine add_new_section_to_list( self, sname )
    !/ -----------------------------------------------------------------------------------
    !! Add a new section to the list
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self  !! this configdb.
    character(*),      intent(in)    :: sname !! name of a new section
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: new_sec
    !/ -----------------------------------------------------------------------------------

    allocate( new_sec )
    call new_sec%setName( sname )
    call self%add_section_to_list( new_sec )

  end subroutine add_new_section_to_list


  !/ =====================================================================================
  subroutine add_comment_to_list( self, comment )
    !/ -----------------------------------------------------------------------------------
    !! Append a section.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self    !! reference to this configdb.
    character(*),      intent(in)    :: comment !! file level comment
    !/ -----------------------------------------------------------------------------------

    self%current_comment = self%current_comment + 1

    if ( self%current_comment.ge.self%max_comment ) then
       call self%grow_comment_list( self%max_comment*2 )
    end if

    self%comments(self%current_comment)%str = comment

  end subroutine add_comment_to_list


  !/ =====================================================================================
  function find_index_by_name( self, name, STATUS ) result( index )
    !/ -----------------------------------------------------------------------------------
    !! Find the array index contatining the first occurance of name
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  name found           |
    !! |    1   |  no name found        |    
    !! |    2   |  section array null   |
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                          :: index  !! return index.
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    character(*),      intent(in)    :: name   !! name.
    integer, optional, intent(out)   :: STATUS !! return condition.
    !/ -----------------------------------------------------------------------------------
    integer :: ier, i
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    index = 0

    if ( associated( self%sections ) ) then
       loop: do i=1,self%current_section
          if ( associated(self%sections(i)%ptr) ) then
             if ( LEQ( name, self%sections(i)%ptr%getName() ) ) then
                index = i
                exit loop
             end if
          end if
       end do loop
    else
       ier = 2
       if ( report ) then
          call log_warn( 'Configdb%find: section array not allocated' )
       end if
    end if

    if ( 0.eq.index ) then
       ier = 1
       if ( report ) then
          call log_warn( 'ConfigDB:find: no name found, matching:', STR=name )
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier


  end function find_index_by_name


  !/ =====================================================================================
  function get_section_by_index( self, index, STATUS ) result( sec )
    !/ -----------------------------------------------------------------------------------
    !! Get a section by index.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), pointer :: sec    !! return section pointer.
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    integer,           intent(in)    :: index  !! index.
    integer, optional, intent(out)   :: STATUS !! return condition.
    !/ -----------------------------------------------------------------------------------
    integer :: ier
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    sec => null()

    if ( index.gt.self%current_section ) then
       ier = 1
       if ( report ) then
          call log_warn( 'ConfigDB:getSection(index): index out of bounds:', I4=index )
       end if
    else
       if ( associated( self%sections(index)%ptr ) ) then
          sec => self%sections(index)%ptr
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier

  end function get_section_by_index


  !/ =====================================================================================
  function get_section_by_name( self, name, STATUS ) result( sec )
    !/ -----------------------------------------------------------------------------------
    !! Return a section pointer contatining the first occurance of name.
    !!
    !! |  stat  |  errmsg         |
    !! | :----: | :-------------: |
    !! |    0   |  name found     |
    !! |    1   |  no name found  |    
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), pointer :: sec    !! pointer to the referenced section.
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    character(*),      intent(in)    :: name   !! reference name.
    integer, optional, intent(out)   :: STATUS !! return condition.
    !/ -----------------------------------------------------------------------------------
    integer :: ier, ier2, index
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    sec => null()
    
    index = self%find( name, STATUS=ier )

    if ( 0.eq.ier ) then
       sec => self%get_section_by_index( index, STATUS=ier2 )
       if ( 0.ne.ier2 ) then
          call log_error( 'ConfigDB:getSection(name): this should not happen' )
       end if
    else
       if ( report ) then
          call log_warn( 'ConfigDB:getSection(name): no name was found, matching:', STR=name )
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier

  end function get_section_by_name


  !/ =====================================================================================
  function get_comment_by_index( self, index, STATUS ) result( com )
    !/ -----------------------------------------------------------------------------------
    !! Return a comment string at index.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable        :: com    !! referenced comment
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    integer,           intent(in)    :: index  !! index.
    integer, optional, intent(out)   :: STATUS !! return condition.
    !/ -----------------------------------------------------------------------------------
    integer :: ier
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    if ( index.gt.self%current_comment ) then
       ier = 1
       if ( report ) then
          call log_warn( 'ConfigDB:getComment: index out of bounds:', I4=index )
       end if
    else
       com = self%comments(index)%str
    end if

    if ( present( STATUS ) ) STATUS = ier

  end function get_comment_by_index


  !/ =====================================================================================
  subroutine rewind_section( self )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self !! reference to this configdb.
    !/ -----------------------------------------------------------------------------------

    self%sindex = 1

  end subroutine rewind_section

  
  !/ =====================================================================================
  subroutine rewind_comment( self )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self !! reference to this configdb.
    !/ -----------------------------------------------------------------------------------

    self%cindex = 1

  end subroutine rewind_comment

  
  !/ =====================================================================================
  function has_next_section( self ) result( flag )    
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                          :: flag
    class(configdb_t), intent(inout) :: self !! reference to this configdb.
    !/ -----------------------------------------------------------------------------------

    if ( self%sindex.gt.self%current_section ) then
       flag = .false.
    else
       flag = .true.
    end if
    
  end function has_next_section

  
  !/ =====================================================================================
  function has_next_comment( self ) result( flag )    
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                          :: flag
    class(configdb_t), intent(inout) :: self !! reference to this configdb.
    !/ -----------------------------------------------------------------------------------

    if ( self%cindex.gt.self%current_comment ) then
       flag = .false.
    else
       flag = .true.
    end if
    
  end function has_next_comment

  
  !/ =====================================================================================
  function get_next_section( self, STATUS ) result( sec )
    !/ -----------------------------------------------------------------------------------
    !! Get the next available entry.
    !!
    !! |  stat  |  errmsg   |
    !! | :----: | :-------: |
    !! |    0   |  n/a      |
    !! |    1   |  no more  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), pointer :: sec    !! pointer to a section
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    integer, optional, intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    sec => null()

    if ( present( STATUS ) ) report = .false.

100 continue

    if ( self%sindex.le.self%current_section ) then
       sec => self%get( self%sindex ) ! this is a deep copy see below
       self%sindex = self%sindex + 1
       if ( associated( sec ) ) goto 200
       goto 100
    else
       if ( report ) then
          call log_warn( 'ConfigDB%next: read past end' )
       end if
       ier = 1
    end if

200 continue
    
    if ( present( STATUS ) ) STATUS = ier

  end function get_next_section

  
  !/ =====================================================================================
  function get_next_comment( self, STATUS ) result( com )
    !/ -----------------------------------------------------------------------------------
    !! Get the next available entry.
    !!
    !! |  stat  |  errmsg   |
    !! | :----: | :-------: |
    !! |    0   |  n/a      |
    !! |    1   |  no more  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable        :: com    !! a comment.
    class(configdb_t), intent(inout) :: self   !! reference to this configdb.
    integer, optional, intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    com = ''

    if ( present( STATUS ) ) report = .false.

100 continue

    if ( self%cindex.le.self%current_comment ) then
       com = self%getComment( self%cindex ) ! this is a deep copy see below
       self%cindex = self%cindex + 1
       if ( 0.lt.LEN_TRIM(com) ) goto 200
       goto 100
    else
       if ( report ) then
          call log_warn( 'ConfigDB%nextComment: read past end' )
       end if
       ier = 1
    end if

200 continue
    
    if ( present( STATUS ) ) STATUS = ier

  end function get_next_comment

  
         
 









  

  !/ =====================================================================================
  subroutine write_ini( self, FILE, UNIT, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !/ Write INI file
    !/ -----------------------------------------------------------------------------------
    use tlogger,    only : log_error
    use file_tools, only : WriteUnit
    implicit none
    class(configdb_t),          intent(inout) :: self   !! reference to this configdb.
    character(len=*), optional, intent(in)    :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)    :: UNIT   !! unit for an open unit
    integer,          optional, intent(out)   :: IOSTAT !! error return.
    !/ -----------------------------------------------------------------------------------
    integer :: ios, un, i
    logical :: report, sep_sec
    !/ -----------------------------------------------------------------------------------

    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    un = WriteUnit( FILE=FILE, UNIT=UNIT, IOSTAT=ios )

    if ( 0.ne.ios ) then
       if ( report ) then
          if ( present( FILE ) ) then
             call log_error( 'Cannot open file:', STR=FILE )
          else
             if ( present( UNIT ) ) then
                call log_error( 'Cannot access unit:', I4=UNIT )
             else
                call log_error( 'Access error:', I4=ios )
             end if
          end if
       end if
    else
       !/ --------------------------------------------------------------------------------

       sep_sec = .false.
       
       if ( associated( self%comments ) ) then
          do i=1,self%current_comment
             if ( allocated( self%comments(i)%str ) ) then
                write(un,1000) self%comments(i)%str
                sep_sec = .true.
             end if
          end do
       end if
       
       if ( associated( self%sections ) ) then
          do i=1,self%current_section
             if ( associated( self%sections(i)%ptr ) ) then
                if ( sep_sec ) then
                   write(un,*)
                else
                   sep_sec = .true.
                end if
                write(un,1100) self%sections(i)%ptr%getName()
                call self%sections(i)%ptr%writeINI( UNIT=un, IOSTAT=ios )
                if ( 0.ne.ios ) then
                   call log_error( 'Section write failed', I4=ios )
                end if
             end if
          end do
       end if
       
       !/ --------------------------------------------------------------------------------
       if ( present( FILE ) ) close( un )
    end if
  
    if ( present( IOSTAT ) ) iostat = ios

1000 format( '; ',A )
1100 format( '[',A,']' )
    
  end subroutine write_ini


  !/ =====================================================================================
  subroutine read_ini( self, FILE, UNIT, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !/ Read and parse INI file
    !/ -----------------------------------------------------------------------------------
    use tlogger,    only : log_error
    use file_tools, only : ReadUnit
    use file_tools, only : ReadLine
    implicit none
    class(configdb_t),          intent(inout) :: self   !! reference to this configdb.
    character(len=*), optional, intent(in)    :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)    :: UNIT   !! unit for an open unit
    integer,          optional, intent(out)   :: IOSTAT !! error return.
    !/ -----------------------------------------------------------------------------------
    integer                   :: ios, un
    logical                   :: report
    character(:), allocatable :: buffer
    character(1)              :: ctest
    class(config_section_t), pointer :: section => null()
    !/ -----------------------------------------------------------------------------------

    report = .true.
    if ( present( IOSTAT ) ) report = .false.

    un = ReadUnit( FILE=FILE, UNIT=UNIT, IOSTAT=ios )

    if ( 0.ne.ios ) then
       if ( report ) then
          if ( present( FILE ) ) then
             call log_error( 'Cannot open file:', STR=FILE )
          else
             if ( present( UNIT ) ) then
                call log_error( 'Cannot access unit:', I4=UNIT )
             else
                call log_error( 'Access error:', I4=ios )
             end if
          end if
       end if
    else
       !/ --------------------------------------------------------------------------------

       section => null()
       
100    continue

       call ReadLine( un, buffer, IOSTAT=ios)
       if ( ios.eq.IOSTAT_END ) goto 120
       if ( ios.ne.0 ) goto 110

       if ( 0.lt.LEN(buffer) ) then

          ctest = buffer

          if ( ';'.eq.ctest ) then
             !/ ----------------------------------------------------- line comment
             if ( associated( section ) ) then
                !/ --------------------------------- section level comment
                call section%addComment( buffer )
             else
                !/ --------------------------------- file level comment
                call self%addComment( buffer )
             end if
          else
             if ( '['.eq.ctest ) then
                !/ -------------------------------------------------- section label
                if ( associated( section ) ) then
                   !/ ------------------------------ save old section
                   call self%add( section )
                   section => null()
                end if
                !/ --------------------------------- create a section
                allocate( section )
                call section%setName( buffer(2:LEN_TRIM(buffer)-1) )
             else
                !/ -------------------------------------------------- other line
                call section%append( LINE=buffer )
             end if
          end if

       end if
       !/ ------------------
       goto 100
110    continue
       if ( report ) then
          write (*,*) 'Read failed'
       end if       
120    continue
       
       !/ --------------------------------------------------------------------------------
       if ( present( FILE ) ) close( un )
    end if

    if ( associated( section ) ) then
       !/ ------------------------------ save last section
       call self%add( section )
       section => null()
    end if
  
    if ( present( IOSTAT ) ) iostat = ios

  end subroutine read_ini




  !/ =====================================================================================
  subroutine merge_configdb( self, src )
    !/ -----------------------------------------------------------------------------------
    !/ Merge a ConfigDB into this ConfigDB
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(configdb_t), intent(inout) :: self !! reference to this configdb.
    type(configdb_t),  intent(inout) :: src  !! reference to another configdb.
    !/ -----------------------------------------------------------------------------------
    class(config_section_t), pointer :: this_sec, src_sec
    character(:), allocatable        :: com, sname
    integer                          :: icheck, ifnd
    !/ -----------------------------------------------------------------------------------
    
    call src%rewindComment

100 continue
    if ( src%hasNextComment() ) then
       com = src%nextComment( STATUS=icheck )
       if ( 0.eq.icheck ) then
          call self%addComment( com )
          goto 100
       else
          call log_error('hasNext=.true. but null was returned by nextComment()')
          goto 300
       end if
    end if

200 continue
    if ( src%hasNext() ) then
       src_sec => src%next( STATUS=icheck )
       if ( 0.eq.icheck ) then
          sname = src_sec%getName()
          this_sec => self%get( sname, STATUS=ifnd )
          if ( 0.eq.ifnd ) then
             call this_sec%merge( src_sec )
          else
             call self%add( src_sec )
          end if
          this_sec => null()
          goto 200
       else
          call log_error('hasNext=.true. but null was returned by next()')
          goto 300
       end if
       src_sec => null()
    end if

300 continue

  end subroutine merge_configdb
  
  
end module configdb_mod


!/ =======================================================================================
!/ **                              C O N F I G D B _ M O D                              **
!/ =========================================================================== END FILE ==
