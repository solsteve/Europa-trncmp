!/ ====================================================================== BEGIN FILE =====
!/ **                         C O N F I G _ S E C T I O N _ M O D                     **
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
module config_section_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-12-07
  !! license: GPL
  !!
  !! Provides a named section of comments and KVC records
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use config_entry_list_mod
  use config_entry_mod
  implicit none
  private

  !/ =====================================================================================
  type, public :: config_section_t
     !/ ----------------------------------------------------------------------------------

     type(config_entry_list_t), private :: section_records

     character(:), allocatable :: section_name

   contains

     procedure :: count_comments
     procedure :: count_KVC

     procedure :: make_comment_index
     procedure :: make_KVC_index

     procedure :: clear_one_entry
     procedure :: clear_all_entries
     generic, public :: clear => clear_one_entry, clear_all_entries

     procedure, public :: getName    => get_section_name
     procedure, public :: setName    => set_section_name

     procedure, public :: find       => find_index_by_key
     
     procedure, public :: get        => get_record_by_key
     procedure, public :: set        => set_record_by_key
     
     procedure, public :: append     => append_record
     
     procedure, public :: getComment => get_comment_by_index
     procedure, public :: addComment => add_comment

     final :: destroy_section

  end type config_section_t

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
  subroutine destroy_section( sec )
    !/ -----------------------------------------------------------------------------------
    !! Clear the data from all of the records
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_section_t), intent(inout) :: sec !! reference to this section.
    !/ -----------------------------------------------------------------------------------

    call sec%clear

  end subroutine destroy_section


  !/ =====================================================================================
  function count_comments( self ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! count the number of comments in this section.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                                :: n    !! number of comment only records.
    class(config_section_t), intent(inout) :: self !! reference to this section.
    !/ -----------------------------------------------------------------------------------
    integer :: i, m
    !/ -----------------------------------------------------------------------------------

    n = 0
    m = size( self%section_records )
    do i=1,m
       if ( self%section_records%isComment(i) ) then
          n = n + 1
       end if
    end do

  end function count_comments


  !/ =====================================================================================
  function count_KVC( self ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! count the number of Key=Value records in this section.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                                :: n    !! number of KVC records.
    class(config_section_t), intent(inout) :: self !! reference to this section.
    !/ -----------------------------------------------------------------------------------
    integer :: i, m
    !/ -----------------------------------------------------------------------------------

    n = 0
    m = size( self%section_records )
    do i=1,m
       if ( self%section_records%isKVPair(i) ) then
          n = n + 1
       end if
    end do

  end function count_KVC


  !/ =====================================================================================
  function get_number_of_records( sec, COUNT ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get then number of records. NUMBER=total number of records, COMMENT=number of
    !! records that are comment only, KVPAIR=number of records vith valid Key=Value pairs
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                               :: n     !! 
    type(config_section_t), intent(inout) :: sec   !! reference to this section.
    character(1), optional, intent(in)    :: COUNT !! type of count
    !/ -----------------------------------------------------------------------------------

    n = 0

    if ( present( COUNT ) ) then
       if ( ( 'N'.eq.COUNT ).or.( 'n'.eq.COUNT ) ) then
          n = size( sec%section_records )
          goto 999
       end if
       if ( ( 'C'.eq.COUNT ).or.( 'c'.eq.COUNT ) ) then
          n = sec%count_comments()
          goto 999
       end if
       if ( ( 'K'.eq.COUNT ).or.( 'k'.eq.COUNT ) ) then
          n = sec%count_KVC()
          goto 999
       end if
       call log_error( "size(COUNT=) should be 'NUMBER' or 'COMMENT' or 'KVPAIR'" )
    end if

999 continue
  end function get_number_of_records


  !/ =====================================================================================
  function make_comment_index( self, index ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Make an index of the records with only comments.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                                :: n        !! number of comment only records.
    class(config_section_t), intent(inout) :: self     !! reference to this section.
    integer, allocatable,    intent(inout) :: index(:) !! indexes of comment only records.
    !/ -----------------------------------------------------------------------------------
    integer :: i, m, k
    !/ -----------------------------------------------------------------------------------

    n = self%count_comments()
    allocate( index(n) )
    m = size( self%section_records )
    k = 1
    do i=1,m
       if ( self%section_records%isComment(i) ) then
          index(k) = i
          k = k + 1
       end if
    end do

  end function make_comment_index


  !/ =====================================================================================
  function make_KVC_index( self, index ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Make an index of the records with Key=Value pairs.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                                :: n        !! number of KVC records.
    class(config_section_t), intent(inout) :: self     !! reference to this section.
    integer, allocatable,    intent(inout) :: index(:) !! indexes of KVC records.
    !/ -----------------------------------------------------------------------------------
    integer :: i, m, k
    !/ -----------------------------------------------------------------------------------

    n = self%count_KVC()
    allocate( index(n) )
    m = size( self%section_records )
    k = 1
    do i=1,m
       if ( self%section_records%isKVPair(i) ) then
          index(k) = i
          k = k + 1
       end if
    end do

  end function make_KVC_index


  !/ =====================================================================================
  subroutine clear_all_entries( self )
    !/ -----------------------------------------------------------------------------------
    !! Clear the data from all of the records
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), intent(inout) :: self !! reference to this section.
    !/ -----------------------------------------------------------------------------------

    call self%section_records%clear

  end subroutine clear_all_entries


  !/ =====================================================================================
  subroutine clear_one_entry( self, index )
    !/ -----------------------------------------------------------------------------------
    !! Clear the data from all of the records
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), intent(inout) :: self  !! reference to this section.
    integer,                 intent(in)    :: index !! index of the entry to remove.
    !/ -----------------------------------------------------------------------------------

    call self%section_records%clear( index )

  end subroutine clear_one_entry


  !/ =====================================================================================
  function get_section_name( self ) result( name )
    !/ -----------------------------------------------------------------------------------
    !! Get the section name.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable              :: name !! name of this section.
    class(config_section_t), intent(inout) :: self !! reference to this section.
    !/ -----------------------------------------------------------------------------------

    name = self%section_name

  end function get_section_name


  !/ =====================================================================================
  subroutine set_section_name( self, name )
    !/ -----------------------------------------------------------------------------------
    !! Set the section name.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), intent(inout) :: self !! reference to this section.
    character(*),            intent(in)    :: name !! name for this section.
    !/ -----------------------------------------------------------------------------------

    self%section_name = name

  end subroutine set_section_name


  !/ =====================================================================================
  function find_index_by_key( self, key, STATUS ) result( index )
    !/ -----------------------------------------------------------------------------------
    !! Find the array index contatining the first occurance of key
    !!
    !! |  stat  |  errmsg        |
    !! | :----: | :------------: |
    !! |    0   |  key found     |
    !! |    1   |  no key found  |    
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer :: index
    class(config_section_t), intent(inout) :: self   !! reference to this section.
    character(*),            intent(in)    :: key    !! key string.
    integer, optional,       intent(out)   :: STATUS !! return condition
    !/ -----------------------------------------------------------------------------------
    integer                   :: i, n, ier
    character(:), allocatable :: test
    !/ -----------------------------------------------------------------------------------

    ier   = 1
    index = 0
    n     = size( self%section_records )

    loop: do i=1,n
       if ( self%section_records%isKVPair(i) ) then
          call self%section_records%get(i,KEY=test)
          if ( LEQ( key, test ) ) then
             index = i
             ier   = 0
             exit loop
          end if
       end if
    end do loop

    if ( present( STATUS ) ) STATUS = ier

  end function find_index_by_key


  !/ =====================================================================================
  subroutine get_record_by_key( self, key, VAL, COM, LINE, ENTRY, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Get record data that matches the key.
    !!
    !! |  stat  |  errmsg        |
    !! | :----: | :------------: |
    !! |    0   |  key found     |
    !! |    1   |  no key found  |    
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t),             intent(inout) :: self   !! reference to this section.
    character(*),                        intent(in)    :: key    !! key string.
    character(:), allocatable, optional, intent(out)   :: VAL    !! value string.
    character(:), allocatable, optional, intent(out)   :: COM    !! comment string.
    character(:), allocatable, optional, intent(out)   :: LINE   !! parsable line.
    type(config_entry_t),      optional, intent(inout) :: ENTRY  !! reference to a config entry.
    integer,                   optional, intent(out)   :: STATUS !! return condition
    !/ -----------------------------------------------------------------------------------
    integer :: ier, index
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    index = self%find( key, STATUS=ier )

    if ( 0.eq.ier ) then
       call self%section_records%get( index, VAL=VAL, COM=COM, LINE=LINE, ENTRY=ENTRY )
    else
       if ( report ) then
          call log_error( 'Section%get: no such key', STR=key )
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier

  end subroutine get_record_by_key


  !/ =====================================================================================
  subroutine set_record_by_key( self, KEY, VAL, COM, LINE, ENTRY, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Set record data that matches the key.
    !!
    !! |  stat  |  message                    |
    !! | :----: | :-------------------------: |
    !! |    0   |  record replaced            |
    !! |   -1   |  new record created         |
    !! |    1   |  input does not have a key  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t),        intent(inout) :: self   !! reference to this section.
    character(*),         optional, intent(in)    :: KEY    !! key string.
    character(*),         optional, intent(in)    :: VAL    !! value string.
    character(*),         optional, intent(in)    :: COM    !! comment string.
    character(*),         optional, intent(in)    :: LINE   !! parsable string.
    type(config_entry_t), optional, intent(inout) :: ENTRY  !! reference to a config entry.
    integer,              optional, intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    type(config_entry_t)      :: temp
    character(:), allocatable :: key_value
    integer                   :: ier, index
    logical                   :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( STATUS ) ) report = .false.

    call temp%set( KEY=key, VAL=VAL, COM=COM, LINE=LINE, ENTRY=ENTRY )

    if ( temp%isKVPair() ) then
       call temp%get( KEY=key_value )
    
       index = self%find( key_value, STATUS=ier )

       if ( 0.eq.ier ) then
          call self%section_records%set( index, KEY=key_value, ENTRY=temp )
       else
          call self%section_records%append( KEY=key_value, ENTRY=temp )
          ier = -1
       end if

    else
       if ( report ) then
          call log_error( 'Section%set: input does not have a key' )
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier

  end subroutine set_record_by_key


  !/ =====================================================================================
  subroutine append_record( self, KEY, VAL, COM, LINE, ENTRY, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Append a record to the list
    !!
    !! |  stat  |  message                    |
    !! | :----: | :-------------------------: |
    !! |    0   |  record replaced            |
    !! |   -1   |  new record created         |
    !! |    1   |  input does not have a key  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t),        intent(inout) :: self   !! reference to this section.
    character(*),         optional, intent(in)    :: KEY    !! key string.
    character(*),         optional, intent(in)    :: VAL    !! value string.
    character(*),         optional, intent(in)    :: COM    !! comment string.
    character(*),         optional, intent(in)    :: LINE   !! parsable line.
    type(config_entry_t), optional, intent(inout) :: ENTRY  !! reference to a config entry.
    integer,              optional, intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    type(config_entry_t)      :: temp
    character(:), allocatable :: comment, key_value
    integer                   :: ier
    logical                   :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( STATUS ) ) report = .false.

    call temp%set( KEY=key, VAL=VAL, COM=COM, LINE=LINE, ENTRY=ENTRY )
    
    if ( temp%isComment() ) then
       call temp%get( COM=comment )
       call self%addComment( comment )
    else
       if ( temp%isKVPair() ) then
          call temp%get( KEY=key_value )
          call self%set( KEY=key_value, ENTRY=temp )
       else
          call log_error( 'Section%append: neither Comment nor KV Pair' )
          ier = 1
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier    

  end subroutine append_record


  !/ =====================================================================================
  function get_comment_by_index( self, index, STATUS ) result( comment )
    !/ -----------------------------------------------------------------------------------
    !! get the index comment from the section records.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable              :: comment !! 
    class(config_section_t), intent(inout) :: self    !! reference to this section.
    integer,                 intent(in)    :: index   !! 
    integer, optional,       intent(out)   :: STATUS  !! return error code
    !/ -----------------------------------------------------------------------------------
    integer, allocatable :: cidx(:)
    integer              :: cn, ier
    logical              :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    cn = self%make_comment_index( cidx )

    if ( index.gt.cn ) then
       if ( report ) then
          call log_warn( 'ConfigSection%getComment: index out of bounds' )
       end if
       ier = 1
    else
       call self%section_records%get( cidx(index), COM=comment )
    end if

    if ( present( STATUS ) ) STATUS = ier

  end function get_comment_by_index


  !/ =====================================================================================
  subroutine add_comment( self, comment )
    !/ -----------------------------------------------------------------------------------
    !! Append a record to the list
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_section_t), intent(inout) :: self    !! reference to this section.
    character(*),            intent(in)    :: comment !! comment string
    !/ -----------------------------------------------------------------------------------

    call self%section_records%append( COM=comment )

  end subroutine add_comment


end module config_section_mod


!/ =======================================================================================
!/ **                         C O N F I G _ S E C T I O N _ M O D                     **
!/ =========================================================================== END FILE ==
