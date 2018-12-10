!/ ====================================================================== BEGIN FILE =====
!/ **                     C O N F I G _ E N T R Y _ L I S T _ M O D                     **
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
module config_entry_list_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-12-07
  !! license: GPL
  !!
  !! Provides a simple dynamic array of configuration file section entries.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use config_entry_mod
  implicit none
  private

  integer, parameter :: ADDITIONAL_PADDING = 16

  !/ =====================================================================================
  type, public :: config_entry_list_t
     !/ ----------------------------------------------------------------------------------

     class(config_entry_t), private, pointer :: entries(:) => null()

     integer, private :: current_index  = 0
     integer, private :: iterator_index = 1
     integer, private :: maximum_index  = 0

   contains

     procedure :: clear_one_entry
     procedure :: clear_all_entries
     generic, public :: clear => clear_one_entry, clear_all_entries

     procedure :: grow => grow_list

     procedure, public :: rewind    => rewind_list_index
     procedure, public :: next      => get_next_entry
     procedure, public :: hasNext   => list_has_next_entry

     procedure, public :: isComment => entry_by_index_is_only_comment
     procedure, public :: isKVPair  => entry_by_index_has_KV_pair

     procedure, public :: get       => get_entry_at_index
     procedure, public :: set       => set_entry_at_index

     procedure, public :: append    => append_to_list

     final :: destroy_entry_list

  end type config_entry_list_t


  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: get_number_stored_entries
  end interface size


  public :: size




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine destroy_entry_list( list )
    !/ -----------------------------------------------------------------------------------
    !! Deallocate.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_list_t), intent(inout) :: list
    !/ -----------------------------------------------------------------------------------

    if ( associated( list%entries ) ) then
       call list%clear
       deallocate( list%entries )
    end if

    nullify( list%entries )

    list%current_index  = 0
    list%iterator_index = 1
    list%maximum_index  = 0

  end subroutine destroy_entry_list


  !/ =====================================================================================
  function get_number_stored_entries( list ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of stored entries
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                                  :: n    !! number of entries stored.
    type(config_entry_list_t), intent(inout) :: list !! reference to an entry list.
    !/ -----------------------------------------------------------------------------------

    n = list%current_index

  end function get_number_stored_entries







  
  !/ =====================================================================================
  subroutine clear_all_entries( self )
    !/ -----------------------------------------------------------------------------------
    !! Clear all of the entries from the list.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self !! reference to this entry list.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    if ( associated( self%entries ) ) then
       n = size( self%entries )
       do i=1,n
          call self%entries(i)%clear
       end do
    else
       self%maximum_index = 0
    end if

    self%current_index  = 0
    self%iterator_index = 1

  end subroutine clear_all_entries


  !/ =====================================================================================
  subroutine clear_one_entry( self, index )
    !/ -----------------------------------------------------------------------------------
    !! Clear one of the entries from the list.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self  !! reference to this entry list.
    integer,                    intent(in)    :: index !! index
    !/ -----------------------------------------------------------------------------------
    integer :: n
    !/ -----------------------------------------------------------------------------------

    if ( associated( self%entries ) ) then
       n = size( self%entries )
       if ( index.le.n ) then
          call self%entries(index)%clear
          if ( index.eq.self%current_index ) then
             self%current_index = self%current_index - 1
          end if
       end if
    end if

  end subroutine clear_one_entry


  !/ =====================================================================================
  subroutine grow_list( self, new_max )
    !/ -----------------------------------------------------------------------------------
    !! Grow the size of the entries array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self !! reference to this entry list.
    integer,                    intent(in)    :: new_max
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    class(config_entry_t), pointer :: temp(:) => null()
    !/ -----------------------------------------------------------------------------------

    n = new_max
    if ( n.le.0 ) then
       n = ADDITIONAL_PADDING
    end if

    if ( associated( self%entries ) ) then
       temp         => self%entries
       self%entries => null()
       allocate( self%entries(n) )
       do i=1,self%current_index
          call self%entries(i)%copy( temp(i) )
       end do
    else
       allocate( self%entries(n) )
       self%current_index = 0
    end if

    self%iterator_index = 1
    self%maximum_index  = n

  end subroutine grow_list


  !/ =====================================================================================
  subroutine rewind_list_index( self )
    !/ -----------------------------------------------------------------------------------
    !! Rewind the iterator index.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self !! reference to this entry list.
    !/ -----------------------------------------------------------------------------------

    self%iterator_index = 1

  end subroutine rewind_list_index


  !/ =====================================================================================
  function list_has_next_entry( self ) result( TF )
    !/ -----------------------------------------------------------------------------------
    !! Can another entry be retireved.
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                                   :: TF   !! true if an entry is available.
    class(config_entry_list_t), intent(inout) :: self !! reference to this entry list.
    !/ -----------------------------------------------------------------------------------

    if ( self%iterator_index.gt.self%current_index ) then
       TF = .false.
    else
       TF = .true.
    end if

  end function list_has_next_entry


  !/ =====================================================================================
  subroutine get_next_entry( self, KEY, VAL, COM, LINE, ENTRY, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Get the next available entry.
    !!
    !! |  stat  |  errmsg   |
    !! | :----: | :-------: |
    !! |    0   |  n/a      |
    !! |    1   |  no more  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t),          intent(inout) :: self   !! reference to this entry list.
    character(:), allocatable, optional, intent(out)   :: KEY    !! key string.
    character(:), allocatable, optional, intent(out)   :: VAL    !! value string.
    character(:), allocatable, optional, intent(out)   :: COM    !! comment string.
    character(:), allocatable, optional, intent(out)   :: LINE   !! parsable line.
    type(config_entry_t),      optional, intent(inout) :: ENTRY  !! reference to a config entry.
    integer, optional,                   intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    if ( self%iterator_index.gt.self%current_index ) then
       if ( report ) then
          call log_warn( 'ConfigEntryList%next: read past end' )
       end if
       ier = 1
    else
       call self%get( self%iterator_index, KEY, VAL, COM, LINE, ENTRY )
       self%iterator_index = self%iterator_index + 1
    end if

    if ( present( STATUS ) ) STATUS = ier

  end subroutine get_next_entry


  !/ =====================================================================================
  function entry_by_index_is_only_comment( self, index ) result( flag )
    !/ -----------------------------------------------------------------------------------
    !! Is this entry a Comment only?
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                                   :: flag  !! true is this entry is only a comment.
    class(config_entry_list_t), intent(inout) :: self  !! reference to this entry list.
    integer,                    intent(in)    :: index !! index.
    !/ -----------------------------------------------------------------------------------

    flag = self%entries(index)%isComment()

  end function entry_by_index_is_only_comment


  !/ =====================================================================================
  function entry_by_index_has_KV_pair( self, index ) result( flag )
    !/ -----------------------------------------------------------------------------------
    !! Is this entry a Key=Value pair?
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                                   :: flag  !! true is this entry is only a comment.
    class(config_entry_list_t), intent(inout) :: self  !! reference to this entry list.
    integer,                    intent(in)    :: index !! index.
    !/ -----------------------------------------------------------------------------------

    flag = self%entries(index)%isKVPair()

  end function entry_by_index_has_KV_pair


  !/ =====================================================================================
  subroutine get_entry_at_index( self, index, KEY, VAL, COM, LINE, ENTRY, STATUS )
    !/ -----------------------------------------------------------------------------------
    !! Get the data from the indexed entry.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t),          intent(inout) :: self   !! reference to this entry list.
    integer,                             intent(in)    :: index  !! index
    character(:), allocatable, optional, intent(out)   :: KEY    !! key string.
    character(:), allocatable, optional, intent(out)   :: VAL    !! value string.
    character(:), allocatable, optional, intent(out)   :: COM    !! comment string.
    character(:), allocatable, optional, intent(out)   :: LINE   !! parsable line.
    type(config_entry_t),      optional, intent(inout) :: ENTRY  !! reference to a config entry.
    integer, optional,                   intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.

    if ( present( STATUS ) ) report = .false.

    if ( index.gt.self%current_index ) then
       if ( report ) then
          call log_warn( 'ConfigEntryList%get: read past end' )
       end if
       ier = 1
    else
       call self%entries(index)%get( KEY, VAL, COM, LINE, ENTRY )
    end if

    if ( present( STATUS ) ) STATUS = ier

  end subroutine get_entry_at_index


  !/ =====================================================================================
  subroutine set_entry_at_index( self, index, KEY, VAL, COM, LINE, ENTRY )
    !/ -----------------------------------------------------------------------------------
    !! Set the data in the indexed entry
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t),     intent(inout) :: self  !! reference to this entry list.
    integer,                        intent(in)    :: index !! index.
    character(*),         optional, intent(in)    :: KEY   !! key string.
    character(*),         optional, intent(in)    :: VAL   !! value string.
    character(*),         optional, intent(in)    :: COM   !! comment string.
    character(*),         optional, intent(in)    :: LINE  !! parsable line.
    type(config_entry_t), optional, intent(inout) :: ENTRY !! reference to a config entry.
    !/ -----------------------------------------------------------------------------------

    if ( index.ge.self%maximum_index ) then
       call self%grow( index+ADDITIONAL_PADDING )
    end if

    call self%entries(index)%set( KEY, VAL, COM, LINE, ENTRY )

    if ( index.gt.self%current_index ) then
       self%current_index = index
    end if

  end subroutine set_entry_at_index


  !/ =====================================================================================
  subroutine append_to_list( self, KEY, VAL, COM, LINE, ENTRY )
    !/ -----------------------------------------------------------------------------------
    !! Append a new entry to the next available index.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t),     intent(inout) :: self  !! reference to this entry list.
    character(*),         optional, intent(in)    :: KEY   !! key string.
    character(*),         optional, intent(in)    :: VAL   !! value string.
    character(*),         optional, intent(in)    :: COM   !! comment string.
    character(*),         optional, intent(in)    :: LINE  !! parsable line.
    type(config_entry_t), optional, intent(inout) :: ENTRY !! reference to a config entry.
    !/ -----------------------------------------------------------------------------------

    self%current_index = self%current_index + 1

    if ( self%current_index.ge.self%maximum_index ) then
       call self%grow( self%maximum_index*2 )
    end if

    call self%set( self%current_index, KEY, VAL, COM, LINE, ENTRY )

  end subroutine append_to_list


end module config_entry_list_mod


!/ =======================================================================================
!/ **                     C O N F I G _ E N T R Y _ L I S T _ M O D                     **
!/ =========================================================================== END FILE ==
