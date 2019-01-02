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
  type :: entry_pointer
     !/ ----------------------------------------------------------------------------------
     class(config_entry_t), pointer :: ptr => null()
  end type entry_pointer


  !/ =====================================================================================
  type, public :: config_entry_list_t
     !/ ----------------------------------------------------------------------------------

     class(entry_pointer), private, pointer :: entries(:) => null()

     integer, private :: current_index  = 0
     integer, private :: iterator_index = 1
     integer, private :: maximum_index  = 0

   contains

     procedure         :: clear_one_entry
     procedure         :: clear_all_entries
     procedure         :: delete_one_entry
     procedure         :: delete_all_entries
     
     generic,   public :: clear      => clear_one_entry, clear_all_entries
     generic,   public :: delete     => delete_one_entry, delete_all_entries
     procedure         :: grow       => grow_list

     procedure, public :: rewind     => rewind_list_index
     procedure, public :: next       => get_next_entry
     procedure, public :: hasNext    => list_has_next_entry

     procedure, public :: isComment  => entry_by_index_is_only_comment
     procedure, public :: isKVPair   => entry_by_index_has_KV_pair

     procedure, public :: getKey     => get_key_at_index
     procedure, public :: getValue   => get_value_at_index
     procedure, public :: getComment => get_comment_at_index

     procedure, public :: get        => get_entry_at_index
     procedure, public :: set        => set_entry_at_index

     procedure, public :: append     => append_to_list

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
    integer :: i,n

    if ( associated( list%entries ) ) then
       n = size( list%entries )
       do i=1,n
          nullify( list%entries(i)%ptr )
       end do
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
          if ( associated( self%entries(i)%ptr ) ) then
             call self%entries(i)%ptr%clear
          end if
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
          if ( associated( self%entries(index)%ptr ) ) then
             call self%entries(index)%ptr%clear
          end if
          if ( index.eq.self%current_index ) then
             self%current_index = self%current_index - 1
          end if
       end if
    end if

  end subroutine clear_one_entry


  !/ =====================================================================================
  subroutine delete_all_entries( self )
    !/ -----------------------------------------------------------------------------------
    !! Delete all of the entries from the list.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self !! reference to this entry list.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    if ( associated( self%entries ) ) then
       n = size( self%entries )
       do i=1,n
          if ( associated( self%entries(i)%ptr ) ) then
             nullify( self%entries(i)%ptr )
          end if
       end do
    else
       self%maximum_index = 0
    end if

    self%current_index  = 0
    self%iterator_index = 1

  end subroutine delete_all_entries


  !/ =====================================================================================
  subroutine delete_one_entry( self, index )
    !/ -----------------------------------------------------------------------------------
    !! Delete one of the entries from the list.
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
          if ( associated( self%entries(index)%ptr ) ) then
             nullify( self%entries(index)%ptr )
          end if
          if ( index.eq.self%current_index ) then
             self%current_index = self%current_index - 1
          end if
       end if
    end if

  end subroutine delete_one_entry


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
    class(entry_pointer), pointer :: temp(:) => null()
    !/ -----------------------------------------------------------------------------------

    n = new_max
    if ( n.le.0 ) then
       n = ADDITIONAL_PADDING
       call log_debug( 'Added padding', I4=n )
    end if

    if ( associated( self%entries ) ) then
       temp         => self%entries
       self%entries => null()
       call log_debug( 'Grow to', I4=n )
       allocate( self%entries(n) )
       do i=1,self%current_index
          if ( associated( temp(i)%ptr ) ) then
             self%entries(i)%ptr => temp(i)%ptr
          else
             self%entries(i)%ptr => null()
          end if
       end do
       if ( self%current_index.lt.n ) then
          do i=self%current_index+1,n
             self%entries(i)%ptr => null()
          end do
       end if
    else
       call log_debug( 'Inital alloc', I4=n )
       allocate( self%entries(n) )
       do i=1,n
          self%entries(i)%ptr => null()
       end do
    end if

    self%iterator_index = 1
    self%maximum_index  = n

  end subroutine grow_list


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

    flag = self%entries(index)%ptr%isComment()

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

    flag = self%entries(index)%ptr%isKVPair()

  end function entry_by_index_has_KV_pair


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
  function get_next_entry( self, STATUS ) result( ent )
    !/ -----------------------------------------------------------------------------------
    !! Get the next available entry.
    !!
    !! |  stat  |  errmsg   |
    !! | :----: | :-------: |
    !! |    0   |  n/a      |
    !! |    1   |  no more  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t), pointer            :: ent    !! pointer to an entry.
    class(config_entry_list_t), intent(inout) :: self   !! reference to this entry list.
    integer, optional,          intent(out)   :: STATUS !! return error code
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    ent => null()

    if ( present( STATUS ) ) report = .false.

100 continue

    if ( self%iterator_index.le.self%current_index ) then
       ent => self%get( self%iterator_index ) ! this is a deep copy see below
       self%iterator_index = self%iterator_index + 1
       if ( associated( ent ) ) goto 200
       goto 100
    else
       if ( report ) then
          call log_warn( 'ConfigEntryList%next: read past end' )
       end if
       ier = 1
    end if

200 continue
    
    if ( present( STATUS ) ) STATUS = ier

  end function get_next_entry

  
  !/ =====================================================================================
  function get_key_at_index( self, index ) result( key )
    !/ -----------------------------------------------------------------------------------
    !! Get the key from the indexed entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character, allocatable                    :: key
    class(config_entry_list_t), intent(inout) :: self   !! reference to this entry list.
    integer,                    intent(in)    :: index    
    !/ -----------------------------------------------------------------------------------
    if ( associated( self%entries(index)%ptr ) ) then
       key = self%entries(index)%ptr%getKey()
    end if
  end function get_key_at_index


  !/ =====================================================================================
  function get_value_at_index( self, index ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Get the value from the indexed entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character, allocatable                    :: val
    class(config_entry_list_t), intent(inout) :: self   !! reference to this entry list.
    integer,                    intent(in)    :: index    
    !/ -----------------------------------------------------------------------------------
    if ( associated( self%entries(index)%ptr ) ) then
       val = self%entries(index)%ptr%getValue()
    end if
  end function get_value_at_index


  !/ =====================================================================================
  function get_comment_at_index( self, index ) result( com )
    !/ -----------------------------------------------------------------------------------
    !! Get the comment from the indexed entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character, allocatable                    :: com
    class(config_entry_list_t), intent(inout) :: self   !! reference to this entry list.
    integer,                    intent(in)    :: index    
    !/ -----------------------------------------------------------------------------------
    if ( associated( self%entries(index)%ptr ) ) then
       com = self%entries(index)%ptr%getComment()
    end if
  end function get_comment_at_index


  !/ =====================================================================================
  function get_entry_at_index( self, index, STATUS ) result( ent )
    !/ -----------------------------------------------------------------------------------
    !! Get the data from the indexed entry.
    !!
    !! |  stat  |  errmsg               |
    !! | :----: | :-------------------: |
    !! |    0   |  n/a                  |
    !! |    1   |  index out of bounds  |
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t), pointer            :: ent    !! pointer to a config entry.
    class(config_entry_list_t), intent(inout) :: self   !! reference to this entry list.
    integer,                    intent(in)    :: index  !! index
    integer,          optional, intent(out)   :: STATUS !! return error code
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
       if ( associated( self%entries(index)%ptr ) ) then
          allocate( ent )
          call ent%copy( self%entries(index)%ptr )
       else
          ent => null()
       end if
    end if

    if ( present( STATUS ) ) STATUS = ier

  end function get_entry_at_index


  !/ =====================================================================================
  subroutine set_entry_at_index( self, index, ent )
    !/ -----------------------------------------------------------------------------------
    !! Set the data in the indexed entry
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self  !! reference to this entry list.
    integer,                    intent(in)    :: index !! index.
    class(config_entry_t),      intent(in)    :: ent   !! reference to a config entry.
    !/ -----------------------------------------------------------------------------------
    if ( index.ge.self%maximum_index ) then
       call self%grow( index+ADDITIONAL_PADDING )
    end if

    if ( .not. associated( self%entries(index)%ptr ) ) then
       allocate( self%entries(index)%ptr )
    end if

    call self%entries(index)%ptr%copy( ent )

    if ( index.gt.self%current_index ) then
       self%current_index = index
    end if

  end subroutine set_entry_at_index


  !/ =====================================================================================
  subroutine append_to_list( self, ent )
    !/ -----------------------------------------------------------------------------------
    !! Append a new entry to the next available index.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_list_t), intent(inout) :: self !! reference to this entry list.
    type(config_entry_t),       intent(in)    :: ent  !! reference to a config entry.
    !/ -----------------------------------------------------------------------------------
    self%current_index = self%current_index + 1

    if ( self%current_index.ge.self%maximum_index ) then
       call self%grow( self%maximum_index*2 )
    end if

    call self%set( self%current_index, ent )

  end subroutine append_to_list


end module config_entry_list_mod


!/ =======================================================================================
!/ **                     C O N F I G _ E N T R Y _ L I S T _ M O D                     **
!/ =========================================================================== END FILE ==
