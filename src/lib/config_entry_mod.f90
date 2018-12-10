!/ ====================================================================== BEGIN FILE =====
!/ **                          C O N F I G _ E N T R Y _ M O D                          **
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
module config_entry_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-11-23
  !! license: GPL
  !!
  !! Provides a configuration file section entry.
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use string_tools
  implicit none
  private


  !/ =====================================================================================
  type, public :: config_entry_t
     !/ ----------------------------------------------------------------------------------
     character(len=:), allocatable :: key_string
     character(len=:), allocatable :: value_string
     character(len=:), allocatable :: comment_string

   contains

     procedure, public :: debug      => temp_debug

     procedure, public :: clear      => clear_entry
     procedure, public :: set        => set_kvc
     procedure, public :: get        => get_kvc
     procedure, public :: copy       => copy_entry
     procedure, public :: fromString => entry_from_string
     procedure, public :: toString   => entry_to_string

     procedure, public :: getKey     => get_key
     procedure, public :: getValue   => get_value
     procedure, public :: getComment => get_comment

     procedure, public :: isComment  => entry_is_only_comment
     procedure, public :: isKVPair   => entry_has_KV_pair

     final :: destroy_entry

  end type config_entry_t




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine temp_debug( self )
    !/ -----------------------------------------------------------------------------------
    !/ PLEASE REMOVE THIS TEMPORARY TESTING PROCEDURE
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------

    if ( allocated( self%key_string) ) then
       write( *,10) self%key_string
    else
       write( *,11)
    end if

    if ( allocated( self%value_string) ) then
       write( *,20) self%value_string
    else
       write( *,21)
    end if

    if ( allocated( self%comment_string) ) then
       write( *,30) self%comment_string
    else
       write( *,31)
    end if

10  format( 'Key     [',A,']')
11  format( 'Key     {NONE}' )
20  format( 'Value   [',A,']')
21  format( 'value   {NONE}' )
30  format( 'Comment [',A,']')
31  format( 'Comment {NONE}' )

  end subroutine temp_debug


  !/ =====================================================================================
  subroutine destroy_entry( ent )
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_t), intent(inout) :: ent !! reference to an entry.
    !/ -----------------------------------------------------------------------------------

    call ent%clear

  end subroutine destroy_entry


  !/ =====================================================================================
  subroutine clear_entry( self )
    !/ -----------------------------------------------------------------------------------
    !! Clear the fields from this entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    !/ -----------------------------------------------------------------------------------

    if ( allocated( self%key_string) ) then
       deallocate( self%key_string )
    end if

    if ( allocated( self%value_string) ) then
       deallocate( self%value_string )
    end if

    if ( allocated( self%comment_string) ) then
       deallocate( self%comment_string )
    end if

  end subroutine clear_entry


  !/ =====================================================================================
  subroutine set_kvc( self, KEY, VAL, COM, LINE, ENTRY )
    !/ -----------------------------------------------------------------------------------
    !! Set Entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t),          intent(inout) :: self  !! reference to this entry.
    character(*),         optional, intent(in)    :: KEY   !! key string.
    character(*),         optional, intent(in)    :: VAL   !! value string.
    character(*),         optional, intent(in)    :: COM   !! comment string.
    character(*),         optional, intent(in)    :: LINE  !! parsable line.
    type(config_entry_t), optional, intent(inout) :: ENTRY !! reference to a source entry.
    !/ -----------------------------------------------------------------------------------

    if ( present( LINE ) ) then
       call self%fromString( LINE )
    end if

    if ( present( ENTRY ) ) then
       call self%copy( ENTRY )
    end if

    ! ----- the following allows you to overide parsed values -------------

    if ( present( KEY ) ) then
       self%key_string = KEY
    end if

    if ( present( VAL ) ) then
       self%value_string = VAL
    end if

    if ( present( COM ) ) then
       self%comment_string = COM
    end if

  end subroutine set_kvc


  !/ =====================================================================================
  subroutine get_kvc( self, KEY, VAL, COM, LINE, ENTRY )
    !/ -----------------------------------------------------------------------------------
    !! Get Entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t),               intent(inout) :: self  !! reference to this entry.
    character(:), allocatable, optional, intent(out)   :: KEY   !! key string.
    character(:), allocatable, optional, intent(out)   :: VAL   !! value string.
    character(:), allocatable, optional, intent(out)   :: COM   !! comment string.
    character(:), allocatable, optional, intent(out)   :: LINE  !! parsable line.
    type(config_entry_t),      optional, intent(inout) :: ENTRY !! reference to a destination entry.
    !/ -----------------------------------------------------------------------------------

    if ( present( KEY ) ) then
       KEY = self%key_string
    end if

    if ( present( VAL ) ) then
       VAL = self%value_string
    end if

    if ( present( COM ) ) then
       COM = self%comment_string
    end if

    if ( present( LINE ) ) then
       LINE = self%toString()
    end if

    if ( present( ENTRY ) ) then
       call ENTRY%copy( self )
    end if

  end subroutine get_kvc


  !/ =====================================================================================
  function get_key( self ) result( key )
    !/ -----------------------------------------------------------------------------------
    !! Get the key for this entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable            :: key  !! key for this entry.
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    !/ -----------------------------------------------------------------------------------

    key = self%key_string

  end function get_key


  !/ =====================================================================================
  function get_value( self ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Get the value for this entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable            :: val  !! value for this entry.
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    !/ -----------------------------------------------------------------------------------

    val = self%value_string

  end function get_value


  !/ =====================================================================================
  function get_comment( self ) result( com )
    !/ -----------------------------------------------------------------------------------
    !! Get the comment for this entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable            :: com  !! comment for this entry.
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    !/ -----------------------------------------------------------------------------------

    com = self%comment_string

  end function get_comment


  !/ =====================================================================================
  subroutine copy_entry( self, src )
    !/ -----------------------------------------------------------------------------------
    !! Deep copy of a source entry.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    type(config_entry_t),  intent(in)    :: src  !! reference to a source entry.
    !/ -----------------------------------------------------------------------------------

    if ( allocated(src%key_string) ) then
       self%key_string = src%key_string
    end if

    if ( allocated(src%value_string) ) then
       self%value_string = src%value_string
    end if

    if ( allocated(src%comment_string ) ) then
       self%comment_string = src%comment_string
    end if

  end subroutine copy_entry


  !/ =====================================================================================
  function entry_has_KV_pair( self ) result( flag )
    !/ -----------------------------------------------------------------------------------
    !! Is this entry a Key=Value pair?
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                              :: flag !! true is this entry is only a comment.
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    !/ -----------------------------------------------------------------------------------

    flag = .false.

    if ( allocated( self%key_string ) ) then
       if ( allocated( self%value_string ) ) then
          flag = .true.
       end if
    end if

  end function entry_has_KV_pair


  !/ =====================================================================================
  function entry_is_only_comment( self ) result( flag )
    !/ -----------------------------------------------------------------------------------
    !! Is this entry a Comment only?
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                              :: flag !! true is this entry is only a comment.
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    !/ -----------------------------------------------------------------------------------

    flag = .false.
    if ( allocated( self%comment_string ) ) then
       if ( .not. allocated( self%key_string ) ) then
          flag = .true.
       end if
    end if

  end function entry_is_only_comment


  !/ =====================================================================================
  subroutine entry_from_string( self, line )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for the contents of this entry.
    !!
    !! key = value ; comment
    !!
    !!     value = string
    !!     value = 'multi part string'
    !!     value = "multi part string"
    !!     value = [list, list, list]
    !!     value = (list, list, list)
    !!     value = {list, list, list}
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(config_entry_t), intent(inout) :: self !! reference to this entry.
    character(*),          intent(in)    :: line !! parsable line.
    !/ -----------------------------------------------------------------------------------
    type(string_splitter)     :: SP
    character(:), allocatable :: S1
    integer                   :: n, k
    !/ -----------------------------------------------------------------------------------

    call split( SP, line, ';', COUNT=n )
    if ( 2.eq.n ) then ! ----- comment found ----------------------
       S1                  = SP%get(1)
       self%comment_string = SP%get(2)
       call split( SP, S1, '=', COUNT=n )
       k = len( SP%get(1) )
       if ( 0.lt.k ) then
          self%key_string = SP%get(1)
       end if
       if ( 2.eq.n ) then
          self%value_string = SP%get(2)
       end if
    else ! ----- no comment found ----------------------
       call split( SP, line, '=', COUNT=n )
       self%key_string   = SP%get(1)
       if ( 2.eq.n ) then
          self%value_string = SP%get(2)
       end if
    end if

  end subroutine entry_from_string


  !/ =====================================================================================
  function entry_to_string( self, STATUS ) result( pstr )
    !/ -----------------------------------------------------------------------------------
    !! Convert this entry to a string
    !! Note: this string is parsable by entry%fromString()
    !/ -----------------------------------------------------------------------------------
    use tlogger, only : log_warn
    implicit none
    character(:), allocatable            :: pstr   !! parsable string
    class(config_entry_t), intent(inout) :: self   !! reference to this entry.
    integer, optional,     intent(out)   :: STATUS !! return a status
    !/ -----------------------------------------------------------------------------------
    integer :: count, ier
    logical :: report
    !/ -----------------------------------------------------------------------------------

    count = 0
    ier   = 0
    report = .true.
    if ( present( STATUS ) ) report = .false.

    if ( allocated( self%key_string ) )     count = count + 1
    if ( allocated( self%value_string ) )   count = count + 2
    if ( allocated( self%comment_string ) ) count = count + 4

    if    ( 1.eq.count ) then
       pstr = self%key_string
    elseif( 2.eq.count ) then
       if ( report ) then
          call log_warn( 'value with no key' )
       end if
       ier = 2
    elseif( 3.eq.count ) then
       pstr = self%key_string // ' = ' // self%value_string
    elseif( 4.eq.count ) then
       pstr = '; ' // self%comment_string
    elseif( 5.eq.count ) then
       pstr = self%key_string // ' ; ' // self%comment_string
    elseif( 6.eq.count ) then
       if ( report ) then
          call log_warn( 'value with no key' )
       end if
       ier = 3
    elseif( 7.eq.count ) then
       pstr = self%key_string // ' = ' // self%value_string // ' ; ' // self%comment_string
    else
       pstr = ''
       ier = 1
    end if

    if ( present( STATUS ) ) STATUS = ier  

  end function entry_to_string


end module config_entry_mod


!/ =======================================================================================
!/ **                          C O N F I G _ E N T R Y _ M O D                          **
!/ =========================================================================== END FILE ==
