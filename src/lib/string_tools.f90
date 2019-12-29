!/ ====================================================================== BEGIN FILE =====
!/ **                              S T R I N G _ T O O L S                              **
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
module string_tools
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2017-02-25
  !! license: GPL
  !!
  !! Provides Collection of string manipulation tools.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  public :: string_splitter, split, toString, strcmp, find_any, find_in, count_char,    &
       &    toUpper, toLower, containedBy, asInteger,  asReal4, asReal8, asIntegerList, &
       &    asReal4List, asReal8List

  !/ -------------------------------------------------------------------------------------
  type :: string_splitter
     !/ ----------------------------------------------------------------------------------

     character(len=:), allocatable :: work_string
     integer                       :: n_parts = 0
     integer,          allocatable :: cut(:)

   contains

     procedure, private :: strsplt_get_part
     procedure, private :: strsplt_count

     generic :: get   => strsplt_get_part
     generic :: count => strsplt_count

  end type string_splitter


  !/ -------------------------------------------------------------------------------------
  interface split
     !/ ----------------------------------------------------------------------------------
     module procedure :: strsplt_create
  end interface split

  !/ -------------------------------------------------------------------------------------
  interface toString
     !/ ----------------------------------------------------------------------------------
     procedure :: to_string_int32
     procedure :: to_string_int16
     procedure :: to_string_real4
     procedure :: to_string_real8
     procedure :: to_string_int32_vec
     procedure :: to_string_int16_vec
     procedure :: to_string_real4_vec
     procedure :: to_string_real8_vec
  end interface toString


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  function find_any( src, test, pos ) result( idx )
    !/ -----------------------------------------------------------------------------------
    !! Find the position of the first test character.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),      intent(in) :: src  !! reference to a character string.
    character(*),      intent(in) :: test !! reference to a list of test characters.
    integer, optional, intent(in) :: pos  !! position in the reference string.
    integer                       :: idx  !! position of the first test character.
    !/ -----------------------------------------------------------------------------------
    integer :: start
    !/ -----------------------------------------------------------------------------------

    start = 1
    if (present(pos)) then
       start = pos
    end if

    idx = start + scan( src(start:), test ) - 1

  end function find_any


  !/ =====================================================================================
  function find_in( ref, test, pos ) result( idx )
    !/ -----------------------------------------------------------------------------------
    !! Find the position of the first test character.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),      intent(in) :: ref  !! reference to a character string.
    character(1),      intent(in) :: test !! reference to a list of test characters.
    integer, optional, intent(in) :: pos  !! position in the reference string.
    integer                       :: idx  !! position of the first test character.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, start
    !/ -----------------------------------------------------------------------------------

    n = len(ref)
    start = 1

    if (present(pos)) then
       start = pos
    end if

    idx = 0
    do i=start,n
       if ( ref(i:i).eq.test ) then
          idx = i
          exit
       end if
    end do

  end function find_in


  !/ =====================================================================================
  function count_char( str, test ) result( cnt )
    !/ -----------------------------------------------------------------------------------
    !! Count the number of occurances
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=*), intent(in) :: str  !! 
    character(len=1), intent(in) :: test !! 
    integer                      :: cnt  !! 
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = len(str)
    cnt = 0
    do i=1,n
       if ( test.eq.str(i:i) ) then
          cnt = cnt + 1
       end if
    end do

  end function count_char


  !/ =====================================================================================
  function toUpper(s1)  result (s2)
    !/ -----------------------------------------------------------------------------------
    !! Convert all the characters of a string to upper case.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*)       :: s1 !! reference to an input string
    character(len(s1)) :: s2 !! input string converted to upper case
    !/ -----------------------------------------------------------------------------------
    character          :: ch
    integer,parameter  :: DUC = ichar('A') - ichar('a')
    integer            :: i
    !/ -----------------------------------------------------------------------------------

    do i = 1,len(s1)
       ch = s1(i:i)
       if (ch.ge.'a'.and.ch.le.'z') ch = char(ichar(ch)+DUC)
       s2(i:i) = ch
    end do

  end function toUpper


  !/ =====================================================================================
  function toLower(s1)  result (s2)
    !/ -----------------------------------------------------------------------------------
    !! Convert all the characters of a string to lower case.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*)       :: s1 !! reference to an input string
    character(len(s1)) :: s2 !! input string converted to lower case
    !/ -----------------------------------------------------------------------------------
    character          :: ch
    integer,parameter  :: DUC = ichar('A') - ichar('a')
    integer            :: i
    !/ -----------------------------------------------------------------------------------

    do i = 1,len(s1)
       ch = s1(i:i)
       if (ch.ge.'A'.and.ch.le.'Z') ch = char(ichar(ch)-DUC)
       s2(i:i) = ch
    end do

  end function toLower


  !/ =====================================================================================
  function containedBy( test, lhs, rhs ) result( xstr )
    !/ -----------------------------------------------------------------------------------
    !! Return a string that is enclosed by an opening and closing character. lhs and rhs
    !! contain valid begin and ending characters.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in)      :: test !! reference to an input string
    character(*), intent(in)      :: lhs  !! reference to a string of opening charaters
    character(*), intent(in)      :: rhs  !! reference to a string of closing charaters
    character(len=len_trim(test)) :: xstr !! enclosed string
    !/ -----------------------------------------------------------------------------------
    integer :: lpos, rpos
    !/ -----------------------------------------------------------------------------------
    xstr = ''

    lpos = scan( test, lhs )
    if ( 0.eq.lpos ) then
       goto 10
    end if

    rpos = scan( test, rhs, .true. )
    if ( 0.eq.rpos ) then
       goto 10
    end if

    xstr = test( lpos+1 : rpos-1 )
10  continue

  end function containedBy


  !/ =====================================================================================
  subroutine strsplt_create( ss, src, delim, COUNT )
    !/ -----------------------------------------------------------------------------------
    !! Split a string along a delimeter
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(string_splitter), intent(inout) :: ss    !! reference to a splitter.
    character(*),          intent(in)    :: src   !! source string.
    character(len=1),      intent(in)    :: delim !! delimeter character.
    integer, optional,     intent(out)   :: COUNT !! optional number of tokens
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, idx
    !/ -----------------------------------------------------------------------------------

    ss%work_string = trim(adjustl( src ) )

    n = count_char( ss%work_string, delim ) + 1

    if ( allocated( ss%cut ) ) deallocate( ss%cut )
    
    allocate( integer :: ss%cut(n) )

    n = len(ss%work_string)

    idx = 1
    do i=1,n
       if ( delim.eq.ss%work_string(i:i) ) then
          ss%cut(idx) = i
          idx = idx + 1
       end if
    end do

    ss%cut(idx) = n+1

    ss%n_parts = idx

    if ( present( COUNT ) ) COUNT = idx

  end subroutine strsplt_create


  !/ =====================================================================================
  function strsplt_get_part( self, n ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Get the nth field in a split string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(string_splitter), intent(in) :: self !! reference to this string splitter.
    integer,                intent(in) :: n    !! index.
    character(len=:), allocatable      :: str  !! the nth field in the split string.
    !/ -----------------------------------------------------------------------------------

    if ( 1.eq.n ) then
       str = trim(adjustl(self%work_string(1:self%cut(1)-1)))
    else
       str = trim(adjustl(self%work_string(self%cut(n-1)+1:self%cut(n)-1)))
    end if

  end function strsplt_get_part


  !/ =====================================================================================
  function strsplt_count( self ) result( cnt )
    !/ -----------------------------------------------------------------------------------
    !! Number of fields.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(string_splitter), intent(in) :: self !! reference to this string splitter.
    integer                            :: cnt  !! number of fields in the split string.
    !/ -----------------------------------------------------------------------------------

    cnt = self%n_parts

  end function strsplt_count




  !/ =====================================================================================
  function asInteger( str, ios ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for an integer value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),      intent(in)  :: str !! reference to a string.
    integer, optional, intent(out) :: ios !! optional error code.
    integer                        :: val !! parsed integer value.
    !/ -----------------------------------------------------------------------------------
    integer :: rstat
    !/ -----------------------------------------------------------------------------------

    read(str,*,iostat=rstat) val
    if ( 0.ne.rstat ) then
       val = 0
    end if

    if ( present(ios) ) then
       ios = rstat
    end if

  end function asInteger


  !/ =====================================================================================
  function asReal4( str, ios ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for a 4 byte real value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),      intent(in)  :: str !! reference to a string.
    integer, optional, intent(out) :: ios !! optional error code.
    real(sp)                       :: val !! parsed 4 byte real value.
    !/ -----------------------------------------------------------------------------------
    integer :: rstat
    !/ -----------------------------------------------------------------------------------

    read(str,*,iostat=rstat) val
    if ( 0.ne.rstat ) then
       val = 0.0e0
    end if

    if ( present(ios) ) then
       ios = rstat
    end if

  end function asReal4


  !/ =====================================================================================
  function asReal8( str, ios ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for a 8 byte real value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),      intent(in)  :: str !! reference to a string.
    integer, optional, intent(out) :: ios !! optional error code.
    real(dp)                       :: val !! parsed 8 byte real value.
    !/ -----------------------------------------------------------------------------------
    integer :: rstat
    !/ -----------------------------------------------------------------------------------

    read(str,*,iostat=rstat) val
    if ( 0.ne.rstat ) then
       val = 0.0d0
    end if

    if ( present(ios) ) then
       ios = rstat
    end if

  end function asReal8


  !/ =====================================================================================
  function asIntegerList( str ) result( ary )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for a list of integers.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: str    !! string to convert.
    integer, allocatable     :: ary(:) !! array of integers.
    !/ -----------------------------------------------------------------------------------
    character(len=:), allocatable :: temp1
    type(string_splitter)         :: split
    integer                       :: i, n
    !/ -----------------------------------------------------------------------------------

    temp1 = containedBy( str, '[{(', ')}]' )

    call strsplt_create( split, temp1, ',' )

    n = split%count()
    allocate( ary(n) )

    do i=1,n
       ary(i) = asInteger( split%get(i) )
    end do

  end function asIntegerList


  !/ =====================================================================================
  function asReal4List( str ) result( ary )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for a list of double precision.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: str    !! string to convert.
    real(sp), allocatable    :: ary(:) !! array of integers.
    !/ -----------------------------------------------------------------------------------
    character(len=:), allocatable :: temp1
    type(string_splitter)         :: split
    integer                       :: i, n
    !/ -----------------------------------------------------------------------------------

    temp1 = containedBy( str, '[{(', ')}]' )

    call strsplt_create( split, temp1, ',' )

    n = split%count()
    allocate( ary(n) )

    do i=1,n
       ary(i) = asReal4( split%get(i) )
    end do

  end function asReal4List


  !/ =====================================================================================
  function asReal8List( str ) result( ary )
    !/ -----------------------------------------------------------------------------------
    !! Parse a string for a list of double precision.
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: str    !! string to convert.
    real(dp), allocatable    :: ary(:) !! array of integers.
    !/ -----------------------------------------------------------------------------------
    character(len=:), allocatable :: temp1
    type(string_splitter)         :: split
    integer                       :: i, n
    !/ -----------------------------------------------------------------------------------

    temp1 = containedBy( str, '[{(', ')}]' )

    call strsplt_create( split, temp1, ',' )

    n = split%count()
    allocate( ary(n) )

    do i=1,n
       ary(i) = asReal8( split%get(i) )
    end do

  end function asReal8List


  !/ =====================================================================================
  function to_string_int32( num, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert 32 bit integer to string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int32),         intent(in) :: num !! number to convert.
    character(*), optional, intent(in) :: fmt !! edit descriptor.
    character(len=:),      allocatable :: str !! formated string.
    !/ -----------------------------------------------------------------------------------
    character(len=64) :: work
    !/ -----------------------------------------------------------------------------------
    
    work = '(I0)'
    if ( present( fmt ) ) then
       write( work, "('(',A,')')" ) fmt
    end if
    write(work,trim(work)) num
    str = trim(work)
    
  end function to_string_int32


  !/ =====================================================================================
  function to_string_int16( num, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert 16 bit integer to string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int16),         intent(in) :: num !! number to convert.
    character(*), optional, intent(in) :: fmt !! edit descriptor.
    character(len=:),      allocatable :: str !! formated string.
    !/ -----------------------------------------------------------------------------------
    character(len=64) :: work
    !/ -----------------------------------------------------------------------------------
    
    work = '(I0)'
    if ( present( fmt ) ) then
       write( work, "('(',A,')')" ) fmt
    end if
    write(work,trim(work)) num
    str = trim(work)
    
  end function to_string_int16


  !/ =====================================================================================
  function to_string_real4( num, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert single precision to string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp),               intent(in) :: num !! number to convert.
    character(*), optional, intent(in) :: fmt !! edit descriptor.
    character(len=:),      allocatable :: str !! formated string.
    !/ -----------------------------------------------------------------------------------
    character(len=64) :: work
    !/ -----------------------------------------------------------------------------------
    
    work = '(G0)'
    if ( present( fmt ) ) then
       write( work, "('(',A,')')" ) fmt
    end if
    write(work,trim(work)) num
    str = trim(work)
    
  end function to_string_real4


  !/ =====================================================================================
  function to_string_real8( num, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert double precision to string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(in) :: num !! number to convert.
    character(*), optional, intent(in) :: fmt !! edit descriptor.
    character(len=:),      allocatable :: str !! formated string.
    !/ -----------------------------------------------------------------------------------
    character(len=64) :: work
    !/ -----------------------------------------------------------------------------------
    
    work = '(G0)'
    if ( present( fmt ) ) then
       write( work, "('(',A,')')" ) fmt
    end if
    write(work,trim(work)) num
    str = trim(work)
    
  end function to_string_real8


  !/ =====================================================================================
  function to_string_int32_vec( ary, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert a vector of 32 bit integers into a list formatted string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int32),         intent(in) :: ary(:) !! ary array of numbers to convert.
    character(*), optional, intent(in) :: fmt    !! fmt output format.
    character(len=:),      allocatable :: str    !! list representation.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    character(len=:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    sfmt = 'I0'
    if ( present( fmt ) ) then
       sfmt = fmt
    end if

    str = '[' // toString( ary(1), sfmt )

    n = size(ary)
    
    do i=2,n
       str = str // ',' // toString( ary(i), sfmt )
    end do

    str = str // ']'
    
  end function to_string_int32_vec


  !/ =====================================================================================
  function to_string_int16_vec( ary, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert a vector of 16 bit integers into a list formatted string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int16),         intent(in) :: ary(:) !! ary array of numbers to convert.
    character(*), optional, intent(in) :: fmt    !! fmt output format.
    character(len=:),      allocatable :: str    !! list representation.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    character(len=:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    sfmt = 'I0'
    if ( present( fmt ) ) then
       sfmt = fmt
    end if

    str = '[' // toString( ary(1), sfmt )

    n = size(ary)
    
    do i=2,n
       str = str // ',' // toString( ary(i), sfmt )
    end do

    str = str // ']'
    
  end function to_string_int16_vec


  !/ =====================================================================================
  function to_string_real4_vec( ary, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert a vector of single precision numbers into a list formatted string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(sp),               intent(in) :: ary(:) !! ary array of numbers to convert.
    character(*), optional, intent(in) :: fmt    !! fmt output format.
    character(len=:),      allocatable :: str    !! list representation.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    character(len=:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    sfmt = 'G0'
    if ( present( fmt ) ) then
       sfmt = fmt
    end if

    str = '[' // toString( ary(1), fmt )

    n = size(ary)
    
    do i=2,n
       str = str // ',' // toString( ary(i), fmt )
    end do

    str = str // ']'

  end function to_string_real4_vec

  
  !/ =====================================================================================
  function to_string_real8_vec( ary, fmt ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert a vector of double precision numbers into a list formatted string.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(in) :: ary(:) !! ary array of numbers to convert.
    character(*), optional, intent(in) :: fmt    !! fmt output format.
    character(len=:),      allocatable :: str    !! list representation.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    character(len=:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    sfmt = 'G0'
    if ( present( fmt ) ) then
       sfmt = fmt
    end if

    str = '[' // toString( ary(1), fmt )

    n = size(ary)
    
    do i=2,n
       str = str // ',' // toString( ary(i), fmt )
    end do

    str = str // ']'

  end function to_string_real8_vec


end module string_tools


!/ =======================================================================================
!/ **                              S T R I N G _ T O O L S                              **
!/ =========================================================================== END FILE ==
