!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ H A S H M A P                             **
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
module ftest_hashmap
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-06-20
  !! license: GPL
  !!
  !!##Test of .
  !!
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use hash_object_class
  use poly_cast_mod
  implicit none


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  function get_integer( range ) result( z )
    !/ -----------------------------------------------------------------------------------
    integer, intent( in ) :: range
    integer               :: z
    !/ -----------------------------------------------------------------------------------
    real(dp) :: a

    call RANDOM_NUMBER( a )

    z = modulo( int( a * 1073741823.0 ), range ) + 1

  end function get_integer


  !/ =====================================================================================
  subroutine test01( SAMPLES )
    !/ -----------------------------------------------------------------------------------
    integer, intent( in ) :: SAMPLES
    !/ -----------------------------------------------------------------------------------

    type(HashMap) :: map
    integer     :: i, a, b, x
    integer, allocatable, dimension(:) :: table
    integer, allocatable, dimension(:) :: hist
    type(hashmap_node) :: test

    call create( map, 8 )

    allocate( table( SAMPLES ) )
    allocate( hist( SAMPLES ) )

    do i=1,SAMPLES
       table(i) = i
       hist(i)  = 0
    end do

    call random_seed

    do i=1,SAMPLES*3
       a = get_integer( SAMPLES )
       b = get_integer( SAMPLES )
       if ( a.ne.b ) then
          x        = table(a)
          table(a) = table(b)
          table(b) = x
       end if
    end do

    do i=1,SAMPLES
       call map%set( toObject( table(i) ), toObject( table(i) ) )
    end do

    do i=1,SAMPLES
       write(*,*) i, castInteger( map%get( toObject( i ) ) )
    end do
    
    write(*,*)
    write(*,*) '----- iterator test -',map%size(),'---------------'
    write(*,*)

    call map%rewind

    testloop: do
       if ( .not. map%hasNext() ) exit testloop

       call map%nextNode( test )

       a = castInteger( test%key )
       b = castInteger( test%object )

       write(*,*) a, b

       hist(a) = hist(a) + 1

    end do testloop

    x = 0
    do i=1,SAMPLES
       if ( 0.eq.hist(i) ) write(*,*) 'empty    ',i
       if ( 1.lt.hist(i) ) write(*,*) 'too many ',i,hist(i)
       x = x + hist(i)
    end do

    write(*,*) 'totals',x,map%size()
    

    deallocate( table )  

  end subroutine test01


  !/ =====================================================================================
  subroutine test02( SAMPLES )
    !/ -----------------------------------------------------------------------------------
    integer, intent( in ) :: SAMPLES
    !/ -----------------------------------------------------------------------------------

    write(*,*) 'These numbers should all be different'

    write(*,*) hash( 'heather',    1000 )
    write(*,*) hash( 'anthea',     1000 )
    write(*,*) hash( 'cassiopeia', 1000 )

    write(*,*) hash( 23,     1000 )
    write(*,*) hash( 17731,  1000 )
    write(*,*) hash( 187621, 1000 )

    write(*,*) hash( 2.3_sp,     1000 )
    write(*,*) hash( 177.31_sp,  1000 )
    write(*,*) hash( 18762.1_sp, 1000 )

    write(*,*) hash( 2.3_dp,     1000 )
    write(*,*) hash( 177.31_dp,  1000 )
    write(*,*) hash( 18762.1_dp, 1000 )


  end subroutine test02


  !/ =====================================================================================
  subroutine test03( line )
    !/ -----------------------------------------------------------------------------------
    use string_tools
    !/ -----------------------------------------------------------------------------------
    character(*), intent(in)  :: line
    integer                   :: n
    type(string_splitter)     :: SP
    character(:), allocatable :: S1, key, val, com

    write(*,*) 'line = ',line

    call split( SP, line, ';', COUNT=n )
    if ( 2.eq.n ) then
       S1  = SP%get(1)
       com = SP%get(2)
       call split( SP, S1, '=', COUNT=n )
       if ( 2.eq.n ) then
          key = SP%get(1)
          val = SP%get(2)          
       else
          write(*,*) 'parse error, comment + key without value'
       end if
    else
       call split( SP, line, '=', COUNT=n )
       if ( 2.eq.n ) then
          key = SP%get(1)
          val = SP%get(2)          
       else
          write(*,*) 'parse error, key without value'
       end if
    end if

    if ( allocated( key ) ) write(*,*) '  key     = ',key
    if ( allocated( val ) ) write(*,*) '  value   = ',val
    if ( allocated( com ) ) write(*,*) '  comment = ',com

  end subroutine test03

end module ftest_hashmap


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_hashmap
  implicit none
  !/ -------------------------------------------------------------------------------------
  integer, parameter :: SAMPLES = 64
  
  write (*,*)
  call test01( SAMPLES )
  write (*,*)
  write (*,*) '--------------------------------------------------'
  write (*,*)
  call test02( SAMPLES )
  write (*,*)

end program main

!/ =======================================================================================
!/ **                               F T E S T _ B T R E E                               **
!/ =========================================================================== END FILE ==
