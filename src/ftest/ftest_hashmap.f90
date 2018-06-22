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
contains !/**                   P R O C E D U R E   S E C T I O N                       **
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

    call create( map, 8 )

    allocate( table( SAMPLES ) )

    do i=1,SAMPLES
       table(i) = i
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
