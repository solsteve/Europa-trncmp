!/ ====================================================================== BEGIN FILE =====
!/ **                               F T E S T _ B T R E E                               **
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
module ftest_btree
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-mm-dd
  !! license: GPL
  !!
  !!##Test of .
  !!
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use btree_object_class
  use hash_object_class
  use poly_cast_mod
  implicit none


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  
  
  
  !/ =====================================================================================
  subroutine print_nodes( key, obj )
    !/ -----------------------------------------------------------------------------------
    class(*), pointer, intent(in) :: key !! pointer to a key.
    class(*), pointer, intent(in) :: obj !! pointer to an object.
    !/ -----------------------------------------------------------------------------------

    write(*,*) castInteger( key ), castInteger( obj )
    
  end subroutine print_nodes

  

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

    type(BTree) :: T
    integer     :: i, a, b, x
    integer, allocatable, dimension(:) :: table

    procedure (btree_node_procedure), pointer :: f_ptr => null()

    f_ptr => print_nodes

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
       x = get_integer( SAMPLES*317 )
       call T%insert( toObject( i ), toObject( x ) )
    end do

    call T%execute( f_ptr )

    deallocate( table )  

  end subroutine test01



  !/ =====================================================================================
  subroutine test02( SAMPLES )
    !/ -----------------------------------------------------------------------------------
    integer, intent( in ) :: SAMPLES
    !/ -----------------------------------------------------------------------------------

    type(BTree) :: T
    integer     :: i, a, b, x
    integer, allocatable, dimension(:) :: table
    class(btree_node), pointer :: node => null()

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
       x = get_integer( SAMPLES*317 )
       call T%insert( toObject( i ), toObject( x ) )
    end do

    call T%buildIndex
    
    do i=1,SAMPLES
       node => T%index(i)
       write(*,*) castInteger( node%key ), castInteger( node%object )
    end do

    node => T%find( toObject( 32 ) )
    write(*,*) 'found',32,castInteger( node%object )

    node => T%find( toObject( 70 ), stat=x )
    write(*,*) 'found stat=',x

    write(*,*)
    write(*,*) '---------------------------------------'
    write(*,*)

    call T%rewind

    testloop: do
       if ( .not. T%hasNext() ) exit testloop

       node => T%nextNode()
       
       write(*,*) castInteger( node%key ), castInteger( node%object )

    end do testloop


    deallocate( table )  

  end subroutine test02


  end module ftest_btree

     
!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_btree
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
