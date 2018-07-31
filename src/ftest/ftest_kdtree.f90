!/ ====================================================================== BEGIN FILE =====
!/ **                              F T E S T _ K D T R E E                              **
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
module ftest_kdtree
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-07-30
  !! license: GPL
  !!
  !!##Test of .
  !!
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use KDTree_class
  implicit none

  integer, public, parameter :: SAMPLES = 1000000
  integer, public, parameter :: KDIMS   = 5


  interface ary_print
     module procedure :: ary_print_1D
     module procedure :: ary_print_2D
  end interface ary_print

  public :: ary_print


  public :: wiki_test

  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function ary_print_1D( ary, fmt ) result( buffer )
    !/ -----------------------------------------------------------------------------------
    implicit None
    real(dp), dimension(:), intent(in) :: ary
    character(*),           intent(in) :: fmt
    character(:), allocatable          :: buffer
    character(32) :: number, sfmt
    integer       :: i,n
    n = size(ary)
    write( sfmt, 100 ) fmt
100 format( '(',A,')' )
    buffer = '('
    write( number, sfmt ) ary(1)
    buffer = buffer // trim(ADJUSTL(number))
    do i=2,n
       buffer = buffer // ', '
       write( number, sfmt ) ary(i)
       buffer = buffer // trim(ADJUSTL(number))
    end do
    buffer = buffer // ')'
  end function ary_print_1D


  !/ =====================================================================================
  function ary_print_2D( ary, fmt ) result( buffer )
    !/ -----------------------------------------------------------------------------------
    implicit None
    real(dp), dimension(:,:), intent(in) :: ary
    character(*),             intent(in) :: fmt
    character(:), allocatable            :: buffer
    integer :: i,n

    n = size(ary,DIM=2)
    buffer = ary_print_1d( ary(:,1), fmt )
    do i=2,n
       buffer = buffer // ary_print_1d( ary(:,i), fmt )
    end do
  end function ary_print_2D



  !/ =====================================================================================
  subroutine print_results( name, test, found, dist, visit, intree, &
       &                    true_idx, true_dist, true_found, fmt )
    !/ -----------------------------------------------------------------------------------
    !! Helper procedure to display results.
    !/ -----------------------------------------------------------------------------------
    implicit None
    character(*),           intent(in) :: name       !! Results name.
    real(dp), dimension(:), intent(in) :: test       !! Test point.
    real(dp), dimension(:), intent(in) :: found      !! Found point.
    real(dp),               intent(in) :: dist       !! Distance to the found point.
    integer,                intent(in) :: visit      !! Number of tree nodes visited.
    integer,                intent(in) :: intree     !! Number of tree nodes inserted.
    integer,                intent(in) :: true_idx   !! True index.
    real(dp),               intent(in) :: true_dist  !! True distance.
    real(dp), dimension(:), intent(in) :: true_found !! True closest point.
    character(*),           intent(in) :: fmt        !! Printed format.
    !/ -----------------------------------------------------------------------------------

    write(*,1000) name
    write(*,1010) ary_print( test, fmt )
    write(*,1020) ary_print( found, fmt ), dist
    write(*,1030) visit, intree
    write(*,*)
    write(*,1040) true_idx, true_dist, ary_print( true_found, fmt )

    write(*,*)
    write(*,*)

1000 format( '>> ',A )
1010 format( 'searching for ',A )
1020 format( 'found ',A,' dist ', F7.4 )
1030 format( 'seen ',I0,' nodes out of ',I0 )
1040 format( 'Exhaustive idx: ',I0,' dist: ', F7.4,' ', A )

  end subroutine print_results


  
  !/ =====================================================================================
  subroutine wiki_test
    !/ -----------------------------------------------------------------------------------
    implicit None
    real(dp), parameter, dimension(2)   :: test_point = [ 9,2 ]
    real(dp),            dimension(2,6) :: wiki
    real(dp), allocatable, dimension(:) :: found
    real(dp)                            :: best_dist, true_dist
    class(kd_tree_t), pointer           :: tree
    integer                             :: true_idx
    !/ -----------------------------------------------------------------------------------

    wiki = reshape( [ 2,3, 5,4, 9,6, 4,7, 8,1, 7,1 ], shape( wiki ) )

    tree => KDTree( list=wiki )
    call tree%search( found, test_point, dist=best_dist )
    true_idx = exhaustive_search( wiki, test_point, dist=true_dist )

    call print_results( 'Wiki', test_point, found, best_dist,                 &
       &                tree%visited(), size( tree ),                         &
       &                true_idx, true_dist, wiki(:,true_idx), 'F7.4' )


  end subroutine wiki_test

  
  !/ =====================================================================================
  subroutine million_test
    !/ -----------------------------------------------------------------------------------
    implicit None
    real(dp), allocatable, dimension(:)   :: test_point
    real(dp), allocatable, dimension(:,:) :: million
    real(dp), allocatable, dimension(:)   :: found
    real(dp)                              :: best_dist, true_dist
    class(kd_tree_t), pointer             :: tree
    integer                               :: true_idx, i, j
    !/ -----------------------------------------------------------------------------------

    call RANDOM_SEED

    allocate( million(KDIMS,SAMPLES) )
    allocate( test_point(KDIMS) )

    do j=1,SAMPLES
       do i=1,KDIMS
          call RANDOM_NUMBER( million(i,j) )
       end do
    end do

    do i=1,KDIMS
       call RANDOM_NUMBER( test_point(i) )
    end do

    !/ -----------------------------------------------------------------------------------

    tree => KDTree( list=million )
    call tree%search( found, test_point, dist=best_dist )
    true_idx = exhaustive_search( million, test_point, dist=true_dist )

    call print_results( 'Million', test_point, found, best_dist,                 &
         &              tree%visited(), size( tree ),                            &
         &              true_idx, true_dist, million(:,true_idx), 'F7.4' )


  end subroutine million_test

  
end module ftest_kdtree






!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_kdtree
  implicit none


  call wiki_test
  call million_test

    
end program main

!/ =======================================================================================
!/ **                              F T E S T _ K D T R E E                              **
!/ =========================================================================== END FILE ==
