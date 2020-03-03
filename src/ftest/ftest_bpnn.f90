!/ ====================================================================== BEGIN FILE =====
!/ **                                F T E S T _ B P N N                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2017, Stephen W. Soliday                                           **
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
!/ ----- Modification History ------------------------------------------------------------
!
!> @brief   .
!!
!! @details Provides the interface and procedures for .
!!
!! @author  Stephen W. Soliday
!! @date    2017-JUL-03
!
!/ =======================================================================================
module bpntest
  !/ -------------------------------------------------------------------------------------
  use exemplar_class
  use bpn2_class
  implicit none

  character(*), parameter :: TEST_DATA = '../data/Iris/iris.data'
  character(*), parameter :: TEST_META = '../data/Iris/iris.meta'


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
!/ =======================================================================================
subroutine MTest
  !/ -------------------------------------------------------------------------------------
  implicit none
  type(exemplar_pair_t)  :: E
  real(dp), parameter :: sig     = 0.36
  class(BPN2), pointer :: net, test_net
  integer             :: num_inp, num_out, num_hid
  !/ -------------------------------------------------------------------------------------

  call read_pair( PAIR=E, FILE=TEST_DATA, META=TEST_META )

  num_inp = size( E%X, 1 )
  num_out = size( E%Y, 1 )
  num_hid = num_inp * num_out

  net => BPN2( num_inp, num_hid, num_out )

  call net%init( sig )
  call net%write( FILE='test.init.net', FMT='E17.10' )

  test_net => BPN2( FILE='test.init.net' )

  write(*,*) 'B1: ', sumsq( net%B1, test_net%B1 )
  write(*,*) 'W1: ', sumsq( net%W1, test_net%W1 )
  write(*,*) 'B2: ', sumsq( net%B2, test_net%B2 )
  write(*,*) 'W2: ', sumsq( net%W2, test_net%W2 )

end subroutine MTest


  
!/ =======================================================================================
subroutine LTest
  !/ -------------------------------------------------------------------------------------
  use file_tools, only : WriteUnit
  implicit none
  integer,  parameter :: n_rep   = 100
  integer,  parameter :: report  = 1000
  real(dp), parameter :: eta     = 0.5
  !/ -------------------------------------------------------------------------------------
  type(exemplar_pair_t)  :: E
  integer             :: m, g, i, j, k, outf
  class(BPN2), pointer :: net
  real(dp) :: cost, err
  !/ -------------------------------------------------------------------------------------

  outf = WriteUnit( FILE='cost.dat' )
  
  call read_pair( PAIR=E, FILE=TEST_DATA, META=TEST_META )
  m = size( E%X, 2 )
  
  net => BPN2( FILE='test.init.net' )

  call net%reset()

  g = 0
  outerlp: do k=1,n_rep
     replp: do j=1,report

        err = 0.0d0
        do i=1,m
           call net%forward( E%X(:,i) )
           call net%backwards( E%X(:,i), E%Y(:,i), CST=cost )
           err = err + cost
        end do

        call net%update(eta/real(m,dp))
        g = g + 1

     end do replp
     write(outf,*) g, err
     write(*,*) g, err
  end do outerlp
  
  call net%write( FILE='test.ftrained.net', FMT='E17.10' )

  
  close( outf )

  
end subroutine LTest



!/ =======================================================================================
subroutine VTest
  !/ -------------------------------------------------------------------------------------
  implicit none
  type(exemplar_pair_t)  :: E
  integer             :: m, i,j
  class(BPN2), pointer :: net
  real(dp), allocatable :: T(:,:)
  !/ -------------------------------------------------------------------------------------
  
  call read_pair( PAIR=E, FILE=TEST_DATA, META=TEST_META )

  net => BPN2( FILE='test.ftrained.net' )

  allocate( T( size(E%Y,1), size(E%Y,2) ) )

  m = size( E%X, 2 )
  
  do i=1,m
     call net%forward( E%X(:,i), T(:,i) )
  end do

  do i=1,m
     do j=1,3
        if ( T(j,i) .lt. 0.5d0 ) T(j,i) = 0.0d0
        if ( T(j,i) .gt. 0.5d0 ) T(j,i) = 1.0d0
     end do
     write(*,100) E%Y(:,i),T(:,i)
  end do
  
  write (*,*) m, sumsq( E%Y, T ) / real(m,dp)

  100 format( 3(F4.2,1X),'==',3(1X,F4.2) )

end subroutine VTest



end module bpntest


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use bpntest
  implicit none

  call MTest
  
  call LTest

  call VTest

end program main

!/ =======================================================================================
!/ **                                F T E S T _ B P N N                                **
!/ =========================================================================== END FILE ==
