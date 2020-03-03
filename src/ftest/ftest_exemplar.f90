!/ ====================================================================== BEGIN FILE =====
!/ **                            F T E S T _ E X E M P L A R                            **
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
!/ ----- Modification History ------------------------------------------------------------
!! author:  Stephen W. Soliday
!! date:    2018-11-22
!! license: GPL
!!
!!##Exemplar Test
!!
!! Provides the interface and procedures for .
!
!/ =======================================================================================
module exptest
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use exemplar_class
  implicit none

  character(*), parameter :: TEST_DATA = '../data/Iris/iris.data'
  character(*), parameter :: TEST_META = '../data/Iris/iris.meta'

  integer, parameter :: NUM_SAMP = 9

  integer, parameter :: NUM_X = 7
  integer, parameter :: NUM_Y = 5



  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  function GenTable( nv, ns ) result( tab )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: nv
    integer, intent(in) :: ns
    real(dp), pointer   :: tab(:,:)
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c
    real(dp) :: x, y, fn
    
    
    allocate( tab(nv,ns) )

    fn = real( nv+ns, dp )

    do c=1,ns
       x = real( c, dp )
       do r=1,nv
          y = real( r, dp )
          tab(r,c) = x+(y/10.0)
       end do
    end do
    
  end function GenTable


  !/ =====================================================================================
  function CompareAB( A, B, tol ) result( v )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                       :: v      
    real(dp), pointer, intent(in) :: A(:,:)
    real(dp), pointer, intent(in) :: B(:,:)
    real(dp),          intent(in) :: tol
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c, nra, nrb, nca, ncb
    real(dp) :: df
    !/ -----------------------------------------------------------------------------------

    v = 0
    nra = size(A,DIM=1)
    nca = size(A,DIM=2)
    nrb = size(B,DIM=1)
    ncb = size(B,DIM=2)

    if (  nra.ne.nrb ) then
       v = 1
       write(ERROR_UNIT,100) nra, nrb
       goto 999
    end if

    if (  nca.ne.ncb ) then
       v = 2
       write(ERROR_UNIT,110) nca, ncb
       goto 999
    end if

    do c=1,nca
       do r=1,nra
          df = A(r,c) - B(r,c)
          if ( -tol.gt.df ) then
             v = 3
             write(ERROR_UNIT,120) r, c, A(r,c), B(r,c)
             goto 999
          end if
          if ( tol.lt.df ) then
             v = 4
             write(ERROR_UNIT,130) r, c, A(r,c), B(r,c)
             goto 999
          end if
       end do
    end do

    write(*,*) 'Match'

999 continue

100 format( 'Row miss-match ',I0,' != ',I0 )
110 format( 'Column miss-match ',I0,' != ',I0 )
120 format( 'Element (',I0,',',I0,') not equal ',E27.20,' < ',E27.20 )
130 format( 'Element (',I0,',',I0,') not equal ',E27.20,' > ',E27.20 )
  end function CompareAB


  !/ =====================================================================================
  subroutine TestSingle
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), pointer :: X0(:,:)
    real(dp), pointer :: X1(:,:)
    real(dp), pointer :: X2(:,:)
    real(dp), pointer :: X3(:,:)
    integer :: rv
    !/ -----------------------------------------------------------------------------------

    X0 => GenTable( NUM_X, NUM_SAMP )

    call write( X0, FMT='EN15.6', FILE='single.data',   META='single.meta' )
    call write( X0, FMT='EN15.6', FILE='single.head',   MODE='H' )
    call write( X0, FMT='EN15.6', FILE='single.nohead', MODE='N' )

    X1 => read( FILE='single.head' )
    X2 => read( FILE='single.data',   META='single.meta' )
    X3 => read( FILE='single.nohead', NS=NUM_SAMP, NX=NUM_X )

    rv = CompareAB( X0, X1, 1.0d-8 )
    rv = CompareAB( X0, X2, 1.0d-8 )
    rv = CompareAB( X0, X3, 1.0d-8 )
  
  end subroutine TestSingle


  !/ =====================================================================================
  subroutine TestPair
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), pointer :: X0(:,:), Y0(:,:)
    type(exemplar_pair_t) :: P1, P2, P3
    integer :: rv
    !/ -----------------------------------------------------------------------------------

    X0 => GenTable( NUM_X, NUM_SAMP )
    Y0 => GenTable( NUM_Y, NUM_SAMP )

    call write_pair( X0, Y0, FMT='EN15.6', FILE='pair.data',   META='pair.meta' )
    call write_pair( X0, Y0, FMT='EN15.6', FILE='pair.head',   MODE='H' )
    call write_pair( X0, Y0, FMT='EN15.6', FILE='pair.nohead', MODE='N' )

    call read_pair( P1, FILE='pair.head' )
    call read_pair( P2, FILE='pair.data',   META='pair.meta' )
    call read_pair( P3, FILE='pair.nohead', NS=NUM_SAMP, NX=NUM_X, NY=NUM_Y )

    rv = CompareAB( X0, P1%X, 1.0d-8 )
    rv = CompareAB( X0, P2%X, 1.0d-8 )
    rv = CompareAB( X0, P3%X, 1.0d-8 )
    rv = CompareAB( Y0, P1%Y, 1.0d-8 )
    rv = CompareAB( Y0, P2%Y, 1.0d-8 )
    rv = CompareAB( Y0, P3%Y, 1.0d-8 )
  
  end subroutine TestPair


end module exptest


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use exptest
  implicit none

  call TestSingle
  call TestPair


end program main

!/ =======================================================================================
!/ **                                F T E S T _ B P N N                                **
!/ =========================================================================== END FILE ==
