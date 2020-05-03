!/ ====================================================================== BEGIN FILE =====
!/ **                                 F T E S T _ P S O                                 **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  Europa is free software: you can redistribute it and/or modify it under the      **
!/ **  terms of the GNU General Public License as published by the Free Software        **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  Europa is distributed in the hope that it will be useful, but WITHOUT ANY        **
!/ **  WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS FOR    **
!/ **  A PARTICULAR PURPOSE. See the GNU General Public License for more details.       **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  Europa. If not, see <http://www.gnu.org/licenses/>.                              **
!/ **                                                                                   **
!/ =======================================================================================
module ftest_pso
  use pso_mod
  use test_models
  use psgraph_mod
  implicit none



  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine plot( fspc, data )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: fspc
    real(dp),     intent(in) :: data(:,:,:)
    !/ -----------------------------------------------------------------------------------
    class(PSGraph), pointer :: ps
    class(PSDraw),  pointer :: pd
    integer  :: i, j, n, m
    real(dp) :: x, last_x, minv, maxv
    !/ -----------------------------------------------------------------------------------

    minv = data(1,1,1)
    maxv = data(1,1,1)
    n = size(data,DIM=2)
    m = size(data,DIM=3)

    do j=1,m
       do i=1,n
          x = data(1,i,j)
          if ( x.lt.minv ) minv = x
          if ( x.gt.maxv ) maxv = x
       end do
    end do

    pd => PSDraw( 7.5d0, 7.5d0, 0.0d0, minv, real(n,dp), maxv/100.0d0 )

    call pd%setRGB( color_blue )
    call pd%drawBorder
    call pd%setRGB( color_black )

    do j=1,m
       last_x = data(1,1,j)

       do i=2,n
          x = data(1,i,j)
          call pd%drawLine( real(i-1,dp), last_x, real(i,dp), x )
          last_x = x
       end do
    end do

    ps => PSGraph( 1 )
    call ps%add( pd, 1, 1.0d0, 1.0d0 )

    call ps%pswrite( fspc )

  end subroutine plot


end module ftest_pso

!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_pso
  implicit none
  !/ -------------------------------------------------------------------------------------

  integer, parameter :: MAXIT = 10000
  integer, parameter :: NTEST = 10
  integer, parameter :: NPOP  = 1000
  integer, parameter :: NPAR  = 60

  integer :: i,j,k,m,n


  type(SPHERE) :: test_mod
  type(PSO) :: optimizer
  real(dp), allocatable :: hist(:,:)
  real(dp), allocatable :: master_hist(:,:,:)
  !/ -------------------------------------------------------------------------------------

  call test_mod%build( NPAR )

  n = test_mod%nMet()

  allocate( master_hist(n,MAXIT,NTEST) )

  call optimizer%build( NPOP, MODEL=test_mod )

  call optimizer%fit( MAXIT, HISTORY=hist, PFMT='ES11.4', MFMT='ES10.4', &
       & W=0.9d0, W_FINAL=0.4d0 )

  do k = 1,NTEST
     do j=1,MAXIT
        do i=1,n
           master_hist(i,j,k) = hist(i,j)
        end do
     end do

     print *, hist(1,MAXIT)
  end do

  call plot( 'test.ps', master_hist )

  deallocate( hist )
  deallocate( master_hist )

end program main


!/ =======================================================================================
!/ **                                 F T E S T _ P S O                                 **
!/ ======================================================================== END FILE =====
