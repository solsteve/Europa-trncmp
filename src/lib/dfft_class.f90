!/ ====================================================================== BEGIN FILE =====
!/ **                                D F F T _ C L A S S                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2029, Stephen W. Soliday                                           **
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
module dfft_class
  !/ -------------------------------------------------------------------------------------
  !! Provides an openmp implementation of a Discrete Fast Fouier Transform.
  !! Acceleration comes from the precalculation of the Sine and Cosine cooeficients.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-09
  !! license: GPL
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use omp_lib

  type DFFT
     integer           :: dim  =  0      !! number of elements in data.

     real(dp), pointer :: W(:) => null()

   contains

     procedure :: build   => dft_build_coefficients
     procedure :: forward => dft_forward_transform
     procedure :: reverse => dft_reverse_transform

     final :: dft_destroy

  end type DFFT


  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  subroutine dft_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(DFFT), intent(inout) :: dts !! reference to a DFFT.
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    if ( associated( dts%W ) ) deallocate( dts%W )
    dts%dim = 0
    
  end subroutine dft_destroy


  !/ =====================================================================================
  subroutine dft_build_coefficients( dts, dim )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DFFT), intent(inout) :: dts  !! reference to this DFFT.
    integer,     intent(inout) :: dim  !! data   dimension.
    !/ -----------------------------------------------------------------------------------
    integer  :: i,  n2
    real(dp) :: aw, arg
    real(dp), pointer :: WW(:)
    !/ -----------------------------------------------------------------------------------
    call dft_destroy( dts )

    allocate( dts%W(dim) )
    WW => dts%W
    dts%dim = dim

    n2 = dim / 2
    aw = D_2PI / real(dim,dp)

    !$omp parallel &
    !$omp   shared ( aw, n2, WW ) &
    !$omp   private ( arg, i )
    !$omp do
    do i = 1, n2
       arg = aw * real(i-1,dp)
       WW(2*i-1) = cos ( arg )
       WW(2*i)   = sin ( arg )
    end do
    !$omp end do
    !$omp end parallel

  end subroutine dft_build_coefficients


  !/ =====================================================================================
  subroutine step ( n, mj, a, b, c, d, w, sgn )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in)    :: n
    integer,  intent(in)    :: mj
    real(dp), intent(inout) :: a(:)
    real(dp), intent(inout) :: b(:)
    real(dp), intent(inout) :: c(:)
    real(dp), intent(inout) :: d(:)
    real(dp), intent(inout) :: w(:)
    real(dp), intent(in)    :: sgn
    !/ -----------------------------------------------------------------------------------
    integer  :: j, ja, jb, jc, jd, jw, k, lj, mj2
    real(dp) :: ambr, ambu, wjw(2)
    !/ -----------------------------------------------------------------------------------

  mj2 = 2 * mj
  lj  = n / mj2

  !$omp parallel &
  !$omp   shared ( a, b, c, d, lj, mj, mj2, sgn, w ) &
  !$omp   private ( ambr, ambu, j, ja, jb, jc, jd, jw, k, wjw )
  !$omp do
  do j = 0, lj - 1

     jw = j * mj
     ja = jw
     jb = ja
     jc = j * mj2
     jd = jc

     wjw(1) = w(jw*2+1) 
     wjw(2) = w(jw*2+2)

     if ( sgn < 0.0D+00 ) then
        wjw(2) = - wjw(2)
     end if

     do k = 0, mj - 1

        c((jc+k)*2+1) = a((ja+k)*2+1) + b((jb+k)*2+1)
        c((jc+k)*2+2) = a((ja+k)*2+2) + b((jb+k)*2+2)

        ambr = a((ja+k)*2+1) - b((jb+k)*2+1)
        ambu = a((ja+k)*2+2) - b((jb+k)*2+2)

        d((jd+k)*2+1) = wjw(1) * ambr - wjw(2) * ambu
        d((jd+k)*2+2) = wjw(2) * ambr + wjw(1) * ambu

     end do
  end do
  !$omp end do
  !$omp end parallel

end subroutine step













  

  !/ =====================================================================================
  subroutine dft_forward_transform( dts, F, S )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DFFT), intent(inout) :: dts   !! reference to this DFFT.
    real(dp),    intent(inout) :: F(:)  !! array of output frequency values.
    real(dp),    intent(inout) :: S(:)  !! array of input  space     values.
    !/ -----------------------------------------------------------------------------------
    integer :: n, j, mj, m
    logical :: tgle
    !/ -----------------------------------------------------------------------------------

    n = dts%dim

    m = int ( ( log( real( n, dp ) ) / log( 1.99D+00 ) ) )
    mj = 1
    !
    !  Toggling switch for work array.
    !
    tgle = .true.
    call step( n, mj, S(1:), S((n/2)*2+1:), F(1:), F(mj*2+1:), dts%W, D_ONE )

    if ( n == 2 ) goto 100

    do j = 1, m - 2  

       mj = mj * 2

       if ( tgle ) then
          call step ( n, mj, F(1:), F((n/2)*2+1:), S(1:), S(mj*2+1:), dts%W, D_ONE )
          tgle = .false.
       else
          call step ( n, mj, S(1:), S((n/2)*2+1:), F(1:), F(mj*2+1:), dts%W, D_ONE )
          tgle = .true.
       end if

    end do
    !
    !  Last pass through data: move Y to X if needed. 
    !
    if ( tgle ) then
       S(1:2*n) = F(1:2*n)
    end if

    mj = n / 2
    call step ( n, mj, S(1:), S((n/2)*2+1:), F(1:), F(mj*2+1:), dts%W, D_ONE )

100 continue


  end subroutine dft_forward_transform


  !/ =====================================================================================
  subroutine dft_reverse_transform( dts, S, F )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(DFFT), intent(inout) :: dts   !! reference to this DFFT.
    real(dp),    intent(inout) :: S(:)  !! array of output space     values.
    real(dp),    intent(inout) :: F(:)  !! array of input  frequency values.
    !/ -----------------------------------------------------------------------------------
    integer :: n, j, mj, m
    logical :: tgle
    !/ -----------------------------------------------------------------------------------

    n = dts%dim

    m = int ( ( log( real( n, dp ) ) / log( 1.99D+00 ) ) )
    mj = 1
    !
    !  Toggling switch for work array.
    !
    tgle = .true.
    call step( n, mj, S(1:), S((n/2)*2+1:), F(1:), F(mj*2+1:), dts%W, -D_ONE )

    if ( n == 2 ) goto 100

    do j = 1, m - 2  

       mj = mj * 2

       if ( tgle ) then
          call step ( n, mj, F(1:), F((n/2)*2+1:), S(1:), S(mj*2+1:), dts%W, -D_ONE )
          tgle = .false.
       else
          call step ( n, mj, S(1:), S((n/2)*2+1:), F(1:), F(mj*2+1:), dts%W, -D_ONE )
          tgle = .true.
       end if

    end do
    !
    !  Last pass through data: move Y to X if needed. 
    !
    if ( tgle ) then
       S(1:2*n) = F(1:2*n)
    end if

    mj = n / 2
    call step ( n, mj, S(1:), S((n/2)*2+1:), F(1:), F(mj*2+1:), dts%W, -D_ONE )

100 continue

  end subroutine dft_reverse_transform



end module dfft_class


!/ =======================================================================================
!/ **                                D F F T _ C L A S S                                **
!/ =========================================================================== END FILE ==
