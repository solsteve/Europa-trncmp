!/ ====================================================================== BEGIN FILE =====
!/ **                            C O V A R I A N C E _ M O D                            **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module covariance_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-11-28
  !! license: GPL
  !!
  !!## Covariance Routines
  !!
  !! Collection of procedures for calculating covariance and correlation matrices
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env



  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================





  !/ =====================================================================================
  subroutine covariance( cov, tab, mu, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compute the covariance of a table of samples.
    !! *NOTE* size of cov and mu determine if the table is in row or column order.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),          intent(inout) :: cov(:,:)  !! covariance matrix.
    real(dp),          intent(in)    :: tab(:,:)  !! table of data in either row or column order.
    real(dp),          intent(in)    :: mu(:)     !! precomputed means of the data.
    integer, optional, intent(out)   :: IERR      !! error return
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, k, nv, ns
    real(dp) :: x, y, S, fnm1, iMean, jMean
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR = 0
    if ( present( EMSG ) ) EMSG = 'success'

    nv = size(cov,DIM=1)
    if ( nv.ne.size(cov,DIM=2) ) then
       call standard_error( 'covariance: output matrix is not square', &
            &                1, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    if ( nv.ne.size(mu) ) then
       call standard_error( 'covariance: output matrix does not match mean vector', &
            &                2, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    !/ ----- samples are in the rows -----------------------------------------------------
    if ( nv.eq.size(tab,DIM=1) ) then
       ns   = size(tab,DIM=2)
       fnm1 = real(ns-1,dp)
       do i=1,nv
          iMean = mu(i)
          do j=i,nv
             jMean = mu(j)
             S     = D_ZERO
             do k=1,ns
                x = tab(i,k) - iMean
                y = tab(j,k) - jMean
                S = S + (x*y)
             end do
             cov(i,j) = S / fnm1
             cov(j,i) = S / fnm1
          end do
       end do
       goto 999
    end if

    !/ ----- samples are in the columns --------------------------------------------------
    if ( nv.eq.size(tab,DIM=2) ) then
       ns   = size(tab,DIM=1)
       fnm1 = real(ns-1,dp)
       do i=1,nv
          iMean = mu(i)
          do j=i,nv
             jMean = mu(j)
             S     = D_ZERO
             do k=1,ns
                x = tab(k,i) - iMean
                y = tab(k,j) - jMean
                S = S + (x*y)
             end do
             cov(i,j) = S / fnm1
             cov(j,i) = S / fnm1
          end do
       end do
       goto 999
    end if

    !/ -----------------------------------------------------------------------------------
    call standard_error( 'covariance: output matrix does not match table', &
         &                3, IERR=IERR, EMSG=EMSG )

999 continue
  end subroutine covariance

  !/ =====================================================================================
  subroutine correlate( cor, cov, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Pearson product-moment correlation coefficients
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),          intent(inout) :: cor(:,:)  !! correlation matrix.
    real(dp),          intent(in)    :: cov(:,:)  !! covariance matrix.
    integer, optional, intent(out)   :: IERR      !! error return
    character(:), optional, allocatable, intent(out) :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer :: i,j,n
    real(dp) :: jvar, w
    !/ -----------------------------------------------------------------------------------

    n = size(cov,DIM=1)
    if ( n.ne.size(cov,DIM=2) ) then
       call standard_error( 'correlation: input covariance matrix is not square', &
            &                1, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    if ( size(cor,DIM=1).ne.size(cor,DIM=2) ) then
       call standard_error( 'correlation: output correlation matrix is not square', &
            &                2, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    if ( n.ne.size(cor,DIM=1) ) then
       call standard_error( 'correlation: output correlation matrix has different dims then input', &
            &                3, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    do j=1,n
       jvar = cov(j,j)
       cor(j,j) = D_ONE
       do i=j+1,n
          w = cov(i,j) / sqrt( cov(i,i) * jvar )
          cor(i,j) = w
          cor(j,i) = w
       end do
    end do
        
    999 continue
  end subroutine correlate

end module covariance_mod


!/ =======================================================================================
!/ **                            C O V A R I A N C E _ M O D                            **
!/ =========================================================================== END FILE ==
