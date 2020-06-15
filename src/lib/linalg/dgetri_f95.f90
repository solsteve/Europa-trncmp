!/ ====================================================================== BEGIN FILE =====
!/ **                                D G E T R I _ F 9 5                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
!
!> @brief 
!!
!! @author Stephen W. Soliday
!!
!! @date 2015-12-17 ( Modified from LAPACK95 / la_dgetri.f90
!!  -- LAPACK95 interface driver routine (version 3.0) --
!!     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!!     September, 2000
!!   
!! @details computes the inverse of a matrix using the LU factorization 
!!          computed by LA_GETRF.
!!
!! @param[inout] A     REAL or COMPLEX square array, shape (:,:),
!!                     size(A,1) == size(A,2).
!!                     On entry contains the factors L and U from the factorization
!!                        A = PLU as computed by LA_GETRF.
!!                     On exit, if INFO = 0, the inverse of the original matrix A.
!!
!! @param[in]    IPIV  INTEGER array, shape (:), size(IPIV) == size(A,1).
!!                     The pivot indices from LA_GETRF; for 1 <= i <= size(A,1), 
!!                     row i of the matrix was interchanged with row IPIV(i).
!!
!! @param[out]   INFO  Optional INTEGER.
!!                     If INFO is present
!!                        = 0: successful exit
!!                        < 0: if INFO = -k, the k-th argument had an illegal value
!!                        > 0: if INFO = k, U(k,k) is exactly zero.  The matrix is
!!                            singular and its inverse could not be computed.
!!                     If INFO is not present and an error occurs, then the program is
!!                        terminated with an error message.
!
!/ =======================================================================================
subroutine DGETRI_F95( A, IPIV, INFO )
  !/ -------------------------------------------------------------------------------------
  use tc_lapack_f77
  use tc_lapack, only: erinfo => erinfo_borrowed
  implicit none
  !/ -------------------------------------------------------------------------------------
  real(dp),          intent(INOUT) :: A(:,:)
  integer,           intent(IN)    :: IPIV(:)
  integer, optional, intent(OUT)   :: INFO
  !/ -------------------------------------------------------------------------------------

  character(LEN=8), parameter :: SRNAME = 'LA_GETRI'
  character(LEN=6), parameter :: BSNAME = 'DGETRI'

  integer    :: LINFO, N, LD, LWORK, ISTAT, ISTAT1, NB

  real(dp), pointer :: WORK(:)

  intrinsic SIZE, MAX

  N     = size(A,1)
  LINFO = 0
  LD    = max(1,N)
  ISTAT = 0

  if( (size( A, 2 ).ne.N).or.(N.lt.0) ) then
     LINFO = -1
  else if( size( IPIV ).ne.N ) then
     LINFO = -2
  else if( N > 0 ) then
     !     DETERMINE THE WORK SPACE.
     NB = ILAENV_F77( 1, BSNAME, ' ', N, -1, -1, -1 )

     if( (NB.lt.1).or.(NB.ge.N) ) then
        NB = 1
     end if

     LWORK = max( N*NB, 1 )

     allocate(WORK(LWORK), STAT=ISTAT)

     if( ISTAT.ne.0 ) then
        deallocate(WORK, STAT=ISTAT1)
        LWORK = max(1,N)
        allocate(WORK(LWORK), STAT=ISTAT)

        if( ISTAT == 0 ) call ERINFO( -200, SRNAME, LINFO )

     end if

     if( LINFO == 0 )then
        call DGETRI_F77( N, A, LD, IPIV, WORK, LWORK, LINFO )
     else
        LINFO = -100
     end if

     deallocate(WORK, STAT=ISTAT1)

  end if

  call ERINFO(LINFO,SRNAME,INFO,ISTAT)

end subroutine DGETRI_F95

!/ =======================================================================================
!/ **                                D G E T R I _ F 9 5                                **
!/ =========================================================================== END FILE ==
