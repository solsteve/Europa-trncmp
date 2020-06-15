!/ ====================================================================== BEGIN FILE =====
!/ **                                D G E T R F _ F 9 5                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
!
!> @brief 
!!
!! @author Stephen W. Soliday
!!
!! @date 2015-12-17 ( Modified from LAPACK95 / la_dgetrf.f90
!!                  -- LAPACK95 interface driver routine (version 3.0) --
!!                     UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!!                     September, 2000 )
!!   
!! @details computes an LU factorization of a general M-by-N matrix A
!!          using partial pivoting with row interchanges.
!!          The factorization has the form   A = P * L * U
!!          where P is a permutation matrix, L is lower triangular with unit
!!          diagonal elements (lower trapezoidal if m > n), and U is upper
!!          triangular (upper trapezoidal if m < n).
!!          This is the right-looking Level 3 BLAS version of the algorithm.
!!   
!! @param[inout] A      A is DOUBLE PRECISION array, dimension (LDA,N)
!!                      On entry, the M-by-N matrix to be factored.
!!                      On exit, the factors L and U from the factorization
!!                      A = P*L*U; the unit diagonal elements of L are not stored.
!!
!! @param[out]   IPIV   Optional  INTEGER array, shape (:) with size(IPIV) = 
!!                      size(A,1).
!!                      The pivot indices that define the permutation matrix P; row i of
!!                      the matrix was interchanged with row IPIV(i).
!!
!! @param[out]   RCOND  Optional REAL*8 return the condition number.
!!                      The reciprocal of the condition number of the matrix A,
!!                      computed as RCOND = 1/(norm(A) * norm(inv(A))).
!!
!! @param[in]    NORM   CHARACTER*1 ( M,m,1,0,o,I,i,F,f,E, or e )
!!                      Used to compute the Conditional number
!!                      LANORM = ( max(abs(A(i,j))), NORM = 'M' or 'm'
!!                               (
!!                               ( norm1(A),         NORM = '1', 'O' or 'o'
!!                               (
!!                               ( normI(A),         NORM = 'I' or 'i'
!!                               (
!!                               ( normF(A),         NORM = 'F', 'f', 'E' or 'e'
!!
!!                      where  norm1  denotes the  one norm of a matrix (maximum column sum),
!!                      normI  denotes the  infinity norm  of a matrix  (maximum row sum) and
!!                      normF  denotes the  Frobenius norm of a matrix (square root of sum of
!!                      squares).  Note that  max(abs(A(i,j)))  is not a consistent matrix norm.
!!
!! @param[out]   RNORM  Optional REAL*8 return the computed norm. Requires RCOND to be present.
!!
!! @param[out]   INFO   INTEGER
!!                         = 0:  successful exit
!!                         < 0:  if INFO = -i, the i-th argument had an illegal value
!!                         > 0:  if INFO = i, U(i,i) is exactly zero. The factorization
!!                      has been completed, but the factor U is exactly
!!                      singular, and division by zero will occur if it is used
!!                      to solve a system of equations.
!!
!
!/ =======================================================================================
subroutine DGETRF_F95( A, IPIV, RCOND, NORM, RNORM, INFO )
  !/ -------------------------------------------------------------------------------------
  use tc_lapack_f77
  use tc_lapack, only: erinfo => erinfo_borrowed, lsame => lsame_borrowed
  implicit none
  !/ -------------------------------------------------------------------------------------
  real(dp),                   intent(INOUT) :: A(:,:)
  integer, target,  optional, intent(OUT)   :: IPIV(:)
  real(dp),         optional, intent(OUT)   :: RCOND
  character(LEN=1), optional, intent(IN)    :: NORM
  real(dp),         optional, intent(OUT)   :: RNORM
  integer,          optional, intent(OUT)   :: INFO
  !/ -------------------------------------------------------------------------------------
  character(LEN=8), parameter :: SRNAME = 'LA_GETRF'

  character(LEN=1) :: LNORM
  integer  :: LINFO, M, N, LD, ISTAT, ISTAT1, MINMN, LWORK, SIPIV
  real(dp) :: LANORM

  integer,  pointer :: LIPIV(:), IWORK(:)
  real(dp), pointer :: WORK(:)

  !intrinsic PRESENT, MAX, MIN, SIZE, TINY

  M = size(A,1)
  N = size(A,2)
  LINFO = 0
  ISTAT = 0
  MINMN = min(M,N)
  LD = max(1,M)
  if( present(IPIV) )then
     SIPIV = size(IPIV)
  else
     SIPIV = MINMN
  endif
  if ( present(NORM) ) then
     LNORM = NORM
  else
     LNORM = '1'
  end if
  !  .. TEST THE ARGUMENTS
  if( M < 0 .or. N < 0 .or. present(RCOND) .and. M /= N )then
     LINFO = -1
  else if( SIPIV /= MINMN )then
     LINFO = -2
  else if( ( .not.present(RCOND) .and. present(NORM) ) .or. &
       ( .not.LSAME(LNORM,'I') .and. .not.LSAME(LNORM,'O') &
       .and. LNORM /= '1' ) ) then
     LINFO = -4
  else if( M > 0 .and. N > 0 ) then
     if( present(RCOND) .and. M == N ) then
        !        .. COMPUTE THE NORM OF THE MATRIX A
        if( LNORM == 'I' ) then
           LWORK = MINMN
        else
           LWORK = 1
        end if
        allocate( WORK(LWORK), STAT=ISTAT )
        if( ISTAT == 0 )then
           LANORM = DLANGE_F77( LNORM, MINMN, MINMN, A, LD, WORK )
           if ( present(rnorm) ) then
              rnorm = lanorm
           end if
        else
           LINFO = -100
        end if
        deallocate(WORK, STAT=ISTAT1)
     end if
     if( LINFO == 0 ) then
        if( present(IPIV) )then
           LIPIV => IPIV
        else
           allocate( LIPIV( MINMN ), STAT=ISTAT )
        endif
        if( ISTAT /= 0 )LINFO = -100
     end if
     if( LINFO == 0 ) then
        !           .. COMPUTE THE LU FACTORS OF THE MATRIX A
        call DGETRF_F77( M, N, A, LD, LIPIV, LINFO )
        if( .not. present(IPIV) )deallocate( LIPIV, STAT=ISTAT )
        if( present(RCOND) ) then
           !              .. COMPUTE THE RECIPROCAL OF THE CONDITION NUMBER OF A
           if( LANORM <= tiny(1.0d0) .or. M /= N .or. LINFO /= 0 ) then
              RCOND = 0.0d0
           else 
              allocate(WORK(4*MINMN), IWORK(MINMN), STAT=ISTAT)
              if( ISTAT == 0 )then
                 call DGECON_F77( LNORM, MINMN, A, LD, LANORM, &
                      RCOND, WORK, IWORK, LINFO )
              else
                 LINFO = -100
              end if
              deallocate( WORK, IWORK, STAT=ISTAT1 )
           end if
        end if
     end if
  else if( present(RCOND) ) then
     if( M == N )then
        RCOND = 1.0d0
     else
        RCOND = 0.0d0
     end if
  end if
  call ERINFO(LINFO,SRNAME,INFO,ISTAT)
end subroutine DGETRF_F95

!/ =======================================================================================
!/ **                                D G E T R F _ F 9 5                                **
!/ =========================================================================== END FILE ==
