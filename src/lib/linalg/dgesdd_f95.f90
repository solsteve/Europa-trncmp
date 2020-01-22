!/ ====================================================================== BEGIN FILE =====
!/ **                                D G E S D D _ F 9 5                                **
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
!
!  2015-11-14 ( Modified from LAPACK95 / la_dgesdd.f90
!       -- LAPACK95 interface driver routine (version 3.0) --
!                   UNI-C, Denmark; Univ. of Tennessee, USA; NAG Ltd., UK
!                   September, 2000 )
!
!
! @note     The routine returns V^H , not V .
!
!/ =======================================================================================
SUBROUTINE dgesdd_f95(a, s, u, vt, ww, job, info )
  !/ -------------------------------------------------------------------------------------
  !! ## Singular Value Decomposition
  !!
  !! compute the singular values and, 
  !! optionally, the left and/or right singular vectors from the singular
  !! value decomposition  (SVD) of a real or complex m by n matrix A. The
  !! SVD of A is written 
  !!     $$A = U \cdot \Sigma \cdot V^H$$
  !! where SIGMA is an  m by n matrix which is zero except for its 
  !! min(m, n) diagonal elements, U is an m by m orthogonal (unitary) 
  !! matrix, and V is an n by n orthogonal (unitary) matrix. The diagonal
  !! elements of SIGMA , i.e., the values 
  !!     $$\sigma_i = \Sigma_{i,i} \forall i = 1, 2,...$$
  !! min(m, n) are the singular values of A; they are real and non-negative,
  !! and are returned in descending order. The first min(m, n) columns of U and V 
  !! are the left and right singular vectors of A, respectively.
  !! LA_GESDD solves the same problem as LA_GESVD but uses a divide and 
  !! conquer method if singular vectors are desired. For large matrices it
  !! is usually much faster than LA_GESVD when singular vectors are 
  !! desired, but uses more workspace.
  !/ -------------------------------------------------------------------------------------
  USE tc_lapack_f77
  USE tc_lapack, ONLY: erinfo => erinfo_borrowed, lsame => lsame_borrowed
  IMPLICIT NONE
  !/ -------------------------------------------------------------------------------------
  REAL(dp), INTENT(inout) :: a(:,:)
  !! REAL or COMPLEX array, shape (:, :) with  size(A, 1) = m and size(A, 2) = n.
  !! On entry, the matrix A.  On exit, if JOB = 'U' and U is not present,
  !! then A is  overwritten with the first min(m, n) columns of U
  !! (the left singular vectors, stored columnwise).  If JOB = 'V' and VT
  !! is not present, then A is overwritten with the first min(m, n) rows of V^H
  !! (the right singular vectors,  stored rowwise).  In all cases the
  !! original contents of A are destroyed.

  REAL(dp), INTENT(out) :: s(:)
  !! REAL array, shape (:) with size(S) = min(m, n).
  !! The singular values of A, sorted so that S(i) >= S(i+1).

  REAL(dp), TARGET, OPTIONAL, INTENT(out) :: u(:,:)
  !! Optional REAL or COMPLEX array, shape (:, :) with
  !! size(U, 1) = m and size(U, 2) = m or min(m, n).
  !! If size(U, 2) = m, U contains the m by m matrix U .
  !! If size(U; 2) = min(m, n), U contains the first min(m, n) 
  !! columns of U (the left singular vectors, stored columnwise).

  REAL(dp), TARGET, OPTIONAL, INTENT(out) :: vt(:,:)
  !! Optional REAL or COMPLEX array, shape (:, :) with 
  !! size(VT, 1) = n or min(m, n) and size(VT, 2) = n.
  !! If size(VT, 1) = n , VT contains the n by n matrix V^H .
  !! If size(VT, 1) = min(m, n), VT contains the first min(m, n)
  !! rows of V^H (the right singular vectors, stored rowwise).

  REAL(dp), TARGET, OPTIONAL, INTENT(out) :: ww(:)
  !! Optional REAL array, shape (:) with size(WW) = min(m, n) - 1
  !! If INFO > 0, WW contains the unconverged superdiagonal elements
  !! of an upper bidiagonal matrix B whose diagonal is in SIGMA (not
  !! necessarily sorted). B has the same singular values as A.
  !! Note: WW is a dummy argument for LA_GESDD.

  CHARACTER(len=1), OPTIONAL, INTENT(in) :: job
  !! Optional CHARACTER(LEN=1).
  !! = 'N': neither columns of U nor rows of V^H are returned in array A.
  !! = 'U': if U is not present, the first min(m, n) columns of U 
  !! (the left singular vectors) are returned in array A;
  !! = 'V': if VT is not present, the first min(m, n) rows of V^H 
  !! (the right singular vectors) are returned in array A;
  !! Default value: 'N'.

  INTEGER, OPTIONAL, INTENT(out) :: info
  !! Optional INTEGER.
  !! .eq. 0: successful exit.
  !! .lt. 0: if INFO = -i, the i-th argument had an illegal value.
  !! .gt. 0: The algorithm did not converge.
  !! If INFO is not present and an error occurs, then the program is
  !! terminated with an error message.
  !/ -------------------------------------------------------------------------------------
  CHARACTER(len=*), PARAMETER :: srname = 'tc_dgesdd'

  CHARACTER(len=1) :: ljobz, jobz
  INTEGER, SAVE :: lwork = 0
  INTEGER :: n, m, linfo, ld, istat, s1u, s2u, s1vt, s2vt,    &
       &     mn, smlsiz

  REAL(dp), TARGET  :: llu(1,1), llvt(1,1)
  REAL(dp), POINTER :: work(:), w1(:,:), w2(:,:)
  INTEGER,  POINTER :: iwork(:)

  INTRINSIC min, max, present, size

  linfo = 0
  istat = 0
  m = SIZE(a,1)
  n = SIZE(a,2)
  ld = MAX(1,m)
  mn = MIN(m,n)

  IF ( PRESENT(job) ) THEN
     ljobz = job
  ELSE
     ljobz = 'n'
  END IF

  IF ( PRESENT(u) ) THEN
     s1u = SIZE(u,1)
     s2u = SIZE(u,2)
  ELSE
     s1u = 1
     s2u = 1
  END IF

  IF ( PRESENT(vt) ) THEN
     s1vt = SIZE(vt,1)
     s2vt = SIZE(vt,2)
  ELSE
     s1vt = 1
     s2vt = 1
  END IF
  !  .. test the arguments
  IF ( m < 0 .OR. n < 0 ) THEN
     linfo = -1
  ELSE IF ( SIZE( s ) /= mn ) THEN
     linfo = -2
  ELSE IF ( PRESENT(u) .AND. ( s1u /= m .OR. ( s2u /= m .AND. s2u /= mn ) ) ) THEN
     linfo = -3
  ELSE IF ( PRESENT(vt) .AND. ( ( s1vt /= n .AND. s1vt /= mn ) .OR. s2vt /= n ) ) THEN
     linfo = -4
  ELSE IF (.NOT. (lsame(ljobz, 'n') .OR. lsame(ljobz,'u') .OR. lsame(ljobz, 'v'))) THEN 
     linfo = -6
  ELSE

     smlsiz = ilaenv( 9, 'dgesdd', ' ', 0, 0, 0, 0 )
     lwork = MAX(MAX(14*MIN(m,n)+4, 10*MIN(m,n)+2+smlsiz*(smlsiz+8)) + MAX(m,n), &
          &         5*MIN(m,n)*MIN(m,n) + MAX(m,n) + 9*MIN(m,n))

     ALLOCATE(work(lwork), stat=istat)

     IF ( istat == 0 ) THEN
        ALLOCATE(iwork(8*MIN(m,n)), stat=istat)
        IF ( istat == 0 ) THEN
           IF (.NOT.PRESENT(u) .AND. .NOT. PRESENT(vt)) THEN
              ALLOCATE(w1(m,m), w2(n,n), stat=istat)
              IF (istat == 0) THEN
                 IF (.NOT. lsame(ljobz, 'n')) THEN
                    jobz = 's'
                 ELSE
                    jobz = 'n'
                 END IF
                 CALL dgesdd_f77(jobz, m, n, a, ld, s, w1, MAX(1,m), &
                      &                 w2, MAX(1,n), work, lwork, iwork, linfo )

                 SELECT CASE(ljobz)
                 CASE ('u')
                    a(1:mn, 1:mn) = w1(1:mn, 1:mn)
                 CASE ('v')
                    a(1:mn, 1:mn) = w2(1:mn, 1:mn)
                 END SELECT

                 DEALLOCATE(w1, w2)
              ELSE
                 linfo = -100
              END IF
           ELSE IF (.NOT. PRESENT(u) .AND. PRESENT(vt)) THEN
              jobz = 'a'
              ALLOCATE(w1(m,m), stat=istat)
              IF (istat == 0) THEN
                 IF (PRESENT (vt)) THEN
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, w1, MAX(1,m), &
                         &                 vt, MAX(1, s1vt), work, lwork, iwork, linfo )
                 ELSE
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, w1, MAX(1,m), &
                         &                 llvt, MAX(1, s1vt), work, lwork, iwork, linfo )
                 END IF

                 IF (lsame(ljobz, 'u')) THEN
                    a(1:m, 1:mn) = w1(1:m, 1:mn)
                 END IF

                 DEALLOCATE(w1)
              ELSE
                 linfo = -100
              END IF
           ELSE IF (PRESENT(u) .AND. .NOT. PRESENT(vt)) THEN
              jobz = 'a'
              ALLOCATE(w2(n,n), stat=istat)
              IF (istat == 0) THEN
                 IF (PRESENT (u)) THEN
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, u, MAX(1,s1u), &
                         &                 w2, MAX(1,n), work, lwork, iwork, linfo )
                 ELSE
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, llu, MAX(1,s1u), &
                         &                 w2, MAX(1,n), work, lwork, iwork, linfo )
                 END IF

                 IF (lsame(ljobz, 'v')) THEN
                    a(1:mn, 1:n) = w2(1:mn, 1:n)
                 END IF

                 DEALLOCATE(w2)
              ELSE
                 linfo = -100
              END IF
           ELSE
              jobz = 'a'
              IF (PRESENT (vt)) THEN
                 IF (PRESENT (u)) THEN
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, u, MAX(1,s1u), &
                         &          vt, MAX(1,s1vt), work, lwork, iwork, linfo )
                 ELSE
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, llu, MAX(1,s1u), &
                         &          vt, MAX(1,s1vt), work, lwork, iwork, linfo )
                 END IF
              ELSE
                 IF (PRESENT (u)) THEN
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, u, MAX(1,s1u), &
                         &          llvt, MAX(1,s1vt), work, lwork, iwork, linfo )
                 ELSE
                    CALL dgesdd_f77(jobz, m, n, a, ld, s, llu, MAX(1,s1u), &
                         &          llvt, MAX(1,s1vt), work, lwork, iwork, linfo )
                 END IF
              END IF
              IF (PRESENT(ww)) THEN
                 ww = work(1)
              END IF
           END IF
        ELSE
           linfo = -100
        END IF
        DEALLOCATE(work)
     ELSE
        linfo = -100
     END IF
  END IF

  CALL erinfo(linfo,srname,info,istat)


END SUBROUTINE dgesdd_f95


!/ =======================================================================================
!/ **                                D G E S D D _ F 9 5                                **
!/ =========================================================================== END FILE ==
