!/ ====================================================================== BEGIN FILE =====
!/ **                             T C _ L A P A C K _ F 7 7                             **
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
module tc_lapack_f77
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-01-15
  !! license: GPL
  !!
  !!## Interface to F77 LAPACK Routines.
  !!
  !! Collection of interfaces to F77 Lapack procedures.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  !/ =====================================================================================
  INTERFACE ilaenv_f77 !/ ----- query local environment ----------------------------------
     !/ ==================================================================================
     FUNCTION ilaenv( ISPEC, NAME, OPTS, N1, N2, N3, N4 )
       INTEGER                      :: ILAENV
       CHARACTER(LEN=*), INTENT(IN) :: NAME, OPTS
       INTEGER,          INTENT(IN) :: ISPEC, N1, N2, N3, N4
     END FUNCTION ilaenv
  END INTERFACE ilaenv_f77


  !/ =====================================================================================
  INTERFACE dgesdd_f77 !/ ----- singular value decomposition  -- a=usv' ------------------
     !/ ==================================================================================
     SUBROUTINE dgesdd( jobz, m, n, a, lda,     &
          &             s, u, ldu, vt, ldvt,    &
          &             work, lwork, iwork, info )
       USE iso_fortran_env
       CHARACTER(len=1), INTENT(IN)    :: jobz
       INTEGER,          INTENT(IN)    :: m
       INTEGER,          INTENT(IN)    :: n
       INTEGER,          INTENT(IN)    :: lda
       INTEGER,          INTENT(IN)    :: ldu
       INTEGER,          INTENT(IN)    :: ldvt
       INTEGER,          INTENT(IN)    :: lwork
       REAL(real64),     INTENT(INOUT) :: a(lda,*)
       REAL(real64),     INTENT(OUT)   :: s(*)
       REAL(real64),     INTENT(OUT)   :: u(ldu,*)
       REAL(real64),     INTENT(OUT)   :: vt(ldvt,*)
       REAL(real64),     INTENT(OUT)   :: work(*)
       INTEGER,          INTENT(OUT)   :: iwork(*)
       INTEGER,          INTENT(OUT)   :: info
     END SUBROUTINE dgesdd
  END INTERFACE dgesdd_f77


  !/ =====================================================================================
  INTERFACE DGETRI_F77 !/ ----- inverse of matrix using LU factorization -----------------
     !/ ==================================================================================
     SUBROUTINE DGETRI( n, A, lda, ipiv, work, lwork, info )
       USE iso_fortran_env
       INTEGER,      INTENT(IN)    :: n
       INTEGER,      INTENT(IN)    :: lda
       INTEGER,      INTENT(IN)    :: lwork
       INTEGER,      INTENT(IN)    :: ipiv(*)
       REAL(real64), INTENT(INOUT) :: A(lda,*)
       REAL(real64), INTENT(OUT)   :: work(lwork)
       INTEGER,      INTENT(OUT)   :: info
     END SUBROUTINE DGETRI
  END INTERFACE DGETRI_F77


  !/ =====================================================================================
  INTERFACE DGETRF_F77 !/ ----- LU factorization -- A = P * L * U ------------------------
     !/ ==================================================================================
     SUBROUTINE DGETRF( M, N, A, LDA, PIV, INFO )
       USE iso_fortran_env
       INTEGER,      INTENT(IN)    :: LDA, M, N
       INTEGER,      INTENT(OUT)   :: INFO
       INTEGER,      INTENT(OUT)   :: PIV(*)
       REAL(real64), INTENT(INOUT) :: A(LDA,*)
     END SUBROUTINE DGETRF
  END INTERFACE DGETRF_F77

  
END MODULE tc_lapack_f77


!/ =======================================================================================
!/ **                             T C _ L A P A C K _ F 7 7                             **
!/ =========================================================================== END FILE ==
