!/ ====================================================================== BEGIN FILE =====
!/ **                                 T C _ L A P A C K                                 **
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
module tc_lapack
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-01-15
  !! license: GPL
  !!
  !!## Interface for LAPACK Routines.
  !!
  !! Collection of interfaces to Lapack procedures.
  !
  !/ -------------------------------------------------------------------------------------
  use tc_lapack_f77
  implicit none

  !/ =====================================================================================
  INTERFACE tc_dgesdd !/ ----- singular value decomposition  -- a=usv' -------------------
     !/ ==================================================================================
     SUBROUTINE dgesdd_f95(a, s, u, vt, ww, job, info )
       USE iso_fortran_env
       REAL(real64),                   INTENT(inout) :: a(:,:)  !! Matrix to factor (destroyed)
       REAL(real64),                   INTENT(out)   :: s(:)    !! Singular Values
       REAL(real64), TARGET, OPTIONAL, INTENT(out)   :: u(:,:)  !! Columns = Left  Singular Vectors
       REAL(real64), TARGET, OPTIONAL, INTENT(out)   :: vt(:,:) !! Rows    = Right Singular Vectors
       REAL(real64), TARGET, OPTIONAL, INTENT(out)   :: ww(:)   !! Work Array
       CHARACTER(len=1),     OPTIONAL, INTENT(in)    :: job     !! default 'N'
       INTEGER,              OPTIONAL, INTENT(out)   :: info    !! zero on success
     END SUBROUTINE dgesdd_f95
  END INTERFACE tc_dgesdd


  !/ =====================================================================================
  INTERFACE tc_dgetri !/ ----- inverse of matrix using LU factorization ------------------
     !/ ==================================================================================
     SUBROUTINE dgetri_f95( a, ipiv, info )
       USE iso_fortran_env
       REAL(real64),      INTENT(INOUT) :: a(:,:)
       INTEGER,           INTENT(IN)    :: ipiv(:)
       INTEGER, OPTIONAL, INTENT(OUT)   :: info
     END SUBROUTINE dgetri_f95
  END INTERFACE tc_dgetri


  !/ =====================================================================================
  INTERFACE tc_dgetrf !/ ----- LU factorization -- A = P * L * U -------------------------
     !/ ==================================================================================
     SUBROUTINE dgetrf_f95( a, ipiv, rcond, norm, rnorm, info )
       USE iso_fortran_env
       REAL(real64),               INTENT(INOUT) :: a(:,:)
       INTEGER,  TARGET, OPTIONAL, INTENT(OUT)   :: ipiv(:)
       REAL(real64),     OPTIONAL, INTENT(OUT)   :: rcond
       CHARACTER(LEN=1), OPTIONAL, INTENT(IN)    :: norm
       REAL(real64),     OPTIONAL, INTENT(OUT)   :: rnorm
       INTEGER,          OPTIONAL, INTENT(OUT)   :: info
     END SUBROUTINE dgetrf_f95
  END INTERFACE tc_dgetrf


  INTERFACE erinfo
     MODULE PROCEDURE :: erinfo_borrowed
  END INTERFACE erinfo


  INTERFACE lsame
     MODULE PROCEDURE :: lsame_borrowed
  END INTERFACE lsame




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  !/ =====================================================================================
  SUBROUTINE erinfo_borrowed(linfo, srname, info, istat)
    !/ -----------------------------------------------------------------------------------
    !
    !  -- lapack95 interface driver routine (version 3.0) --
    !     uni-c, denmark; univ. of tennessee, usa; nag ltd., uk
    !     september, 2000
    !
    !/ -----------------------------------------------------------------------------------
    IMPLICIT NONE
    INTEGER             , INTENT(in)  :: linfo
    CHARACTER( len = * ), INTENT(in)  :: srname
    INTEGER, OPTIONAL   , INTENT(out) :: info
    INTEGER, OPTIONAL   , INTENT(in)  :: istat
    !/ -----------------------------------------------------------------------------------
    IF ( ( ( linfo < 0 .AND. linfo > -200 ) .OR. linfo > 0 ) .AND. .NOT.PRESENT(info) ) THEN
       WRITE (*,*) 'program terminated in lapack95 subroutine ',srname
       WRITE (*,*) 'error indicator, info = ',linfo
       IF ( PRESENT(istat) )THEN
          IF ( istat /= 0 ) THEN
             IF ( linfo == -100 )THEN
                WRITE (*,*) 'the statement allocate causes status = ', istat
             ELSE
                WRITE (*,*) 'linfo = ', linfo, ' not expected'
             END IF
          END IF
       END IF
       STOP
    ELSE IF ( linfo <= -200 ) THEN
       WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++'
       WRITE(*,*) '*** warning, info = ', linfo, ' warning ***'
       IF ( linfo == -200 )THEN
          WRITE(*,*) 'could not allocate sufficient workspace for the optimum'
          WRITE(*,*) 'blocksize, hence the routine may not have performed as'
          WRITE(*,*) 'efficiently as possible'
       ELSE
          WRITE(*,*) 'unexpected warning'
       END IF
       WRITE(*,*) '++++++++++++++++++++++++++++++++++++++++++++++++'
    END IF

    IF ( PRESENT(info) ) THEN
       info = linfo
    END IF
  END SUBROUTINE erinfo_borrowed


  !/ =====================================================================================
  LOGICAL FUNCTION lsame_borrowed( ca, cb )
    !/ -----------------------------------------------------------------------------------
    !
    !  -- lapack95 interface driver routine (version 3.0) --
    !     uni-c, denmark; univ. of tennessee, usa; nag ltd., uk
    !     september, 2000
    !
    !/ -----------------------------------------------------------------------------------
    !
    !  purpose
    !  =======
    !
    !  lsame  tests if ca is the same letter as cb regardless of case.
    !
    !  parameters
    !  ==========
    !
    !  ca      (input) character*1
    !  cb      (input) character*1
    !          characters to be compared.
    !
    !  .. scalar arguments ..
    CHARACTER(len=1), INTENT(in) :: ca, cb
    !  .. parameters ..
    !    INTEGER, PARAMETER      :: ioff=32
    !  .. local scalars ..
    INTEGER                 :: inta, intb, zcode
    !  .. intrinsic functions ..
    INTRINSIC                  ichar
    !
    !  .. executable statements ..
    !
    !  test if the characters are equal
    !
    lsame_borrowed = (ca .EQ. cb)
    !
    !  now test for equivalence
    !
    IF ( .NOT.lsame_borrowed )THEN
       !
       !     use 'z' rather than 'a' so that ascii can be detected on prime
       !     machines, on which ichar returns a value with bit 8 set.
       !     ichar('a') on prime machines returns 193 which is the same as
       !     ichar('a') on an ebcdic machine.
       !
       zcode = ICHAR( 'z' )
       !
       inta = ICHAR( ca )
       intb = ICHAR( cb )
       !
       IF ( zcode.EQ.90 .OR. zcode.EQ.122 )THEN
          !
          !        ascii is assumed - zcode is the ascii code of either lower or
          !        upper case 'z'.
          !
          IF ( inta.GE.97 .AND. inta.LE.122 ) inta = inta - 32
          IF ( intb.GE.97 .AND. intb.LE.122 ) intb = intb - 32
          !
       ELSE IF ( zcode.EQ.233 .OR. zcode.EQ.169 )THEN
          !
          !        ebcdic is assumed - zcode is the ebcdic code of either lower or
          !        upper case 'z'.
          !
          IF ( inta.GE.129 .AND. inta.LE.137 .OR.                         &
                                !    &       inta.ge.145 .and. inta.le.153 .or. &
               &       inta.GE.162 .AND. inta.LE.169 ) inta = inta + 64
          IF ( intb.GE.129 .AND. intb.LE.137 .OR.                         &
               &       intb.GE.145 .AND. intb.LE.153 .OR.                         &
               &       intb.GE.162 .AND. intb.LE.169 ) intb = intb + 64
          !
       ELSE IF ( zcode.EQ.218 .OR. zcode.EQ.250 )THEN
          !
          !        ascii is assumed, on prime machines - zcode is the ascii code
          !        plus 128 of either lower or upper case 'z'.
          !
          IF ( inta.GE.225 .AND. inta.LE.250 ) inta = inta - 32
          IF ( intb.GE.225 .AND. intb.LE.250 ) intb = intb - 32
       ENDIF
       lsame_borrowed = (inta .EQ. intb)
    ENDIF
  END FUNCTION lsame_borrowed

END module tc_lapack


!/ =======================================================================================
!/ **                                 T C _ L A P A C K                                 **
!/ =========================================================================== END FILE ==
