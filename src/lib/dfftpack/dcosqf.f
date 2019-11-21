      SUBROUTINE DCOSQF (N,X,WSAVE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION       X(*)       ,WSAVE(*)
      DATA SQRT2 /1.41421356237309504880D0/
      IF (N-2.eq.1) goto 102
      IF (N-2.eq.2) goto 103
      IF (N-2.eq.3) goto 103
  101 TSQX = SQRT2*X(2)
      X(2) = X(1)-TSQX
      X(1) = X(1)+TSQX
  102 RETURN
  103 CALL COSQF1 (N,X,WSAVE,WSAVE(N+1))
      RETURN
      END
