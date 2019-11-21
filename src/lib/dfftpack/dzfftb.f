      SUBROUTINE DZFFTB (N,R,AZERO,A,B,WSAVE)
      IMPLICIT DOUBLE PRECISION (A-H,O-Z)
      DIMENSION       R(*)       ,A(*)       ,B(*)       ,WSAVE(*)
c      IF (N-2) 101,102,103
      IF (N-2.eq.1) goto 101
      IF (N-2.eq.2) goto 102
      IF (N-2.eq.3) goto 103
  101 R(1) = AZERO
      RETURN
  102 R(1) = AZERO+A(1)
      R(2) = AZERO-A(1)
      RETURN
  103 NS2 = (N-1)/2
      DO 104 I=1,NS2
         R(2*I) = 0.5D0*A(I)
         R(2*I+1) = -0.5D0*B(I)
  104 CONTINUE
      R(1) = AZERO
      IF (MOD(N,2) .EQ. 0) R(N) = A(NS2+1)
      CALL DFFTB (N,R,WSAVE(N+1))
      RETURN
      END
