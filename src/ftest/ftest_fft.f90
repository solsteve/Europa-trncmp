!/ ====================================================================== BEGIN FILE =====
!/ **                                 F T E S T _ F F T                                 **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module ftest_fft_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-17
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use fft_mod
  use file_tools




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  


  !/ =====================================================================================
  subroutine test03
    !/ -----------------------------------------------------------------------------------
    use stopwatch_class
    implicit none
    integer                  :: i, N, inf, outf
    real(dp),    allocatable :: T(:)
    real(dp),    allocatable :: X(:)
    real(dp),    allocatable :: R(:)
    type(stopwatch) :: SW
    real(dp) :: et
    !/ -----------------------------------------------------------------------------------
    character(*), parameter :: FSPC = '/data/datasets/temp/big.dat'
    character(*), parameter :: FOUT = 'last.dat'
    !/ -----------------------------------------------------------------------------------

    N  = LineCount( FSPC )

    write(*,1000) N

    allocate( T(N) )
    allocate( X(N) )
    allocate( R(N) )

    inf = ReadUnit( FSPC )
    do i=1,N
       read(inf,*) T(i), X(i)
    end do
    close( inf )

    !/ -----------------------------------------------------------------------------------

    call SW%reset()
    call auto_correlate( R, X )
    et = SW%check()

    write(*,1100) N, et
    
    outf = WriteUnit( FOUT )
    
    do i=1,N
       write(outf,*) T(i),' ',R(i)
    end do

    close(outf)
    
1000 format( 'Read and formated ',I0,' records' )
1100 format( 'Auto correlate ',I0,' records in ',F10.6,' seconds' )
  
  end subroutine test03


  !/ =====================================================================================
  subroutine test02
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                  :: i, N, P, inf, outf
    real(dp)                 :: fn, mean
    real(dp),    allocatable :: T(:)
    real(dp),    allocatable :: X(:)
    real(dp),    allocatable :: R(:)
    complex(dp), allocatable :: S(:)
    !/ -----------------------------------------------------------------------------------
    character(*), parameter :: FSPC = '/data/datasets/temp/big.dat'
    character(*), parameter :: FOUT = 'test.dat'
    !/ -----------------------------------------------------------------------------------

    N  = LineCount( FSPC )
    P  = find_p2( N )
    fn = real( N, dp )

    write(*,*) 'Records=', N

    allocate( T(N) )
    allocate( X(N) )
    allocate( R(N) )
    allocate( S(P) )

    inf = ReadUnit( FSPC )
    do i=1,N
       read(inf,*) T(i), X(i)
    end do
    close( inf )

    !/ -----------------------------------------------------------------------------------
    
    mean = 0.0d0
    do concurrent(i=1:N)
       mean = mean + X(I)
    end do
    mean = mean / fn

    do concurrent(i=1:N)
       S(i) = cmplx( X(i) - mean, 0.0d0, dp )
    end do
    do concurrent(i=N+1:P)
       S(i) = cmplx( 0.0d0, 0.0d0, dp )
    end do

    !/ -----------------------------------------------------------------------------------

    call fft( S )

    do concurrent(i=1:N)
       S(i) = S(i) * conjg( S(i) )
    end do
    
    call ifft( S )

    do concurrent(i=1:N)
       R(i) = real( S(i), dp )
    end do

    outf = WriteUnit( FOUT )
    
    do i=1,N
       write(outf,*) T(i),' ',R(i)
    end do

    close(outf)
    
  
  end subroutine test02

  
  !/ =====================================================================================
  subroutine test01
    !/ -----------------------------------------------------------------------------------
    implicit none
    complex(kind=dp), dimension(8) :: Fx = (/1.0, 0.0, 1.0, 1.0, 0.0, 0.0, 0.0, 0.0/)
    complex(kind=dp), dimension(8) :: Ft
    complex(kind=dp), dimension(8) :: Fr
    integer :: i
    real(dp), parameter :: ZERO = 0.0d0

    Ft = Fx
    call fft(Ft)
    Fr = Ft
    call ifft(Fr)

    do i=1,8
       if (aimag(Ft(i)).lt.ZERO) then
          write(*,100) Fx(i), Ft(i), Fr(i)
       else
          write(*,110) Fx(i), Ft(i), Fr(i)
       end if
    end do

100 format( '(',F6.3,F0.3,'i)  (',F6.3,F0.3,'i)  (',F6.3,F0.3,'i)' )
110 format( '(',F6.3,'+',F0.3,'i)  (',F6.3,'+',F0.3,'i)  (',F6.3,'+',F6.3,'i)' )

    
  end subroutine test01
  
  !/ =====================================================================================
  subroutine test04
    !/ -----------------------------------------------------------------------------------
    use dfftpack
    implicit none
    !/ -----------------------------------------------------------------------------------
    real(dp) :: sqrt2, fn, tfn, arg1, arg2, cf, dcfb, dcfftb, dcfftf, dt
    !real(dp), dimension(100)  ::
    real(dp),    dimension(200)  :: X, Y, XH
    real(dp),    dimension(2000) :: W
    complex(dp), dimension(200)  :: cx, cy
    integer  :: nns, nz, i, j, k, n, modn, np1, nm1
    !/ -----------------------------------------------------------------------------------
     integer, parameter :: ND(7) = (/ 120, 54, 49, 32, 4, 3, 2 /)
   real(dp), parameter :: pi = 3.14159265358979d0
     !/ -----------------------------------------------------------------------------------
      SQRT2 = SQRT(2.0d0)
      NNS = 7
      L157: DO NZ=1,NNS
         N    = ND(NZ)
         MODN = MOD(N,2)
         FN   = REAL(N,dp)
         TFN  = FN + FN
         NP1  = N + 1
         NM1  = N - 1
         L101: DO J=1,NP1
            X(J)  = SIN(REAL(J,dp)*SQRT2)
            Y(J)  = X(J)
            XH(J) = X(J)
         END DO L101
         
    !/ -----------------------------------------------------------------------------------
         L149: DO I=1,N
            CX(I) = CMPLX(COS(SQRT2*REAL(I,dp)),SIN(SQRT2*REAL(I*I,dp)), dp)
         END DO L149
         
         DT = (PI+PI)/FN
         L151: DO I=1,N
            ARG1 = -REAL(I-1,dp)*DT
            CY(I) = (0.,0.)
            L150: DO K=1,N
               ARG2 = REAL(K-1,dp)*ARG1
               CY(I) = CY(I)+CMPLX(COS(ARG2),SIN(ARG2),dp)*CX(K)
            END DO L150
         END DO L151
         
         CALL TC_FFTI (N,W)
         CALL TC_FFTF (N,CX,W)
         
         DCFFTF = 0.
         L152: DO I=1,N
            DCFFTF = MAX(DCFFTF,ABS(CX(I)-CY(I)))
            CX(I) = CX(I)/FN
         END DO L152
            
         DCFFTF = DCFFTF/FN
         L154: DO I=1,N
            ARG1 = REAL(I-1,dp)*DT
            CY(I) = (0.,0.)
            L153: DO K=1,N
               ARG2 = REAL(K-1,dp)*ARG1
               CY(I) = CY(I)+CMPLX(COS(ARG2),SIN(ARG2),dp)*CX(K)
            END DO L153
         END DO L154
               
         CALL TC_FFTB (N,CX,W)
         
         DCFFTB = 0.
         L155: DO I=1,N
            DCFFTB = MAX(DCFFTB,ABS(CX(I)-CY(I)))
            CX(I) = CY(I)
         END DO L155
            
         CF = 1./FN
         CALL TC_FFTF (N,CX,W)
         CALL TC_FFTB (N,CX,W)
         
         DCFB = 0.
         L156: DO I=1,N
            DCFB = MAX(DCFB,ABS(CF*CX(I)-CY(I)))
         END DO L156

    !/ -----------------------------------------------------------------------------------
            WRITE(*,1001) DCFFTF,DCFFTB,DCFB
1001        FORMAT( 'CFFTF: ', ES10.3, ' CFFTB: ', ES10.3, ' CFFTFB: ', ES10.3 )
      END DO L157

  end subroutine test04

end module ftest_fft_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_fft_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test04



end program main

!/ =======================================================================================
!/ **                                 F T E S T _ F F T                                 **
!/ =========================================================================== END FILE ==
