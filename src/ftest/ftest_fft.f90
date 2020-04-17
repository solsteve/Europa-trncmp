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
  use dfft_class
  use file_tools
  use omp_lib
  use dice_mod


    real(dp), parameter :: test_X(2,32) = reshape( [ & 
         &   9.7424584586d-02, -1.0878115695d-01, -1.5780082582d+00, -4.7104877883d-01,  &
         &   8.2711739634d-01,  8.0804592499d-01,  6.7044702029d-01, -1.7925486667d-01,  &
         &  -1.6394881611d-01,  1.1523961568d+00,  8.3964611934d-01, -1.5500583170d-02,  &
         &  -8.3852149944d-01, -1.3703995060d+00, -6.3746041854d-02,  7.7472599259d-01,  &
         &   8.5288763512d-01,  1.0175125997d-01, -4.1263899584d-01,  5.8086222716d-01,  &
         &   2.8953251631d-01,  1.7565091214d-01,  1.5451273160d+00,  4.6901904829d-01,  &
         &  -5.5571596918d-01, -7.4609273801d-01, -1.1630708159d+00,  3.3798290398d-01,  &
         &  -5.8780687971d-01, -1.5613608885d+00,  2.0923094130d-02,  9.5449480127d-01,  &
         &   2.0856088585d+00, -3.7189512852d-01, -5.0068206371d-01,  2.8238405205d-01,  &
         &   4.3412128508d-01, -6.1676058433d-01, -2.0784912984d+00,  1.8492337760d-01,  &
         &   3.5496942503d-01,  4.5018250462d-01,  1.1854942347d+00,  8.8766369217d-01,  &
         &  -3.3425565664d-01,  1.6102272901d-01,  1.0087338870d+00, -2.2948589150d+00,  &
         &  -5.2466259167d-01,  6.0392527755d-01,  8.0826283294d-01,  7.0354926197d-01,  &
         &   7.0885693658d-01, -3.0597864698d-01, -6.8288316284d-01,  1.7410978263d+00,  &
         &  -2.9957873518d-01,  5.5393242880d-01,  1.1976588489d+00, -8.8179611986d-01,  &
         &  -7.3583513751d-03, -6.7381763806d-01, -1.5253290592d+00, -1.1823455107d+00   &
         &  ],[2,32])

    real(dp), parameter :: test_Y(2,32) = reshape( [ & 
         &   1.6101137956d+00,  1.4371931571d-01, -8.9510503194d-01,  1.9358325122d-01,  &
         &  -2.7695669034d+00, -3.7925632397d+00, -2.3770238723d+00, -3.8074856900d+00,  &
         &   7.3702999616d+00, -4.8362387849d+00, -3.1946794311d+00, -6.7473506196d-01,  &
         &   2.1769903818d-01,  3.7720923386d+00, -9.6544893330d+00, -2.0708149984d+00,  &
         &   2.3115935458d+00,  3.5371361550d+00,  1.2968287155d+00, -5.6374975050d-01,  &
         &  -2.2029195338d+00, -5.3169787605d+00,  8.8636174311d+00, -1.8110641896d+00,  &
         &   5.6716441650d+00,  7.7392551003d+00, -2.8594911208d+00,  1.1608718466d+00,  &
         &   4.0943551969d+00,  9.5994256310d-01,  1.9345104142d+00,  6.6505684166d+00,  &
         &   3.0672264809d+00, -3.6400775025d+00, -5.0975537974d+00,  6.0872157353d+00,  &
         &   1.3162785526d+01, -1.2235045319d+00,  1.6294018134d+00,  1.0482226864d+01,  &
         &   5.9917910210d+00, -5.5897384583d+00,  3.2524049466d-01, -1.1331773774d+01,  &
         &   6.1938057760d-01, -2.6111525105d+00,  3.8940332636d+00,  6.2057427833d+00,  &
         &   3.9900374217d-01,  6.5008964489d+00, -7.6997262841d+00, -2.3194197420d+00,  &
         &   6.4078903927d+00,  1.4023739870d+00, -1.5007645772d+01, -1.7720188478d+00,  &
         &  -6.3316048197d+00, -2.0549502577d+00, -1.7983798768d+00, -1.4674825468d+00,  &
         &  -4.6911570967d+00, -2.6810324301d+00, -1.1704859953d+00, -7.5184075133d-01   &
         &  ],[2,32])

    real(dp), parameter :: test_Z(2,32) = reshape( [ & 
         &   9.7424584586d-02, -1.0878115695d-01, -1.5780082582d+00, -4.7104877883d-01,  &
         &   8.2711739634d-01,  8.0804592499d-01,  6.7044702029d-01, -1.7925486667d-01,  &
         &  -1.6394881611d-01,  1.1523961568d+00,  8.3964611934d-01, -1.5500583170d-02,  &
         &  -8.3852149944d-01, -1.3703995060d+00, -6.3746041854d-02,  7.7472599259d-01,  &
         &   8.5288763512d-01,  1.0175125997d-01, -4.1263899584d-01,  5.8086222716d-01,  &
         &   2.8953251631d-01,  1.7565091214d-01,  1.5451273160d+00,  4.6901904829d-01,  &
         &  -5.5571596918d-01, -7.4609273801d-01, -1.1630708159d+00,  3.3798290398d-01,  &
         &  -5.8780687971d-01, -1.5613608885d+00,  2.0923094130d-02,  9.5449480127d-01,  &
         &   2.0856088585d+00, -3.7189512852d-01, -5.0068206371d-01,  2.8238405205d-01,  &
         &   4.3412128508d-01, -6.1676058433d-01, -2.0784912984d+00,  1.8492337760d-01,  &
         &   3.5496942503d-01,  4.5018250462d-01,  1.1854942347d+00,  8.8766369217d-01,  &
         &  -3.3425565664d-01,  1.6102272901d-01,  1.0087338870d+00, -2.2948589150d+00,  &
         &  -5.2466259167d-01,  6.0392527755d-01,  8.0826283294d-01,  7.0354926197d-01,  &
         &   7.0885693658d-01, -3.0597864698d-01, -6.8288316284d-01,  1.7410978263d+00,  &
         &  -2.9957873518d-01,  5.5393242880d-01,  1.1976588489d+00, -8.8179611986d-01,  &
         &  -7.3583513751d-03, -6.7381763806d-01, -1.5253290592d+00, -1.1823455107d+00   &
         &  ],[2,32])




    !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
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
    complex(kind=dp), dimension(8) :: Fx = [ 1.0d0, 0.0d0, 1.0d0, 1.0d0, 0.0d0, 0.0d0, 0.0d0, 0.0d0 ]
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
1001   FORMAT( 'CFFTF: ', ES10.3, ' CFFTB: ', ES10.3, ' CFFTFB: ', ES10.3 )
    END DO L157

  end subroutine test04


  !/ =====================================================================================
  subroutine test05
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    integer :: proc_num, thread_num, n, ln2, i, j, nits
    type(Dice) :: dd
    complex(dp), allocatable :: x(:), y(:)
    real(dp) :: mse, d0, d1, flops, z0, z1, t1, t2
    !/ -----------------------------------------------------------------------------------

    call dd%seed_set

    proc_num   = omp_get_num_procs ( )
    thread_num = omp_get_max_threads ( )

    write ( *, '(A,1X,I0)' ) '\nThe number of processors available =', proc_num
    write ( *, '(A,1X,I0)' )   'The number of threads available    =', thread_num

    write ( *, '(a)' ) '\n             N      NITS    Error         Time' // &
         &             '          Time/Call     MFLOPS\n'
    nits = 10000

    n = 1
    do ln2=1,24
       n = n * 2

       allocate( x(n) )
       allocate( y(n) )

       do i=1,n
          z0 = dd%normal()
          z1 = dd%normal()

          x(i) = cmplx(z0,z1,dp)
          y(i) = cmplx(z0,z1,dp)
       end do

       t1 = omp_get_wtime ( )

       do j = 1, nits
          call fft( x )
          call ifft( x )
       end do

       t2 = omp_get_wtime ( ) - t1

       mse = D_ZERO
       do j=1,n
          d0 = realpart(x(j)) - realpart(y(j))
          d1 = imagpart(x(j)) - imagpart(y(j))
          mse = mse + (d0*d0) + (d1*d1)
       end do

       deallocate( x )
       deallocate( y )

       if ( 0.eq.mod(ln2,4) ) then
          nits = nits / 10
          if ( nits.le.1 ) then
             nits = 1
          end if
       end if


       flops = D_TWO * real(nits,dp) * (D_FIVE * real(n,dp) * real (ln2,dp))


       write(*,100) n, nits, mse, t2, t2 / real(2*nits,dp), flops/1.0D6/t2


    end do

100 format( i12,2x,i8,2x,g14.6,2x,g12.4,2x,g12.4,2x,g12.4 )

  end subroutine test05


  !/ =====================================================================================
  subroutine test06
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    integer :: proc_num, thread_num, n, ln2, i, j, nits
    type(Dice) :: dd
    real(dp), allocatable :: x(:), y(:), z(:)
    real(dp) :: mse, d0, d1, flops, z0, z1, t1, t2
    type(DFFT) :: FT
    !/ -----------------------------------------------------------------------------------

    call dd%seed_set

    proc_num   = omp_get_num_procs ( )
    thread_num = omp_get_max_threads ( )

    write ( *, '(A,1X,I0)' ) '\nThe number of processors available =', proc_num
    write ( *, '(A,1X,I0)' )   'The number of threads available    =', thread_num

    write ( *, '(a)' ) '\n             N      NITS    Error         Setup        Time' // &
         &             '          Time/Call     MFLOPS\n'
    nits = 10000

    n = 1
    do ln2=1,14
       n = n * 2

       allocate( x(2*n) )
       allocate( y(2*n) )
       allocate( z(2*n) )

       t1 = omp_get_wtime ( )
       call FT%build( n )
       t1 = omp_get_wtime ( ) - t1

       do i=1,2*n
          x(i) = dd%normal()
       end do

       !/ -------------------------------------
       t2 = omp_get_wtime ( )

       do j = 1, nits
          call FT%forward( y, x )
          call FT%reverse( z, y )
       end do

       t2 = omp_get_wtime ( ) - t2
       !/ -------------------------------------

       mse = D_ZERO
       do j=1,2*n
          d0 = x(j) - z(j)
          mse = mse + (d0*d0)
       end do

       deallocate( x )
       deallocate( y )
       deallocate( z )

       if ( 0.eq.mod(ln2,3) ) then
          nits = nits / 10
          if ( nits.le.1 ) then
             nits = 1
          end if
       end if


       flops = D_TWO * real(nits,dp) * (D_FIVE * real(n,dp) * real (ln2,dp))


       write(*,100) n, nits, mse, t1, t2, t2 / real(2*nits,dp), flops/1.0D6/t2


    end do

100 format( i12,2x,i8,2x,g14.6,2x,g14.6,2x,g12.4,2x,g12.4,2x,g12.4 )

  end subroutine test06

  !/ =====================================================================================
  subroutine test08
    !/ -----------------------------------------------------------------------------------
    implicit none

    integer :: np, i
    real(dp) :: mse
    complex(dp), allocatable :: Cx(:), Cy(:), Cz(:)
    
    np = size( test_x, DIM=2 )

    allocate( Cx(np) )
    allocate( Cy(np) )
    allocate( Cz(np) )

    Cx = transfer( test_x, Cx )
    Cy = transfer( test_y, Cy )
    Cz = transfer( test_z, Cz )

    call fft( Cx )

    mse = D_ZERO
    do i=1,np
       mse = mse + ((Cx(i) - Cy(i))*conjg(Cx(i) - Cy(i)))
    end do

    print *, 'MSE=', mse

    call ifft( Cx )

     mse = D_ZERO
    do i=1,np
       mse = mse + ((Cx(i) - Cz(i))*conjg(Cx(i) - Cz(i)))
    end do

    print *, 'MSE=', mse

   

    deallocate( Cx )
    deallocate( Cy )
    deallocate( Cz )
    
  end subroutine test08




  
end module ftest_fft_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_fft_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test06



end program main

!/ =======================================================================================
!/ **                                 F T E S T _ F F T                                 **
!/ =========================================================================== END FILE ==
