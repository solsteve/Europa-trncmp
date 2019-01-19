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
    implicit none
    integer                  :: i, N, inf, outf
    real(dp),    allocatable :: T(:)
    real(dp),    allocatable :: X(:)
    real(dp),    allocatable :: R(:)
    !/ -----------------------------------------------------------------------------------
    character(*), parameter :: FSPC = '/data/datasets/temp/big.dat'
    character(*), parameter :: FOUT = 'last.dat'
    !/ -----------------------------------------------------------------------------------

    N  = LineCount( FSPC )

    write(*,*) 'Records=', N

    allocate( T(N) )
    allocate( X(N) )
    allocate( R(N) )

    inf = ReadUnit( FSPC )
    do i=1,N
       read(inf,*) T(i), X(i)
    end do
    close( inf )

    !/ -----------------------------------------------------------------------------------
    
    call auto_correlate( R, X )

    outf = WriteUnit( FOUT )
    
    do i=1,N
       write(outf,*) T(i),' ',R(i)
    end do

    close(outf)
    
  
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


end module ftest_fft_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_fft_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test03



end program main

!/ =======================================================================================
!/ **                                 F T E S T _ F F T                                 **
!/ =========================================================================== END FILE ==
