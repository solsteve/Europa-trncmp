!/ ====================================================================== BEGIN FILE =====
!/ **                              F T E S T _ S I M P L E                              **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
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
module ftest_simple_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-12-02
  !! license: GPL
  !!
  !!##Test of simple BPNN
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use omp_lib
  use openacc
  implicit none

  character(*), parameter :: TRAINFILE = '/data/datasets/Iris/iris.for'
  character(*), parameter :: FMT_PART  = 'F4.2'
  integer,      parameter :: NIN       = 4
  integer,      parameter :: NOUT      = 3
  integer,      parameter :: NSAMP     = 150
  integer,      parameter :: NCOL      = NIN + NOUT

  integer,      parameter :: MAXGEN    = 100000

  integer,      parameter :: NH1       = 7
  integer,      parameter :: NH2       = 5

  real(dp), allocatable :: X_DATA(:,:)
  real(dp), allocatable :: Y_DATA(:,:)


  interface displayMat
     module procedure :: displayMat_mat
     module procedure :: displayMat_vec
  end interface displayMat


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine read_data
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    real(dp) :: row(NCOL)
    character(:), allocatable :: fmt
    integer :: i, j, fp

    fmt = '(' // FMT_PART
    do i=2,NCOL
       fmt = fmt // ',1X,'
       fmt = fmt // FMT_PART
    end do
    fmt = fmt // ')'

    open( NEWUNIT=fp, FILE=TRAINFILE, ACTION='READ', STATUS='OLD' )

    allocate( X_DATA(NIN,  NSAMP) )
    allocate( Y_DATA(NOUT, NSAMP) )

    do j=1,NSAMP
       read(fp,fmt) (row(i),i=1,NCOL)
       do i=1,NIN
          X_DATA(i,j) = row(i)
       end do
       do i=1,NOUT
          Y_DATA(i,j) = row(NIN+i)
       end do
    end do

    close(fp)

  end subroutine read_data


  !/ =====================================================================================
  subroutine initialize( W, B )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: W(:,:)
    real(dp), intent(inout) :: B(:)
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c, nr, nc
    real(dp) :: x

    nr = size(W,DIM=1)
    nc = size(W,DIM=2)

    do c=1,nc
       do r=1,nr
          call random_number(x)
          W(r,c) = 2.0d-2 * x - 1.0d-2
       end do
    end do

    nr = size(B)
    do r=1,nr
       call random_number(x)
       B(r) = 2.0d-2 * x - 1.0d-2
    end do

  end subroutine initialize

  !/ =====================================================================================
  subroutine display_error( g, E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: g
    real(dp), intent(in) :: E(:,:)
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c, nr, nc
    real(dp) :: sum

    sum = 0.0d0

    nr = size( E, DIM=1 )
    nc = size( E, DIM=2 )

    do c=1,nc
       do r=1,nr
          sum = sum + ( E(r,c) * E(r,c) )
       end do
    end do

    write(*,100) g, sum / real(nr*nc, dp)

100 format( I10,1X,ES13.6 )
    
  end subroutine display_error


  !/ =====================================================================================
  subroutine WriteMat( u, M, NR, NC )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: u
    real(dp), intent(in) :: M(:,:)
    integer, optional, intent(in) :: NR
    integer, optional, intent(in) :: NC
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, nru, ncu
    !/ -----------------------------------------------------------------------------------

    if ( present( NR ) ) then
       nru = NR
    else
       nru = size( M, DIM=1 )
    end if
    
    if ( present( NC ) ) then
       ncu = NC
        else
       ncu = size( M, DIM=2 )
     end if
   
    do r=1,nru
       do c=1,ncu
          write(u,100) M(r,c)
       end do
    end do

100 format( F12.8 )

  end subroutine WriteMat


  !/ =====================================================================================
  subroutine WriteVec( u, V, N )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,  intent(in) :: u
    real(dp), intent(in) :: V(:)
    integer, optional, intent(in) :: N
    !/ -----------------------------------------------------------------------------------
    integer :: i, nu
    !/ -----------------------------------------------------------------------------------

    if ( present(N) ) then
       nu = N
    else
       nu = size( V )
    end if
    
    do i=1,nu
       write(u,100) V(i)
    end do

100 format( F12.8 )

  end subroutine WriteVec


  !/ =====================================================================================
  subroutine ReadMat( u, M, nr, nc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,               intent(in)  :: u
    real(dp), allocatable, intent(out) :: M(:,:)
    integer,               intent(in)  :: nr
    integer,               intent(in)  :: nc
    !/ -----------------------------------------------------------------------------------
    integer :: r, c
    !/ -----------------------------------------------------------------------------------

    allocate( M(nr,nc) )

    do r=1,nr
       do c=1,nc
          read(u,*) M(r,c)
       end do
    end do

  end subroutine ReadMat


  !/ =====================================================================================
  subroutine ReadVec( u, V, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,               intent(in)  :: u
    real(dp), allocatable, intent(out) :: V(:)
    integer,               intent(in)  :: n
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    allocate( V(n) )

    do i=1,n
       read(u,*) V(i)
    end do

  end subroutine ReadVec


  !/ =====================================================================================
  subroutine WriteNet( fspc, W1, W2, W3, b1, b2, b3 )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: fspc
    real(dp),     intent(in) :: W1(:,:)
    real(dp),     intent(in) :: W2(:,:)
    real(dp),     intent(in) :: W3(:,:)
    real(dp),     intent(in) :: b1(:)
    real(dp),     intent(in) :: b2(:)
    real(dp),     intent(in) :: b3(:)
    !/ -----------------------------------------------------------------------------------
    integer :: fp
    !/ -----------------------------------------------------------------------------------
    open( NEWUNIT=fp, FILE=fspc, ACTION='WRITE', STATUS='REPLACE' )
    call WriteMat( fp, W1 )
    call WriteMat( fp, W2 )
    call WriteMat( fp, W3 )
    call WriteVec( fp, b1 )
    call WriteVec( fp, b2 )
    call WriteVec( fp, b3 )
    close(fp)
  end subroutine WriteNet
  
    
  !/ =====================================================================================
  subroutine ReadNet( fspc, W1, W2, W3, b1, b2, b3, n0, n1, n2, n3 )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),          intent(in)  :: fspc
    real(dp), allocatable, intent(out) :: W1(:,:)
    real(dp), allocatable, intent(out) :: W2(:,:)
    real(dp), allocatable, intent(out) :: W3(:,:)
    real(dp), allocatable, intent(out) :: b1(:)
    real(dp), allocatable, intent(out) :: b2(:)
    real(dp), allocatable, intent(out) :: b3(:)
    integer,               intent(in)  :: n0
    integer,               intent(in)  :: n1
    integer,               intent(in)  :: n2
    integer,               intent(in)  :: n3
    !/ -----------------------------------------------------------------------------------
    integer :: fp
    !/ -----------------------------------------------------------------------------------
    open( NEWUNIT=fp, FILE=fspc, ACTION='READ', STATUS='OLD' )
    call ReadMat( fp, W1, n0, n1 )
    call ReadMat( fp, W2, n1, n2 )
    call ReadMat( fp, W3, n2, n3 )
    call ReadVec( fp, b1, n1 )
    call ReadVec( fp, b2, n2 )
    call ReadVec( fp, b3, n3 )
    close(fp)
  end subroutine ReadNet


  !/ =====================================================================================
  subroutine displayMat_mat( M )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: M(:,:)
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: line
    character(len=32) :: buffer
    integer :: r, c, nr, nc
    character(*), parameter :: FMT='(ES15.8)'
    !/ -----------------------------------------------------------------------------------

    nr = size( M, DIM=1 )
    nc = size( M, DIM=2 )

    do r=1,nr
       write(buffer,FMT) M(r,1)
       line = '[' // trim(buffer)
       do c=2,nc
          write(buffer,FMT) M(r,c)
          line = line // ' ' // trim(buffer)
       end do
       line = line // ']'
       if ( 1.eq.r ) then
          write(*,100) line
       else
          if ( nr.eq.r ) then
             write(*,120) line
          else
             write(*,110) line
          end if
       end if
    end do

100 format('[',A)
110 format(1X,A)
120 format(1X,A,']')

  end subroutine displayMat_mat
  
    
  !/ =====================================================================================
  subroutine displayMat_vec( M )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: M(:)
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: line
    character(len=32) :: buffer
    integer :: c, nc
    character(*), parameter :: FMT='(ES15.8)'
    !/ -----------------------------------------------------------------------------------

    nc = size( M )

       write(buffer,FMT) M(1)
       line = '[' // trim(buffer)
       do c=2,nc
          write(buffer,FMT) M(c)
          line = line // ' ' // trim(buffer)
       end do
       line = line // ']'
          write(*,100) line
100 format('[',A)

  end subroutine displayMat_vec
  







  !/ =====================================================================================
  subroutine transfer( A, Z, nr, nc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: A(:,:)  !! acitvated non-linear values
    real(dp), intent(in)    :: Z(:,:)  !! input weighted sums
    integer,  intent(in)    :: nr      !! number of nodes
    integer,  intent(in)    :: nc      !! number of samples
    !/ -----------------------------------------------------------------------------------
    integer :: r, c

    do concurrent( r=1:nr, c=1:nc )
       A(r,c) = 1.0d0 / (1.0d0 + exp( -Z(r,c) ) )
    end do

  end subroutine transfer


  !/ =====================================================================================
  subroutine delta( D, E, A, nr, nc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: D(:,:)  !! error matirx time the gradient
    real(dp), intent(in)    :: E(:,:)  !! the gradient
    real(dp), intent(in)    :: A(:,:)  !! the forward activation values
    integer,  intent(in)    :: nr      !! number of nodes
    integer,  intent(in)    :: nc      !! number of samples
    !/ -----------------------------------------------------------------------------------
    integer :: r, c

    do concurrent( r=1:nr, c=1:nc )
       D(r,c) = E(r,c)*A(r,c)*(1.0d0 - A(r,c))
    end do

  end subroutine delta


  !/ =====================================================================================
  subroutine mul_weight( Z, W, B, X, nx, nz, ns )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: Z(:,:) !! weighted sum
    real(dp), intent(in)    :: W(:,:) !! weigthts
    real(dp), intent(in)    :: B(:)   !! bias
    real(dp), intent(in)    :: X(:,:) !! output from previous layer
    integer,  intent(in)    :: nx     !! number of outputs previous layer
    integer,  intent(in)    :: nz     !! number of outputs this layer
    integer,  intent(in)    :: ns     !! number of samples
    !/ -----------------------------------------------------------------------------------
    integer  :: is
    integer  :: ix, iz
    real(dp) :: sum

    !/ this should be just faster than DGEMM(BLAS L3)
    !/ Form C := alpha*A**T*B + beta*C
    !/ the three nested loops are in the same order
    !/ Less the if-then-else block in the loop not to mention the 5 to 13
    !/ outside the loop if's to Get there

    do concurrent( is=1:ns, iz=1:nz )
          Z(iz,is) = B(iz)
          do ix=1,nx
             Z(iz,is) = Z(iz,is) + ( W(ix,iz) * X(ix,is) )
          end do
       end do

  end subroutine mul_weight


  !/ =====================================================================================
  subroutine subtract( S, A, B, nr, nc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: S(:,:)
    real(dp), intent(in)    :: A(:,:)
    real(dp), intent(in)    :: B(:,:)
    integer,  intent(in)    :: nr
    integer,  intent(in)    :: nc
    !/ -----------------------------------------------------------------------------------
    integer :: r, c

    do concurrent( r=1:nr, c=1:nc )
       S(r,c) = A(r,c) - B(r,c)
    end do

  end subroutine subtract


  !/ =====================================================================================
  subroutine error_matrix_mul( E, D, W, nr, nk, nc )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: E(:,:)
    real(dp), intent(in)    :: D(:,:)
    real(dp), intent(in)    :: W(:,:)
    integer,  intent(in)    :: nr
    integer,  intent(in)    :: nk
    integer,  intent(in)    :: nc
    !/ -----------------------------------------------------------------------------------
    integer  :: c, r, k

    do concurrent (c=1:nc, r=1:nr)
       E(r,c) = 0.0d0
       do k=1,nk
          E(r,c) = E(r,c) + ( W(r,k) * D(k,c) )
       end do
    end do

  end subroutine error_matrix_mul


  !/ =====================================================================================
  subroutine update( W, nr, nc, B, D, A, ns, alpha )
    !/ -----------------------------------------------------------------------------------
    !! Update a weight matrix and a bias vector
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: W(:,:)  !! weight matrix being updated
    integer,  intent(in)    :: nr      !! number of inputs to this layer
    integer,  intent(in)    :: nc      !! number of nodes in this layer
    real(dp), intent(inout) :: B(:)    !! bias vector being updated
    real(dp), intent(in)    :: D(:,:)  !! eror matrix
    real(dp), intent(in)    :: A(:,:)  !! inputs that feed the weights being updated
    integer,  intent(in)    :: ns      !! number of batch samples
    real(dp), intent(in)    :: alpha   !! learning rate
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c, s
    real(dp) :: sum

    do concurrent (c=1:nc, r=1:nr)
       sum = 0.0d0
       do s=1,ns
          sum = sum + A(r,s) * D(c,s)
       end do
       W(r,c) = W(r,c) +  alpha*sum
    end do

    do c=1,nc
       sum = 0.0d0
       do s=1,ns
          sum = sum + D(c,s)
       end do
       B(c) = B(c) + alpha*sum
    end do

  end subroutine update








  !/ =====================================================================================
  subroutine train_net
    !/ -----------------------------------------------------------------------------------
    use stopwatch_class
    use string_tools
    use omp_lib
    implicit none
    !/ -----------------------------------------------------------------------------------
    integer :: rep
    real(dp), allocatable :: W1(:,:), W2(:,:), W3(:,:)
    real(dp), allocatable :: Z1(:,:), Z2(:,:), Z3(:,:)
    real(dp), allocatable :: A1(:,:), A2(:,:), A3(:,:)
    real(dp), allocatable :: E1(:,:), E2(:,:), E3(:,:)
    real(dp), allocatable :: d1(:,:), d2(:,:), d3(:,:)
    real(dp), allocatable :: B1(:),   B2(:),   B3(:)
    type(stopwatch) :: SW
    real(dp) :: elapsed, alpha
    integer :: nt, count
    !/ -----------------------------------------------------------------------------------

    nt = 1
    alpha = 3.0d-1 / real( NSAMP, dp )

    !write(*,*) 'using alpha=', alpha

    call random_seed
    allocate( W1( NIN, NH1  ) )
    allocate( W2( NH1, NH2  ) )
    allocate( W3( NH2, NOUT ) )
    allocate( B1( NH1 ) )
    allocate( B2( NH2 ) )
    allocate( B3( NOUT ) )
    call initialize( W1, B1 )
    call initialize( W2, B2 )
    call initialize( W3, B3 )

    !call ReadNet( '/tmp/net-init.cfg', W1, W2, W3, b1, b2, b3, NIN, NH1, NH2, NOUT )
    call WriteNet( '/tmp/net-check-fortran.cfg', W1, W2, W3, b1, b2, b3 )
    
    allocate( Z1( NH1, NSAMP ) )
    allocate( A1( NH1, NSAMP ) )
    allocate( E1( NH1, NSAMP ) )
    allocate( d1( NH1, NSAMP ) )

    allocate( Z2( NH2, NSAMP ) )
    allocate( A2( NH2, NSAMP ) )
    allocate( E2( NH2, NSAMP ) )
    allocate( d2( NH2, NSAMP ) )

    allocate( Z3( NOUT, NSAMP ) )
    allocate( A3( NOUT, NSAMP ) )
    allocate( E3( NOUT, NSAMP ) )
    allocate( d3( NOUT, NSAMP ) )

    !/ -----------------------------------------------------------------------------------

    call SW%reset 

    count = 0
    do rep=1,MAXGEN

       !/ ----- forward pass ------------------------------------------

       call mul_weight( Z1, W1, B1, X_DATA, NIN, NH1, NSAMP )
       call transfer( A1, Z1, NH1, NSAMP )

       call mul_weight( Z2, W2, B2, A1, NH1, NH2, NSAMP )
       call transfer( A2, Z2, NH2, NSAMP )

       call mul_weight( Z3, W3, B3, A2, NH2, NOUT, NSAMP )
       call transfer( A3, Z3, NOUT, NSAMP )

       !/ ----- backpropagation ---------------------------------------

       call subtract( E3, Y_DATA, A3, NOUT, NSAMP )
       call delta( d3, E3, A3, NOUT, NSAMP )

       if ( 0.eq.modulo(rep-1,1000) ) then
          call display_error( rep, E3 )
       end if

       call error_matrix_mul( E2, D3, W3, NH2, NOUT, NSAMP )
       call delta( d2, E2, A2, NH2, NSAMP )

       call error_matrix_mul( E1, D2, W2, NH1, NH2, NSAMP )
       call delta( d1, E1, A1, NH1, NSAMP )

       call update( W3, NH2, NOUT,  B3, D3, A2,     NSAMP, alpha )
       call update( W2, NH1, NH2,   B2, D2, A1,     NSAMP, alpha )
       call update( W1, NIN, NH1,   B1, D1, X_DATA, NSAMP, alpha )

       count = count + 1
       
       !if ( 1.eq.rep ) then
       !   call WriteNet( '/tmp/net-once-fortran.cfg', W1, W2, W3, b1, b2, b3 )
       !end if
       
    end do

    !/ -----------------------------------------------------------------------------------

    elapsed = SW%check()

    call display_error( count, E3 )

    write(*,100) nt, elapsed / real( nt, dp )
100 format( I0,' threads, ',F10.5,' seconds' )

    call WriteNet( '/tmp/net-last-fortran.cfg', W1, W2, W3, b1, b2, b3 )
    
    !/ -----------------------------------------------------------------------------------

    deallocate( W1 )
    deallocate( W2 )
    deallocate( W3 )

    deallocate( B1 )
    deallocate( B2 )
    deallocate( B3 )

    deallocate( Z1 )
    deallocate( A1 )
    deallocate( E1 )
    deallocate( d1 )

    deallocate( Z2 )
    deallocate( A2 )
    deallocate( E2 )
    deallocate( d2 )

    deallocate( Z3 )
    deallocate( A3 )
    deallocate( E3 )
    deallocate( d3 )

  end subroutine train_net


end module ftest_simple_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_simple_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call read_data
  call train_net

end program main

!/ =======================================================================================
!/ **                                 F T E S T _ V L A                                 **
!/ =========================================================================== END FILE ==
