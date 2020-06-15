!/ ====================================================================== BEGIN FILE =====
!/ **                                  F T E S T _ P C A                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module ftest_PCA
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-JUN-01
  !! license: GPL
  !!
  !! ## Test GNUPlot file generation
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use statistics_mod
  use pca_mod
  use dice_mod

  integer, parameter :: NUM_SAMP = 60
  integer, parameter :: NUM_DIM  = 2

  character(1), parameter :: LB(26) = [ 'A', 'B', 'C', 'D', 'E', 'F', &
       &                                'G', 'H', 'I', 'J', 'K', 'L',  &
       &                                'M', 'N', 'O', 'P', 'Q', 'R',  &
       &                                'S', 'T', 'U', 'V', 'W', 'X',  &
       &                                'Y', 'Z' ]


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  !/ =====================================================================================
  subroutine LABELS( buffer, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(:), allocatable, intent(inout) :: buffer
    integer,                   intent(in)    :: n
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    buffer = '"' // LB(1) // '"'
    do i=2,n
       buffer = buffer // ', "' // LB(i) // '"'
    end do

  end subroutine LABELS

  !/ =====================================================================================
  function BOUNDS( X, Y, Z ) result( mx )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: X(:,:)
    real(dp), intent(in) :: Y(:,:)
    real(dp), intent(in) :: Z(:,:)
    real(dp)             :: mx
    !/ -----------------------------------------------------------------------------------
    integer  :: i,j,m,n
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------

    mx = D_ZERO
    
    !/ -----------------------------------------------------------------------------------
    m = size(X,DIM=1)
    n = size(X,DIM=2)
    do i=1,m
       do j=1,n
          t = abs(X(i,j))
          if ( t.gt.mx) mx=t
       end do
    end do

    !/ -----------------------------------------------------------------------------------
    m = size(Y,DIM=1)
    n = size(Y,DIM=2)
    do i=1,m
       do j=1,n
          t = abs(Y(i,j))
          if ( t.gt.mx) mx=t
       end do
    end do

    !/ -----------------------------------------------------------------------------------
    m = size(Z,DIM=1)
    n = size(Z,DIM=2)
    do i=1,m
       do j=1,n
          t = abs(Z(i,j))
          if ( t.gt.mx) mx=t
       end do
    end do

  end function BOUNDS
  
    

  !/ =====================================================================================
  subroutine TEST01
    !/ -----------------------------------------------------------------------------------
    use file_tools, only : WriteUnit
    implicit none

    real(dp), allocatable :: W(:,:)
    real(dp), allocatable :: mu(:)
    real(dp), allocatable :: X(:,:)
    real(dp), allocatable :: Y(:,:)
    real(dp), allocatable :: Z(:,:)

    integer  :: i, j, k, fh, imx
    real(dp) :: sm

    type(Dice) :: dd

    type(PCA) :: PC

    character(:), allocatable :: line

    !/ -----------------------------------------------------------------------------------

    call dd%seed_set()

    allocate( mu(NUM_DIM) )
    allocate( W(NUM_DIM,NUM_DIM) )
    allocate( X(NUM_SAMP,NUM_DIM) )
    
    do i=1,NUM_DIM
       mu(i) = dd%normal()
       do j=1,NUM_DIM
          W(i,j) = D_TWO*dd%normal()
       end do
    end do

    do i=1,NUM_SAMP
       do j=1,NUM_DIM
          sm = mu(j)
          do k=1,NUM_DIM
             sm = sm + ( dd%normal() * W(k,j) )
          end do
          X(i,j) = sm
       end do
    end do
    
    fh = WriteUnit( FILE='original.csv' )

    !call LABELS( line, NUM_DIM )
    !write(fh,'(A)') line

    do i=1,NUM_SAMP
       write(fh,'(A)')  toString( X(i,:), del=',', fmt='F11.6' ) 
    end do

    close(fh)

    !/ -----------------------------------------------------------------------------------

    call PC%compile( X, MEAN_CENTERED=.false. )

    print *, 'mu   = ', mu
    print *, 'mean = ', PC%mu
    print *, 'sing = ', PC%S

    allocate( Y(NUM_SAMP,NUM_DIM) )
    
    call zero(Y)
    
    call PC%transform( Y, X, NUM_DIM )

    fh = WriteUnit( FILE='transform.csv' )

    !call LABELS( line, NUM_DIM )
    !write(fh,'(A)') line
    
    do i=1,NUM_SAMP
       write(fh,'(A)')  toString( Y(i,:), del=',', fmt='F11.6' ) 
    end do

    close(fh)

     !/ -----------------------------------------------------------------------------------

    allocate( Z(NUM_SAMP,NUM_DIM) )
    
    call zero(Z)
    
    call PC%recover( Z, Y, NUM_DIM )

    fh = WriteUnit( FILE='recover.csv' )

    !call LABELS( line, NUM_DIM )
    !write(fh,'(A)') line
    
    do i=1,NUM_SAMP
       write(fh,'(A)')  toString( Z(i,:), del=',', fmt='F11.6' ) 
    end do

    close(fh)

   !/ -----------------------------------------------------------------------------------

    imx = int(floor(BOUNDS( X, Y, Z )+0.5d0))
    
    fh = WriteUnit( FILE='plot.cfg' )

    write(fh,100) 'x', imx, imx
    write(fh,100) 'y', imx, imx
    write(fh,'(A)') 'plot "original.csv"'
    write(fh,'(A)') 'pause -1 "Press Enter..."'
    write(fh,'(A)') 'plot "transform.csv"'
    write(fh,'(A)') 'pause -1 "Press Enter..."'
    write(fh,'(A)') 'plot "recover.csv"'
    write(fh,'(A)') 'pause -1 "Press Enter..."'
    write(fh,'(A)') 'plot "recover.csv", "original.csv"'
    write(fh,'(A)') 'pause -1 "Press Enter..."'

100 format( 'set ',A,'range[-',I0,':',I0,']')
    
    close(fh)

    
   !/ -----------------------------------------------------------------------------------

    deallocate( Z )
    deallocate( Y )
    deallocate( X )
    deallocate( W )
    deallocate( mu )


end subroutine TEST01
  
end module ftest_PCA




!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
use ftest_PCA

call TEST01

end program main


!/ =======================================================================================
!/ **                             F T E S T _ G N U P L O T                             **
!/ =========================================================================== END FILE ==
