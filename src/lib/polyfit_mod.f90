!/ ====================================================================== BEGIN FILE =====
!/ **                               P O L Y F I T _ M O D                               **
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
module polyfit_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-06-02
  !! license: GPL
  !!
  !!## Polynomial Regression
  !!
  !! Collection of procedures for performing polynomial regresion fits.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use matrix_mod
  implicit none




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function poly1d( P, x ) result( y )
    !/ -----------------------------------------------------------------------------------
    !! Compute a polynomial
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: P(:)  !! polynomial coefficients
    real(dp), intent(in) :: x     !! independent variable
    real(dp)             :: y     !! dependent   variable
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(P)

    y = P(1)

    if ( 1.lt.n ) then
       do i=2,n
          y = y*x + P(i)
       end do
    end if
        
  end function poly1d


  !/ =====================================================================================
  subroutine PolyFit( P, x_list, y_list )
    !/ -----------------------------------------------------------------------------------
    !! fit an nth polynomial to a set of data.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: P(:)  !! polynomial coefficients
    real(dp), intent(in)  :: x_list(:)  !! independent variable list
    real(dp), intent(in)  :: y_list(:)  !! dependent   variable list
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c, nr, nc 
    real(dp) :: tx, t
    real(dp), allocatable, dimension(:,:) :: X, Y, XTX, invXTX, XTY, PM
    !/ -----------------------------------------------------------------------------------

    nc = size(P)
    nr = size(x_list)

    allocate( X(nr,nc) )
    allocate( Y(nr,1) )
    allocate( XTX(nc,nc) )
    allocate( invXTX(nc,nc) )
    allocate( XTY(nc,1) )
    allocate( PM(nc,1) )

    do r=1,nr
       Y(r,1) = y_list(r)
       tx     = x_list(r)
       t      = D_ONE
       do c=1,nc
          X(r,nc-c+1) = t
          t = t * tx
       end do
    end do

    call ATA( XTX, X )
    call ATB( XTY, X, Y )
    call inverse( invXTX, XTX )
    call dot( PM, invXTX, XTY )    

    do c=1,nc
       P(c) = PM(c,1)
    end do
    
    deallocate( PM )
    deallocate( XTY )
    deallocate( invXTX )
    deallocate( XTX )
    deallocate( Y )
    deallocate( X )

    
  end subroutine PolyFit


end module polyfit_mod


!/ =======================================================================================
!/ **                               P O L Y F I T _ M O D                               **
!/ =========================================================================== END FILE ==
