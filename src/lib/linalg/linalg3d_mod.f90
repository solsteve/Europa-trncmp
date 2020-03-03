!/ ====================================================================== BEGIN FILE =====
!/ **                              L I N A L G 3 D _ M O D                              **
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
module linalg3d_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-02-06
  !! license: GPL
  !!
  !!## 3D Linear Algebra
  !!
  !! Collection of procedures for performing linear algebra in 3D.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use vector3d_mod
  use matrix3d_mod
  
  !/ -------------------------------------------------------------------------------------
  interface diagonal !! 
     !/ ----------------------------------------------------------------------------------
     module procedure :: la3_diagonal
  end interface diagonal

  !/ -------------------------------------------------------------------------------------
  interface DiagMul !! Multiply a diagonal and a matrix
     !/ ----------------------------------------------------------------------------------
     module procedure :: la3_mul_dm
  end interface DiagMul

  !/ -------------------------------------------------------------------------------------
  interface outer !! 
     !/ ----------------------------------------------------------------------------------
     module procedure :: la3_outer_multiply
  end interface outer


  !/ -------------------------------------------------------------------------------------
  interface dot !! 
     !/ ----------------------------------------------------------------------------------
     module procedure :: la3_dot_mm
     module procedure :: la3_dot_mv
     module procedure :: la3_dot_vm
  end interface dot


  !/ -------------------------------------------------------------------------------------
  interface ATA !! 
     !/ ----------------------------------------------------------------------------------
     module procedure :: dot_ATA
  end interface ATA


  !/ -------------------------------------------------------------------------------------
  interface AAT !! 
     !/ ----------------------------------------------------------------------------------
     module procedure :: dot_AAT
  end interface AAT




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine la3_diagonal( mat, GET, SET, ZERO )
    !/ -----------------------------------------------------------------------------------
    !! Get the diagonal
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D),           intent(inout) :: mat  !! reference to this Matrix3D.
    type(Vector3D), optional, intent(inout) :: GET  !! diagonal
    type(vector3D), optional, intent(in)    :: SET  !! diagonal
    logical,        optional, intent(in)    :: ZERO !! flag to zero off diagonal elements
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( present( GET ) ) then
       do concurrent( i=1:3 )
          GET%v(i) = mat%m(i,i)
       end do
    end if

    if ( present( SET ) ) then
       do concurrent( i=1:3 )
          mat%m(i,i) = SET%v(i)
       end do
    end if

    if ( present( ZERO ) ) then
       if ( ZERO ) then
          mat%m(2,1) = D_ZERO
          mat%m(3,1) = D_ZERO
          mat%m(1,2) = D_ZERO
          mat%m(3,2) = D_ZERO
          mat%m(1,3) = D_ZERO
          mat%m(2,3) = D_ZERO
       end if
    end if
    
  end subroutine la3_diagonal




 
  
  !/ =====================================================================================
  subroutine la3_mul_dm( mat, ldia, rmat )
    !/ -----------------------------------------------------------------------------------
    !! multiply a diagonal with a matrix
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: mat  !! resulting Matrix
    type(Vector3D), intent(in)    :: ldia !! left diagonal
    type(Matrix3d), intent(in)    :: rmat !! right Matrix3D
    !/ -----------------------------------------------------------------------------------
    integer :: r, c
    !/ -----------------------------------------------------------------------------------
    do c=1,3
       do concurrent( r=1:3 )
          mat%m(r,c) = ldia%v(r) * rmat%m(r,c)
       end do
    end do
  end subroutine la3_mul_dm





  !/ =====================================================================================
  subroutine la3_outer_multiply( mat, lvec, rvec )
    !/ -----------------------------------------------------------------------------------
    !! Generate a matrix from the outer product of two vectors.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: mat  !! resulting Matrix3D
    type(Vector3D), intent(in)    :: lvec !! left Vector3D
    type(Vector3D), intent(in)    :: rvec !! right Vector3D
    !/ -----------------------------------------------------------------------------------
    integer :: i, j
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       mat%m(i,j) = lvec%v(i) * rvec%v(j)
    end do
  end subroutine la3_outer_multiply


  !/ =====================================================================================
  subroutine la3_dot_mm( mat, lmat, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Matrix-Matrix multiply. $V = M_l /cdot M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: mat  !! resulting Matrix3D
    type(Matrix3D), intent(in)    :: lmat !! left      Matrix3D
    type(Matrix3D), intent(in)    :: rmat !! right     Matrix3D
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, k
    real(dp) :: t
    !/ -----------------------------------------------------------------------------------
    do concurrent( i=1:3, j=1:3 )
       mat%m(i,j) = D_ZERO
    end do

    do concurrent( j=1:3, k=1:3 )
       t = rmat%m(k,j)
       do i=1,3
          mat%m(i,j) = mat%m(i,j) + t*lmat%m(i,k)
       end do
    end do

  end subroutine la3_dot_mm


  !/ =====================================================================================
  subroutine la3_dot_mv( vec, lmat, rvec )
    !/ -----------------------------------------------------------------------------------
    !! Matrix-vector multiply. $V = M_l /cdot V_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(inout) :: vec  !! resulting Vector3D
    type(Matrix3D), intent(in)    :: lmat !! left      Matrix3D
    type(Vector3D), intent(in)    :: rvec !! right     Vector3D
    !/ -----------------------------------------------------------------------------------
    vec%v(1) = lmat%m(1,1)*rvec%v(1) + lmat%m(1,2)*rvec%v(2) + lmat%m(1,3)*rvec%v(3)
    vec%v(2) = lmat%m(2,1)*rvec%v(1) + lmat%m(2,2)*rvec%v(2) + lmat%m(2,3)*rvec%v(3)
    vec%v(3) = lmat%m(3,1)*rvec%v(1) + lmat%m(3,2)*rvec%v(2) + lmat%m(3,3)*rvec%v(3)
  end subroutine la3_dot_mv


  !/ =====================================================================================
  subroutine la3_dot_vm( vec, lvec, rmat )
    !/ -----------------------------------------------------------------------------------
    !! Vector-matrix multiply. $V = V_l /cdot M_r$
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(inout) :: vec  !! resulting Vector3D
    type(Vector3D), intent(in)    :: lvec !! left      Vector3D
    type(Matrix3D), intent(in)    :: rmat !! right     Matrix3D
    !/ -----------------------------------------------------------------------------------
    vec%v(1) = lvec%v(1)*rmat%m(1,1) + lvec%v(2)*rmat%m(2,1) + lvec%v(3)*rmat%m(3,1)
    vec%v(2) = lvec%v(1)*rmat%m(1,2) + lvec%v(2)*rmat%m(2,2) + lvec%v(3)*rmat%m(3,2)
    vec%v(3) = lvec%v(1)*rmat%m(1,3) + lvec%v(2)*rmat%m(2,3) + lvec%v(3)*rmat%m(3,3)
  end subroutine la3_dot_vm


  

  !/ =======================================================================================
  subroutine dot_ATA( B, A )
    !/ -------------------------------------------------------------------------------------
    !! Inner product of a Transpose of a Matrix with itself.  $$B = A^T \cdot A$$
    !/ -------------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: B !! reference to the resulting Matrix3D.
    type(Matrix3D), intent(in) :: A !! reference to the source    Matrix3D.
    !/ -------------------------------------------------------------------------------------
    integer :: r,c,k
    !/ -------------------------------------------------------------------------------------
    do concurrent( r=1:3, c=1:3 )
       B%m(r,c) = D_ZERO
    end do
    
    do concurrent( r=1:3, c=1:3, k=1:3 )
       B%m(r,c) = B%m(r,c) + ( A%m(k,r) * A%m(k,c) )
    end do
    
  end subroutine dot_ATA

  !/ =======================================================================================
  subroutine dot_AAT( B, A )
    !/ -------------------------------------------------------------------------------------
    !! inner product of a Matrix with its own transpose. $$B = A \cdot A^T$$
    !/ -------------------------------------------------------------------------------------
    implicit none
    type(Matrix3D), intent(inout) :: B !! reference to the resulting Matrix3D.
    type(Matrix3D), intent(in) :: A !! reference to the source    Matrix3D.
    !/ -------------------------------------------------------------------------------------
    integer :: r, c, k
    !/ -------------------------------------------------------------------------------------
    do concurrent( r=1:3, c=1:3 )
       B%m(r,c) = D_ZERO
    end do
    
    do concurrent( r=1:3, c=1:3, k=1:3 )
       B%m(r,c) = B%m(r,c) + ( A%m(r,k) * A%m(c,k) )
    end do
    
  end subroutine dot_AAT





 

end module linalg3d_mod


!/ =======================================================================================
!/ **                              L I N A L G 3 D _ M O D                              **
!/ =========================================================================== END FILE ==
