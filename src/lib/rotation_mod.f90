!/ ====================================================================== BEGIN FILE =====
!/ **                              R O T A T I O N _ M O D                              **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module rotation_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a tool for building rotations
  !!
  !! author:  Stephen W. Soliday
  !! date:    20198-11-17
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none

  !/ =====================================================================================
  type, public :: RotationMatrix
     !/ ----------------------------------------------------------------------------------
     !! Rotation
     !/ ----------------------------------------------------------------------------------

     real(dp) :: rmat(3,3) !! the rotation matrix

   contains

     procedure :: reset => reset_matrix
     procedure :: add   => append_rotation

     procedure, private :: mul_vector
     procedure, private :: mul_matrix

     generic :: mul => mul_vector, mul_matrix

  end type RotationMatrix



  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  !/ =====================================================================================
  subroutine reset_matrix( dts )
    !/ -----------------------------------------------------------------------------------
    !! Initialize a new btree node structure.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RotationMatrix), intent(out) :: dts !! reference to a RotationMatrix.
    !/ -----------------------------------------------------------------------------------
    dts%rmat(1,1) = D_ONE  ; dts%rmat(1,2) = D_ZERO ; dts%rmat(1,3) = D_ZERO
    dts%rmat(2,1) = D_ZERO ; dts%rmat(2,2) = D_ONE  ; dts%rmat(2,3) = D_ZERO
    dts%rmat(3,1) = D_ZERO ; dts%rmat(3,2) = D_ZERO ; dts%rmat(3,3) = D_ONE
  end subroutine reset_matrix


  !/ =====================================================================================
  subroutine append_rotation( dts, theta, num )
    !/ -----------------------------------------------------------------------------------
    !! Append a rotation to the Rotationmatrix.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RotationMatrix), intent(inout) :: dts   !! reference to a RotationMatrix.
    real(dp),              intent(in)    :: theta !! rotation angle 
    integer,               intent(in)    :: num   !! axis number 1-X, 2-Y, and 3-Z
    !/ -----------------------------------------------------------------------------------
    real(dp) :: CC, SS
    real(dp), save :: A(3,3), B(3,3)
    integer  :: i, j, k
    !/ -----------------------------------------------------------------------------------

    do j=1,3
       do i=1,3
          A(i,j) = dts%rmat(i,j)
       end do
    end do

    CC = cos(theta)
    SS = sin(theta)

    !/ TODO reorder for speed
    
    if ( 1.eq.num ) then
       !/ ----- Rotate points around the X-axis from positive Y to positive Z ------------
       B(1,1) =  D_ONE  ; B(1,2) =  D_ZERO ; B(1,3) =  D_ZERO
       B(2,1) =  D_ZERO ; B(2,2) =  CC     ; B(2,3) = -SS
       B(3,1) =  D_ZERO ; B(3,2) =  SS     ; B(3,3) =  CC

    else if ( 2.eq.num ) then
       !/ ----- Rotate points around the Y-axis from positive Z to positive X ------------
       B(1,1) =  CC     ; B(1,2) =  D_ZERO ; B(1,3) =  SS
       B(2,1) =  D_ZERO ; B(2,2) =  D_ONE  ; B(2,3) =  D_ZERO
       B(3,1) = -SS     ; B(3,2) =  D_ZERO ; B(3,3) =  CC

    else if ( 3.eq.num ) then
       !/ ----- Rotate points around the Z-axis from positive X to positive Y ------------
       B(1,1) =  CC     ; B(1,2) = -SS     ; B(1,3) =  D_ZERO
       B(2,1) =  SS     ; B(2,2) =  CC     ; B(2,3) =  D_ZERO
       B(3,1) =  D_ZERO ; B(3,2) =  D_ZERO ; B(3,3) =  D_ONE

    else
       write( ERROR_UNIT ) 'rotation_mod:append_rotation: NUM must be 1, 2, or 3'
    end if

    do i=1,3
       dts%rmat(i,1) =  B(i,1)*A(1,1) + B(i,2)*A(2,1) + B(i,3)*A(3,1)
       dts%rmat(i,2) =  B(i,1)*A(1,2) + B(i,2)*A(2,2) + B(i,3)*A(3,2)
       dts%rmat(i,3) =  B(i,1)*A(1,3) + B(i,2)*A(2,3) + B(i,3)*A(3,3)
    end do
    
  end subroutine append_rotation

  
  !/ =====================================================================================
  subroutine mul_vector( dts, vp, v )
    !/ -----------------------------------------------------------------------------------
    !! Apply the RotationMatrix to a single vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RotationMatrix), intent(in)  :: dts   !! reference to a RotationMatrix.
    real(dp),              intent(out) :: vp(3) !! output vector.
    real(dp),              intent(in)  :: v(3)  !! input vector.
    !/ -----------------------------------------------------------------------------------

    vp(1) = dts%rmat(1,1)*v(1) + dts%rmat(1,2)*v(2) + dts%rmat(1,3)*v(3)
    vp(2) = dts%rmat(2,1)*v(1) + dts%rmat(2,2)*v(2) + dts%rmat(2,3)*v(3)
    vp(3) = dts%rmat(3,1)*v(1) + dts%rmat(3,2)*v(2) + dts%rmat(3,3)*v(3)

  end subroutine mul_vector


  !/ =====================================================================================
  subroutine mul_matrix( dts, mp, m )
    !/ -----------------------------------------------------------------------------------
    !! Apply the RotationMatrix to an array of vectors.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RotationMatrix), intent(in)  :: dts     !! reference to a RotationMatrix.
    real(dp),              intent(out) :: mp(:,:) !! array of output vectors.
    real(dp),              intent(in)  :: m(:,:)  !! array of input vectors.
    !/ -----------------------------------------------------------------------------------
    integer :: i, nr_in, nr_out, nc_in, nc_out
    !/ -----------------------------------------------------------------------------------

    nr_out = size(mp,1)
    nc_out = size(mp,2)

    nr_in  = size(m,1)
    nc_in  = size(m,2)

    !/ ----- R = ROT * R -----------------------------------------------------------------
    
    if ( ( 3.eq.nr_out ).and.( 3.eq.nr_in ) ) then
       if ( nc_out.ne.nc_in ) then
          call log_error('dims do not match')
          goto 999
       end if
       do i=1,nc_out
          mp(1,i) = dts%rmat(1,1)*m(1,i) + dts%rmat(1,2)*m(2,i) + dts%rmat(1,3)*m(3,i)
          mp(2,i) = dts%rmat(2,1)*m(1,i) + dts%rmat(2,2)*m(2,i) + dts%rmat(2,3)*m(3,i)
          mp(3,i) = dts%rmat(3,1)*m(1,i) + dts%rmat(3,2)*m(2,i) + dts%rmat(3,3)*m(3,i)
       end do
       goto 999
    end if

    !/ ----- C = ROT * C -----------------------------------------------------------------
    
    if ( ( 3.eq.nc_out ).and.( 3.eq.nc_in ) ) then
       if ( nr_out.ne.nr_in ) then
          call log_error('dims do not match')
          goto 999
       end if
       do i=1,nr_out
          mp(i,1) = dts%rmat(1,1)*m(i,1) + dts%rmat(1,2)*m(i,2) + dts%rmat(1,3)*m(i,3)
          mp(i,2) = dts%rmat(2,1)*m(i,1) + dts%rmat(2,2)*m(i,2) + dts%rmat(2,3)*m(i,3)
          mp(i,3) = dts%rmat(3,1)*m(i,1) + dts%rmat(3,2)*m(i,2) + dts%rmat(3,3)*m(i,3)
       end do
       goto 999
    end if

    !/ ----- C = ROT * R -----------------------------------------------------------------
    
    if ( ( 3.eq.nc_out ).and.( 3.eq.nr_in ) ) then
       if ( nr_out.ne.nc_in ) then
          call log_error('dims do not match')
          goto 999
       end if
       do i=1,nr_out
          mp(i,1) = dts%rmat(1,1)*m(1,i) + dts%rmat(1,2)*m(2,i) + dts%rmat(1,3)*m(3,i)
          mp(i,2) = dts%rmat(2,1)*m(1,i) + dts%rmat(2,2)*m(2,i) + dts%rmat(2,3)*m(3,i)
          mp(i,3) = dts%rmat(3,1)*m(1,i) + dts%rmat(3,2)*m(2,i) + dts%rmat(3,3)*m(3,i)
       end do
       goto 999
    end if

    !/ ----- R = ROT * C -----------------------------------------------------------------
    
    if ( ( 3.eq.nr_out ).and.( 3.eq.nc_in ) ) then
       if ( nc_out.ne.nr_in ) then
          call log_error('dims do not match')
          goto 999
       end if
       do i=1,nc_out
          mp(1,i) = dts%rmat(1,1)*m(i,1) + dts%rmat(1,2)*m(i,2) + dts%rmat(1,3)*m(i,3)
          mp(2,i) = dts%rmat(2,1)*m(i,1) + dts%rmat(2,2)*m(i,2) + dts%rmat(2,3)*m(i,3)
          mp(3,i) = dts%rmat(3,1)*m(i,1) + dts%rmat(3,2)*m(i,2) + dts%rmat(3,3)*m(i,3)
       end do
       goto 999
    end if

999 continue

  end subroutine mul_matrix


end module rotation_mod


!/ =======================================================================================
!/ **                               B T R E E _ C L A S S                               **
!/ =========================================================================== END FILE ==
