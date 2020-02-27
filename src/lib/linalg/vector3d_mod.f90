!/ ====================================================================== BEGIN FILE =====
!/ **                              V E C T O R 3 D _ M O D                              **
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
module vector3d_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-01-31
  !! license: GPL
  !!
  !!## 3D Vector Routines
  !!
  !! Collection of procedures for manipulating 3D vectors.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger

  !/ -------------------------------------------------------------------------------------
  type Vector3D
     !/ ----------------------------------------------------------------------------------
     real(dp) :: v(3)
   contains
     procedure :: X      => v3_get_x
     procedure :: Y      => v3_get_y
     procedure :: Z      => v3_get_z

     procedure :: zero   => v3_zero_all
     procedure :: copy   => v3_copy_internal
     procedure :: dot    => v3_dot_internal
     procedure :: equals => v3_compare
     procedure :: cross  => v3_cross_internal
     procedure :: mag    => v3_mag_internal
     procedure :: L1     => v3_L1_norm_internal
     procedure :: L2     => v3_L2_norm_internal    ! a.k.a.  mag^2
     procedure :: LN     => v3_LN_norm_internal

     procedure :: numpy  => v3_to_numpy
     procedure :: matlab => v3_to_matlab
     procedure :: sage   => v3_to_sage

     ! fromAZEL
     ! toAZEL, getAzm, getElv

     ! InfinityNorm max(x,max(y,z))

     procedure, private :: v3_set_all
     procedure, private :: v3_set_array

     procedure, private :: v3_add_v_internal
     procedure, private :: v3_add_s_internal
     procedure, private :: v3_add_vv_internal
     procedure, private :: v3_add_vs_internal
     procedure, private :: v3_add_sv_internal

     procedure, private :: v3_sub_v_internal
     procedure, private :: v3_sub_s_internal
     procedure, private :: v3_sub_vv_internal
     procedure, private :: v3_sub_vs_internal
     procedure, private :: v3_sub_sv_internal

     procedure, private :: v3_mul_v_internal
     procedure, private :: v3_mul_s_internal
     procedure, private :: v3_mul_vv_internal
     procedure, private :: v3_mul_vs_internal
     procedure, private :: v3_mul_sv_internal

     procedure, private :: v3_div_v_internal
     procedure, private :: v3_div_s_internal
     procedure, private :: v3_div_vv_internal
     procedure, private :: v3_div_vs_internal
     procedure, private :: v3_div_sv_internal

     generic :: set => v3_set_all, v3_set_array

     generic :: add => v3_add_v_internal, v3_add_s_internal,  &
          &            v3_add_vv_internal, v3_add_vs_internal, v3_add_sv_internal

     generic :: sub => v3_sub_v_internal, v3_sub_s_internal,  &
          &            v3_sub_vv_internal, v3_sub_vs_internal, v3_sub_sv_internal

     generic :: mul => v3_mul_v_internal, v3_mul_s_internal,  &
          &            v3_mul_vv_internal, v3_mul_vs_internal, v3_mul_sv_internal

     generic :: div => v3_div_v_internal, v3_div_s_internal,  &
          &            v3_div_vv_internal, v3_div_vs_internal, v3_div_sv_internal

  end type Vector3D




  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_size_external
  end interface size

  !/ -------------------------------------------------------------------------------------
  interface zero
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_zero_external
  end interface zero

  !/ -------------------------------------------------------------------------------------
  interface copy
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_copy_va_external
     module procedure :: v3_copy_av_external
     module procedure :: v3_copy_vv_external
  end interface copy


  !/ -------------------------------------------------------------------------------------
  interface angle
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_angle_external
  end interface angle


  !/ -------------------------------------------------------------------------------------
  interface norm
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_normalize
  end interface norm


  !/ -------------------------------------------------------------------------------------
  interface L1_norm
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_L1_norm_external
  end interface L1_norm


  !/ -------------------------------------------------------------------------------------
  interface L2_norm
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_L2_norm_external
  end interface L2_norm


  !/ -------------------------------------------------------------------------------------
  interface LN_norm
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_LN_norm_external
  end interface LN_norm


  !/ -------------------------------------------------------------------------------------
  interface toString
     !/ ----------------------------------------------------------------------------------
     module procedure :: v3_to_string
  end interface toString




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function v3_get_x( dts ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convineance function for accessing the Abscissa (first axis element) by name.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this Vector3D.
    real(dp)                       :: val !! return value.
    !/ -----------------------------------------------------------------------------------
    val = dts%v(1)
  end function v3_get_x


  !/ =====================================================================================
  function v3_get_y( dts ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convineance function for accessing the Ordinate (second axis element) by name.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this Vector3D.
    real(dp)                       :: val !! return value.
    !/ -----------------------------------------------------------------------------------
    val = dts%v(2)
  end function v3_get_y


  !/ =====================================================================================
  function v3_get_z( dts ) result( val )
    !/ -----------------------------------------------------------------------------------
    !! Convineance function for accessing the Height (third axis element) by name.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this Vector3D.
    real(dp)                       :: val !! return value.
    !/ -----------------------------------------------------------------------------------
    val = dts%v(3)
  end function v3_get_z


  !/ =====================================================================================
  subroutine v3_zero_all( dts )
    !/ -----------------------------------------------------------------------------------
    !! Zero the elements of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this Vector3D.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = D_ZERO
    dts%v(2) = D_ZERO
    dts%v(3) = D_ZERO
  end subroutine v3_zero_all


  !/ =====================================================================================
  subroutine v3_copy_internal( dts, src )
    !/ -----------------------------------------------------------------------------------
    !! Copy the elements of a source Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this Vector3D.
    type(Vector3D),  intent(in)    :: src !! reference to a source Vector3D.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = src%v(1)
    dts%v(2) = src%v(2)
    dts%v(3) = src%v(3)
  end subroutine v3_copy_internal


  !/ =====================================================================================
  function v3_compare( dts, V ) result( cmp )
    !/ -----------------------------------------------------------------------------------
    !! Perform a element by element comparison of two vectors
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(in) :: dts !! reference to this Vector3D.
    type(Vector3D),  intent(in) :: V   !! reference to another Vector3D.
    logical                     :: cmp
    !/ -----------------------------------------------------------------------------------

    cmp = .true.
    if ( dts%v(1).lt.V%v(1) ) then
       cmp = .false.
       goto 999
    end if
    if ( dts%v(2).lt.V%v(2) ) then
       cmp = .false.
       goto 999
    end if
    if ( dts%v(3).lt.V%v(3) ) then
       cmp = .false.
       goto 999
    end if
    if ( dts%v(1).gt.V%v(1) ) then
       cmp = .false.
       goto 999
    end if
    if ( dts%v(2).gt.V%v(2) ) then
       cmp = .false.
       goto 999
    end if
    if ( dts%v(3).gt.V%v(3) ) then
       cmp = .false.
       goto 999
    end if
999 continue
  end function v3_compare


  !/ =====================================================================================
  function v3_dot_internal( dts, rv ) result( ip )
    !/ -----------------------------------------------------------------------------------
    !! Inner product.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(in) :: dts !! reference to this Vector3D.
    type(Vector3D),  intent(in) :: rv  !! reference to a right Vector3D.
    real(dp)                    :: ip  !! dot product
    !/ -----------------------------------------------------------------------------------
    ip = (dts%v(1)*rv%v(1)) + (dts%v(2)*rv%v(2)) + (dts%v(3)*rv%v(3))
  end function v3_dot_internal


  !/ =====================================================================================
  subroutine v3_cross_internal( dts, A, B )
    !/ -----------------------------------------------------------------------------------
    !/ Make this Vector3D the right-hand cross product of A into B.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this   Vector3D.
    type(Vector3D),  intent(in)    :: A   !! reference to a from Vector3D.
    type(Vector3D),  intent(in)    :: B   !! reference to a to   Vector3D.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = A%v(2)*B%v(3) - A%v(3)*B%v(2)
    dts%v(2) = A%v(3)*B%v(1) - A%v(1)*B%v(3)
    dts%v(3) = A%v(1)*B%v(2) - A%v(2)*B%v(1)
  end subroutine v3_cross_internal


  !/ =====================================================================================
  function v3_mag_internal( dts ) result( m )
    !/ -----------------------------------------------------------------------------------
    !! Magnitude of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(in) :: dts !! reference to this Vector3D.
    real(dp)                    :: m   !! magnitude.
    !/ -----------------------------------------------------------------------------------
    m = sqrt( (dts%v(1)*dts%v(1)) + (dts%v(2)*dts%v(2)) + (dts%v(3)*dts%v(3)) )
  end function v3_mag_internal


  !/ =====================================================================================
  function v3_L1_norm_internal( dts ) result( L )
    !/ -----------------------------------------------------------------------------------
    !! L1 Norm of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(in) :: dts !! reference to this Vector3D.
    real(dp)                    :: L   !! L1 Norm
    !/ -----------------------------------------------------------------------------------
    L = abs(dts%v(1)) + abs(dts%v(2)) + abs(dts%v(3))
  end function v3_L1_norm_internal


  !/ =====================================================================================
  function v3_L2_norm_internal( dts ) result( L )
    !/ -----------------------------------------------------------------------------------
    !! L2 Norm of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(in) :: dts !! reference to this Vector3D.
    real(dp)                    :: L   !! L2 Norm
    !/ -----------------------------------------------------------------------------------
    L = (dts%v(1)*dts%v(1)) + (dts%v(2)*dts%v(2)) + (dts%v(3)*dts%v(3))
  end function v3_L2_norm_internal


  !/ =====================================================================================
  function v3_LN_norm_internal( dts, n ) result( L )
    !/ -----------------------------------------------------------------------------------
    !! LN Norm of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(in) :: dts !! reference to this Vector3D.
    integer,         intent(in) :: n   !! power > 1
    real(dp)                    :: L   !! LN Norm
    !/ -----------------------------------------------------------------------------------
    L = (dts%v(1)**n) + (dts%v(2)**n) + (dts%v(3)**n)
  end function v3_LN_norm_internal




  !/ =====================================================================================
  function v3_to_numpy( dts, FMT, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Format this Vector3D to be read as a numpy array
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D),        intent(in)  :: dts   !! reference to a Vector3D.
    character(*), optional, intent(in)  :: FMT   !! edit descriptor (defualt: G0).
    logical, optional,      intent(in)  :: TRANS !! transpose (defualt: .false.).
    character(len=:),       allocatable :: str   !! string formated as a vector
    !/ -----------------------------------------------------------------------------------
    str = 'np.array( ' // toString( dts, FMT=FMT, TRANS=TRANS ) // ' )'
  end function v3_to_numpy


  !/ =====================================================================================
  function v3_to_matlab( dts, FMT, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Format this Vector3D to be read as a numpy array
    !/ -----------------------------------------------------------------------------------
    use string_tools, only : toString
    implicit none
    class(Vector3D),        intent(in)  :: dts   !! reference to a Vector3D.
    character(*), optional, intent(in)  :: FMT   !! edit descriptor (defualt: G0).
    logical, optional,      intent(in)  :: TRANS !! transpose (defualt: .false.).
    character(len=:),       allocatable :: str   !! string formated as a vector
    !/ -----------------------------------------------------------------------------------
    str = '[' // toString( dts%v, FMT=FMT, DEL=' ' ) // ']'
    if ( present( TRANS ) ) then
       str = str // "'"
    end if
  end function v3_to_matlab


  !/ =====================================================================================
  function v3_to_sage( dts, FMT, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Format this Vector3D to be read as a numpy array
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D),        intent(in)  :: dts   !! reference to a Vector3D.
    character(*), optional, intent(in)  :: FMT   !! edit descriptor (defualt: G0).
    logical, optional,      intent(in)  :: TRANS !! transpose (defualt: .false.).
    character(len=:),       allocatable :: str   !! string formated as a vector
    !/ -----------------------------------------------------------------------------------
    str = 'Matrix(SR, ' // toString( dts, FMT=FMT, TRANS=TRANS ) // ' )'
    if ( present( TRANS ) ) then
       str = str // ".transpose()"
    end if
  end function v3_to_sage


  !/ =====================================================================================
  subroutine v3_set_all( dts, X, Y, Z, VAL )
    !/ -----------------------------------------------------------------------------------
    !! Set individual axis elements. If none are selected zero this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this Vector3D.
    real(dp), optional, intent(in) :: X   !! first  axis value
    real(dp), optional, intent(in) :: Y   !! second axis value
    real(dp), optional, intent(in) :: Z   !! third  axis value
    real(dp), optional, intent(in) :: VAL !! value to set all
    !/ -----------------------------------------------------------------------------------
    integer :: count
    !/ -----------------------------------------------------------------------------------
    count = 0

    if (present(VAL)) then
       count = count + 3
       dts%v(1) = VAL
       dts%v(2) = VAL
       dts%v(3) = VAL
    end if

    if (present(X)) then
       count = count + 1
       dts%v(1) = X
    end if

    if (present(Y)) then
       count = count + 1
       dts%v(2) = Y
    end if

    if (present(Z)) then
       count = count + 1
       dts%v(3) = Z
    end if

    if (0.eq.count) then
       dts%v(1) = D_ZERO
       dts%v(2) = D_ZERO
       dts%v(3) = D_ZERO
    end if
  end subroutine v3_set_all


  !/ =====================================================================================
  subroutine v3_set_array( dts, ary )
    !/ -----------------------------------------------------------------------------------
    !! Copy the elements of an array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts   !! reference to this Vector3D.
    real(dp),        intent(in)    :: ary(3)!! source array.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = ary(1)
    dts%v(2) = ary(2)
    dts%v(3) = ary(3)
  end subroutine v3_set_array








  !/ =====================================================================================
  subroutine v3_add_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Add a scalar to each element of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    real(dp),        intent(in)    :: rs  !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) + rs
    dts%v(2) = dts%v(2) + rs
    dts%v(3) = dts%v(3) + rs
  end subroutine v3_add_s_internal


  !/ =====================================================================================
  subroutine v3_add_v_internal( dts, rv )
    !/ -----------------------------------------------------------------------------------
    !! Add each element of a right side Vector3D to the coresponding element of
    !! this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    type(Vector3D),  intent(in)    :: rv  !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) + rv%v(1)
    dts%v(2) = dts%v(2) + rv%v(2)
    dts%v(3) = dts%v(3) + rv%v(3)
  end subroutine v3_add_v_internal


  !/ =====================================================================================
  subroutine v3_add_sv_internal( dts, ls, rv )
    !/ -----------------------------------------------------------------------------------
    !! Add a left side scalar to each element of a right side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    real(dp),       intent(in)  :: ls   !! left side scalar.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = ls + rv%v(1)
    dts%v(2) = ls + rv%v(2)
    dts%v(3) = ls + rv%v(3)
  end subroutine v3_add_sv_internal


  !/ =====================================================================================
  subroutine v3_add_vs_internal( dts, lv, rs )
    !/ -----------------------------------------------------------------------------------
    !! Add a right side scalar to each element of a left side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    real(dp),       intent(in)  :: rs   !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) + rs
    dts%v(2) = lv%v(2) + rs
    dts%v(3) = lv%v(3) + rs
  end subroutine v3_add_vs_internal


  !/ =====================================================================================
  subroutine v3_add_vv_internal( dts, lv, rv )
    !/ -----------------------------------------------------------------------------------
    !! Add each element of a left side Vector3D with the coresponding element of a right
    !! side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) + rv%v(1)
    dts%v(2) = lv%v(2) + rv%v(2)
    dts%v(3) = lv%v(3) + rv%v(3)
  end subroutine v3_add_vv_internal








  !/ =====================================================================================
  subroutine v3_sub_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Subtract a scalar from each element of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    real(dp),        intent(in)    :: rs  !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) - rs
    dts%v(2) = dts%v(2) - rs
    dts%v(3) = dts%v(3) - rs
  end subroutine v3_sub_s_internal


  !/ =====================================================================================
  subroutine v3_sub_v_internal( dts, rv )
    !/ -----------------------------------------------------------------------------------
    !! Subtract each element of a right side Vector3D from the coresponding element
    !! of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    type(Vector3D),  intent(in)    :: rv  !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) - rv%v(1)
    dts%v(2) = dts%v(2) - rv%v(2)
    dts%v(3) = dts%v(3) - rv%v(3)
  end subroutine v3_sub_v_internal


  !/ =====================================================================================
  subroutine v3_sub_sv_internal( dts, ls, rv )
    !/ -----------------------------------------------------------------------------------
    !! Subtract each element of a right side Vector3D from a left side scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    real(dp),       intent(in)  :: ls   !! left side scalar.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = ls - rv%v(1)
    dts%v(2) = ls - rv%v(2)
    dts%v(3) = ls - rv%v(3)
  end subroutine v3_sub_sv_internal


  !/ =====================================================================================
  subroutine v3_sub_vs_internal( dts, lv, rs )
    !/ -----------------------------------------------------------------------------------
    !! Subtract a right side scalar from each element of a left side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    real(dp),       intent(in)  :: rs   !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) - rs
    dts%v(2) = lv%v(2) - rs
    dts%v(3) = lv%v(3) - rs
  end subroutine v3_sub_vs_internal


  !/ =====================================================================================
  subroutine v3_sub_vv_internal( dts, lv, rv )
    !/ -----------------------------------------------------------------------------------
    !! Subtract each element of a right side Vector3D from the corespomding element of a
    !! left side Vector3D
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) - rv%v(1)
    dts%v(2) = lv%v(2) - rv%v(2)
    dts%v(3) = lv%v(3) - rv%v(3)
  end subroutine v3_sub_vv_internal








  !/ =====================================================================================
  subroutine v3_mul_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of this Vector3D with a scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    real(dp),        intent(in)    :: rs  !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) * rs
    dts%v(2) = dts%v(2) * rs
    dts%v(3) = dts%v(3) * rs
  end subroutine v3_mul_s_internal


  !/ =====================================================================================
  subroutine v3_mul_v_internal( dts, rv )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of this Vector3D with the coresponding element of a right
    !! side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    type(Vector3D),  intent(in)    :: rv  !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) * rv%v(1)
    dts%v(2) = dts%v(2) * rv%v(2)
    dts%v(3) = dts%v(3) * rv%v(3)
  end subroutine v3_mul_v_internal


  !/ =====================================================================================
  subroutine v3_mul_vs_internal( dts, lv, rs )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of a left side Vector3D with a right side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    real(dp),       intent(in)  :: rs   !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) * rs
    dts%v(2) = lv%v(2) * rs
    dts%v(3) = lv%v(3) * rs
  end subroutine v3_mul_vs_internal


  !/ =====================================================================================
  subroutine v3_mul_sv_internal( dts, ls, rv )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of a right side Vector3D with a left side scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    real(dp),       intent(in)  :: ls   !! left side scalar.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = ls * rv%v(1)
    dts%v(2) = ls * rv%v(2)
    dts%v(3) = ls * rv%v(3)
  end subroutine v3_mul_sv_internal


  !/ =====================================================================================
  subroutine v3_mul_vv_internal( dts, lv, rv )
    !/ -----------------------------------------------------------------------------------
    !! Multiply each element of a left side Vector3D with the coresponding element of a
    !! right side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) * rv%v(1)
    dts%v(2) = lv%v(2) * rv%v(2)
    dts%v(3) = lv%v(3) * rv%v(3)
  end subroutine v3_mul_vv_internal








  !/ =====================================================================================
  subroutine v3_div_s_internal( dts, rs )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of this Vector3D by a scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    real(dp),        intent(in)    :: rs  !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) / rs
    dts%v(2) = dts%v(2) / rs
    dts%v(3) = dts%v(3) / rs
  end subroutine v3_div_s_internal


  !/ =====================================================================================
  subroutine v3_div_v_internal( dts, rv )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of this Vector3D by the coresponding element of a right
    !! side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to this left side Vector3D.
    type(Vector3D),  intent(in)    :: rv  !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = dts%v(1) / rv%v(1)
    dts%v(2) = dts%v(2) / rv%v(2)
    dts%v(3) = dts%v(3) / rv%v(3)
  end subroutine v3_div_v_internal


  !/ =====================================================================================
  subroutine v3_div_sv_internal( dts, ls, rv )
    !/ -----------------------------------------------------------------------------------
    !! Divide a left side scalar by each element of a right side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    real(dp),       intent(in)  :: ls   !! left side scalar.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = ls / rv%v(1)
    dts%v(2) = ls / rv%v(2)
    dts%v(3) = ls / rv%v(3)
  end subroutine v3_div_sv_internal


  !/ =====================================================================================
  subroutine v3_div_vs_internal( dts, lv, rs )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of a left side Vector3D by a right side scalar.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    real(dp),       intent(in)  :: rs   !! right side scalar.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) / rs
    dts%v(2) = lv%v(2) / rs
    dts%v(3) = lv%v(3) / rs
  end subroutine v3_div_vs_internal


  !/ =====================================================================================
  subroutine v3_div_vv_internal( dts, lv, rv )
    !/ -----------------------------------------------------------------------------------
    !! Divide each element of a left side Vector3D by the coresponding element of a right
    !! side Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Vector3D), intent(inout) :: dts !! reference to the output vector.
    type(Vector3D), intent(in)  :: lv   !! left side vector.
    type(Vector3D), intent(in)  :: rv   !! right side vector.
    !/ -----------------------------------------------------------------------------------
    dts%v(1) = lv%v(1) / rv%v(1)
    dts%v(2) = lv%v(2) / rv%v(2)
    dts%v(3) = lv%v(3) / rv%v(3)
  end subroutine v3_div_vv_internal








  !/ =====================================================================================
  function v3_size_external( vec ) result( n )
    !/ -----------------------------------------------------------------------------------
    type(Vector3D), intent(in) :: vec !! reference to a Vector3D.
    !/ -----------------------------------------------------------------------------------
    n = 3
  end function v3_size_external


  !/ =====================================================================================
  subroutine v3_zero_external( vec )
    !/ -----------------------------------------------------------------------------------
    !! Zero the elements of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(inout) :: vec !! reference to a Vector3D.
    !/ -----------------------------------------------------------------------------------
    vec%v(1) = D_ZERO
    vec%v(2) = D_ZERO
    vec%v(3) = D_ZERO
  end subroutine v3_zero_external


  !/ =====================================================================================
  subroutine v3_copy_va_external( dstvec, srcary )
    !/ -----------------------------------------------------------------------------------
    !! Copy the elements of an array into a Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(inout) :: dstvec     !! reference to a Vector3D.
    real(dp),       intent(in)    :: srcary(3)  !! source array.
    !/ -----------------------------------------------------------------------------------
    dstvec%v(1) = srcary(1)
    dstvec%v(2) = srcary(2)
    dstvec%v(3) = srcary(3)
  end subroutine v3_copy_va_external


  !/ =====================================================================================
  subroutine v3_copy_av_external( dstary, srcvec )
    !/ -----------------------------------------------------------------------------------
    !! Copy the elements of a Vector3D into an array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),       intent(inout) :: dstary(3)  !! source array.
    type(Vector3D), intent(in)    :: srcvec     !! reference to a Vector3D.
    !/ -----------------------------------------------------------------------------------
    dstary(1) = srcvec%v(1)
    dstary(2) = srcvec%v(2)
    dstary(3) = srcvec%v(3)
  end subroutine v3_copy_av_external


  !/ =====================================================================================
  subroutine v3_copy_vv_external( dstvec, srcvec )
    !/ -----------------------------------------------------------------------------------
    !! Copy the elements of a Vector3D into a VEctor3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(inout) :: dstvec !! reference to a Vector3D.
    type(Vector3D), intent(in)    :: srcvec !! reference to a Vector3D.
    !/ -----------------------------------------------------------------------------------
    dstvec%v(1) = srcvec%v(1)
    dstvec%v(2) = srcvec%v(2)
    dstvec%v(3) = srcvec%v(3)
  end subroutine v3_copy_vv_external


  !/ =====================================================================================
  function v3_angle_external( A, B ) result( rad )
    !/ -----------------------------------------------------------------------------------
    !! Compute the angular separation between two Vector3D objects.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(in) :: A    !! left  side Vector3D.
    type(Vector3D), intent(in) :: B    !! right side Vector3D.
    real(dp)                   :: rad  !! angle between the two Vector3D's in radians.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: np, dt, th
    type(Vector3D) :: C
    !/ -----------------------------------------------------------------------------------
    np = A%mag() * B%mag()
    ! print *, 'NP=', np
    if ( np.gt.D_EPSILON ) goto 100
    call log_warn( 'V3 Angle: Zero Norm' )
    rad = D_ZERO
    goto 999

100 continue

    dt = A%dot( B )
    !print *, 'DT=', dt

    th = np * 9.999D-1
    !print *, 'TH=', th

    if ( (dt.lt.th).or.(dt.gt.th) ) then
       !  the vectors are almost aligned, compute using the sine
       call C%cross( A, B )
       rad = asin( C%mag() / np )
       if ( dt.lt.D_ZERO ) then
          rad = D_PI - rad
       end if
    else
       !  the vectors are sufficiently separated to use the cosine
       rad = acos( dt / np )
    end if

999 continue
  end function v3_angle_external








  !/ =====================================================================================
  function v3_L1_norm_external( lvec, rvec ) result( L )
    !/ -----------------------------------------------------------------------------------
    !! L1 Norm of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(in) :: lvec !! reference to the left  Vector3D.
    type(Vector3D), intent(in) :: rvec !! reference to the right Vector3D.
    real(dp)                   :: L    !! L1 Norm
    !/ -----------------------------------------------------------------------------------
    L = abs( lvec%v(1) - rvec%v(1) ) + &
         &   abs( lvec%v(2) - rvec%v(2) ) + &
         &   abs( lvec%v(3) - rvec%v(3) )
  end function v3_L1_norm_external


  !/ =====================================================================================
  function v3_L2_norm_external( lvec, rvec ) result( L )
    !/ -----------------------------------------------------------------------------------
    !! L2 Norm of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(in) :: lvec !! reference to the left  Vector3D.
    type(Vector3D), intent(in) :: rvec !! reference to the right Vector3D.
    real(dp)                   :: L    !! L2 Norm
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d1, d2, d3
    !/ -----------------------------------------------------------------------------------
    d1 = lvec%v(1) - rvec%v(1)
    d2 = lvec%v(2) - rvec%v(2)
    d3 = lvec%v(3) - rvec%v(3)
    L = ( d1*d1 + d2*d2 + d3*d3 )
  end function v3_L2_norm_external


  !/ =====================================================================================
  function v3_LN_norm_external( lvec, rvec, n ) result( L )
    !/ -----------------------------------------------------------------------------------
    !! LN Norm of this Vector3D.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Vector3D), intent(in) :: lvec !! reference to left  Vector3D.
    type(Vector3D), intent(in) :: rvec !! reference to right Vector3D.
    integer,        intent(in) :: n    !! power > 1
    real(dp)                   :: L    !! LN Norm
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d1, d2, d3
    !/ -----------------------------------------------------------------------------------
    d1 = lvec%v(1) - rvec%v(1)
    d2 = lvec%v(2) - rvec%v(2)
    d3 = lvec%v(3) - rvec%v(3)
    L = d1**n + d2**n + d3**n
  end function v3_LN_norm_external


  !/ =====================================================================================
  subroutine v3_normalize( nvec, vec, MAG )
    !/ -----------------------------------------------------------------------------------
    !! Normalize a vector.
    !/ -----------------------------------------------------------------------------------
    type(Vector3D),     intent(inout) :: nvec  !! outpout normalized vector3D.
    type(Vector3D),     intent(in)    :: vec   !! input Vector3D.
    real(dp), optional, intent(out)   :: MAG   !! output magnitude.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: m
    !/ -----------------------------------------------------------------------------------
    m = sqrt( (vec%v(1)*vec%v(1)) + (vec%v(2)*vec%v(2)) + (vec%v(3)*vec%v(3)) )
    nvec%v(1) = vec%v(1) / m
    nvec%v(2) = vec%v(2) / m
    nvec%v(3) = vec%v(3) / m
    if ( present( MAG ) ) MAG = m
  end subroutine v3_normalize








  !/ =====================================================================================
  function v3_to_string( vec, FMT, DEL, PRE, POST, TRANS ) result( str )
    !/ -----------------------------------------------------------------------------------
    !! Convert a Vector3D into a formatted string.
    !/ -----------------------------------------------------------------------------------
    use string_tools, only : toString
    implicit none
    type(Vector3D),         intent(in) :: vec   !! reference to a Vector3D.
    character(*), optional, intent(in) :: FMT   !! edit descriptor (defualt: G0).
    character(*), optional, intent(in) :: DEL   !! delimeter       (defualt: ,).
    character(*), optional, intent(in) :: PRE   !! prefix          (defualt: [).
    character(*), optional, intent(in) :: POST  !! postfix         (defualt: ]).
    logical,      optional, intent(in) :: TRANS !! postfix         (defualt: .false.).
    character(len=:), allocatable      :: str   !! string formated as a vector
    !/ -----------------------------------------------------------------------------------
    character(len=:), allocatable :: sdel, spre, spost
    !/ -----------------------------------------------------------------------------------

    sdel = ','

    if ( present( TRANS ) ) then
       if ( TRANS ) then
          sdel = '],['
       end if
    end if

    spre  = '[['
    spost = ']]'

    if ( present( PRE ) )  spre  = PRE
    if ( present( POST ) ) spost = POST

    str = spre // toString( vec%v, FMT=FMT, DEL=sdel ) // spost
  end function v3_to_string


end module vector3d_mod


!/ =======================================================================================
!/ **                              V E C T O R 3 D _ M O D                              **
!/ =========================================================================== END FILE ==
