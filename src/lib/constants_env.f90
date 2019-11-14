!/ ====================================================================== BEGIN FILE =====
!/ **                             C O N S T A N T S _ E N V                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
module constants_env
  !/ -------------------------------------------------------------------------------------
  !! author: Stephen W. Soliday
  !! date:   2015-10-23
  !! license: GPL
  !!
  !!##Mathematical Constants
  !!
  !! Provides standard constants derived from systems built-in functions.
  !!
  !!####Instead of using actual values of 
  !!   * PI 3.14159265358979323846264338327... and
  !!   * E  2.71828182845904523536028747135...
  !!   * PI is derived from the intrinsic function ACOS(-1.0D0) and
  !!   * Napier's constant E is derived from EXP(1.0D0)
  !!
  !! note: This provides better accuracy when using the built-in intrinsic
  !! trigonometry and logarithmic functions.
  !
  !/ -------------------------------------------------------------------------------------
  use iso_fortran_env
  implicit none

  public :: OUTPUT_UNIT
  public :: ERROR_UNIT

  integer, public, parameter :: qp=REAL128   !! 16 byte floating point
  integer, public, parameter :: dp=REAL64    !! 8  byte floating point
  integer, public, parameter :: sp=REAL32    !! 4  byte floating point

  real(dp), public, parameter :: D_ZERO      = 0.0d0              !! 0
  real(dp), public, parameter :: D_HALF      = 5.0d-1             !! 1/2
  real(dp), public, parameter :: D_ONE       = 1.0d0              !! 1
  real(dp), public, parameter :: D_TWO       = 2.0d0              !! 2
  real(dp), public, parameter :: D_THREE     = 3.0d0              !! 3
  real(dp), public, parameter :: D_FOUR      = 4.0d0              !! 4
  real(dp), public, parameter :: D_FIVE      = 5.0d0              !! 5
  real(dp), public, parameter :: D_SIX       = 6.0d0              !! 6
  real(dp), public, parameter :: D_SEVEN     = 7.0d0              !! 7
  real(dp), public, parameter :: D_EIGHT     = 8.0d1              !! 8
  real(dp), public, parameter :: D_NINE      = 9.0d1              !! 9
  real(dp), public, parameter :: D_TEN       = 1.0d1              !! 10
  real(dp), public, parameter :: D_E         = exp(D_ONE)         !! E (Napier)
  real(dp), public, parameter :: D_PI        = acos(-D_ONE)       !! Pi

  real(dp), public, parameter :: D_LN_2      = log(D_TWO)         !! Log_e(2)
  real(dp), public, parameter :: D_LN_10     = log(D_TEN)         !! Log_e(10)
  real(dp), public, parameter :: D_LN_PI     = log(D_PI)          !! Log_e(Pi)

  real(dp), public, parameter :: D_LOG_2     = log10(D_TWO)       !! Log_10(2)
  real(dp), public, parameter :: D_LOG_E     = log10(D_E)         !! Log_10(E)
  real(dp), public, parameter :: D_LOG_PI    = log10(D_PI)        !! Log_10(Pi)

  real(dp), public, parameter :: D_L2_10     = log(D_TEN)/D_LN_2  !! Log_2(10)
  real(dp), public, parameter :: D_L2_E      = log(D_E)/D_LN_2    !! Log_2(E)
  real(dp), public, parameter :: D_L2_PI     = log(D_PI)/D_LN_2   !! Log_2(Pi)

  real(dp), public, parameter :: D_LPI_2     = log(D_TWO)/D_LN_PI !! Log_pi(2)
  real(dp), public, parameter :: D_LPI_10    = log(D_TEN)/D_LN_PI !! Log_pi(10)
  real(dp), public, parameter :: D_LPI_E     = log(D_E)/D_LN_PI   !! Log_pi(E)

  real(dp), public, parameter :: D_2PI       = D_TWO*D_PI         !! 2*Pi
  real(dp), public, parameter :: D_3PI       = D_THREE*D_PI       !! 3*Pi
  real(dp), public, parameter :: D_4PI       = D_FOUR*D_PI        !! 4*Pi
  real(dp), public, parameter :: D_PI_2      = acos(D_ZERO)       !! Pi/2
  real(dp), public, parameter :: D_PI_4      = D_HALF*D_PI_2      !! Pi/4
  real(dp), public, parameter :: D_3PI_2     = D_THREE*D_PI_2     !! 3*Pi/2
  real(dp), public, parameter :: D_3PI_4     = D_THREE*D_PI_4     !! 3*Pi/4
  real(dp), public, parameter :: D_5PI_4     = D_FIVE*D_PI_4      !! 5*Pi/4
  real(dp), public, parameter :: D_7PI_4     = D_SEVEN*D_PI_4     !! 7*Pi/4

  real(dp), public, parameter :: D_PI2       = D_PI*D_PI          !! Pi**2
  real(dp), public, parameter :: D_1_PI      = D_ONE/D_PI         !! 1/Pi
  real(dp), public, parameter :: D_2_PI      = D_TWO/D_PI         !! 2/Pi

  real(dp), public, parameter :: D_SQRTPI    = sqrt(D_PI)         !! sqrt(Pi)
  real(dp), public, parameter :: D_SQRT2PI   = sqrt(D_2PI)        !! sqrt(2Pi)
  real(dp), public, parameter :: D_1_SQRTPI  = D_ONE/D_SQRTPI     !! 1/sqrt(Pi)
  real(dp), public, parameter :: D_1_SQRT2PI = D_ONE/D_SQRT2PI    !! 1/sqrt(2Pi)
  real(dp), public, parameter :: D_2_SQRTPI  = D_TWO/D_SQRTPI     !! 2/sqrt(Pi)

  real(dp), public, parameter :: D_SQRT_PI_2 = sqrt(D_PI/D_TWO)   !! sqrt(Pi/2)
  real(dp), public, parameter :: D_SQRT_PI_4 = sqrt(D_PI)/D_TWO   !! sqrt(Pi/4) = sqrt(Pi)/2

  real(dp), public, parameter :: D_SQRTE     = exp(D_HALF)        !! sqrt(E)
  real(dp), public, parameter :: D_1_SQRTE   = D_ONE/D_SQRTE      !! 1/sqrt(E)

  real(dp), public, parameter :: D_SQRT2     = sqrt(D_TWO)        !! sqrt(2)
  real(dp), public, parameter :: D_1_SQRT2   = D_ONE/D_SQRT2      !! 1/sqrt(2)
  real(dp), public, parameter :: D_2_SQRT2   = D_TWO/D_SQRT2      !! 2/sqrt(2)

  real(dp), public, parameter :: D_SQRT3     = sqrt(D_THREE)      !! sqrt(3)
  real(dp), public, parameter :: D_1_SQRT3   = D_ONE/D_SQRT3      !! 1/sqrt(3)
  real(dp), public, parameter :: D_2_SQRT3   = D_TWO/D_SQRT3      !! 2/sqrt(3)

  real(dp), public, parameter :: D_180_PI    = 1.8D2/D_PI         !! 180/Pi
  real(dp), public, parameter :: D_PI_180    = D_PI/1.8D2         !! Pi/180

  real(dp), public, parameter :: D_EULER     = 5.7721566490153286554942724D-01 !! Euler constant

  real(dp), public, parameter :: D_EPSILON   =  epsilon(1.0_dp) !! smallest number
  real(dp), public, parameter :: D_MAX_POS   =  huge(0.0d0)     !! Use this to init min_value
  real(dp), public, parameter :: D_MAX_NEG   = -huge(0.0d0)     !! Use this to init max_value

  real(dp), public, parameter :: RAD2DEG     = 1.8D2 / D_PI    !! Convert Radian to Degree
  real(dp), public, parameter :: DEG2RAD     = D_PI  / 1.8d2   !! Convert Degree to Radian

  integer(int8),  public, parameter :: MAX_I8  = huge( 1_int8  )  !! Largest 8 bit integer
  integer(int16), public, parameter :: MAX_I16 = huge( 1_int16 )  !! Largest 16 bit integer
  integer(int32), public, parameter :: MAX_I32 = huge( 1_int32 )  !! Largest 32 bit integer
  integer(int64), public, parameter :: MAX_I64 = huge( 1_int64 )  !! Largest 64 bit integer
  real(sp),       public, parameter :: MAX_R32 = huge( 1.0_sp  )  !! Largest 32 bit real
  real(dp),       public, parameter :: MAX_R64 = huge( 1.0_dp  )  !! Largest 64 bit real

  integer(int8),  public, parameter :: MIN_I8  = 0_int8
  integer(int16), public, parameter :: MIN_I16 = 0_int16
  integer(int32), public, parameter :: MIN_I32 = 0_int32
  integer(int64), public, parameter :: MIN_I64 = 0_int64
  real(sp),       public, parameter :: MIN_R32 = tiny( 1.0_sp  )
  real(dp),       public, parameter :: MIN_R64 = tiny( 1.0_dp  )

end module constants_env

!/ =======================================================================================
!/ **                             C O N S T A N T S _ E N V                             **
!/ =========================================================================== END FILE ==
