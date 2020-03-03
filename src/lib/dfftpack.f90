!/ ====================================================================== BEGIN FILE =====
!/ **                              C O N F I G D B _ M O D                              **
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
module dfftpack
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-22
  !! license: GPL
  !!
  !! Provides a wrapper for FFTPACK form www.netlib.org/fftpack
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  !/ =====================================================================================
  interface tc_ffti
     !/ ----------------------------------------------------------------------------------

     !/ ==================================================================================
     SUBROUTINE CFFTI (N,WSAVE)
       !/ --------------------------------------------------------------------------------
       USE :: constants_env, ONLY : dp 
       INTEGER,  INTENT(IN)    :: N
       REAL(dp), INTENT(INOUT) :: WSAVE
     END SUBROUTINE CFFTI

     module procedure :: tc_cffti_wrapper

  end interface tc_ffti

  !/ =====================================================================================
  interface tc_fftf
     !/ ----------------------------------------------------------------------------------

     !/ ==================================================================================
     SUBROUTINE CFFTF (N,C,WSAVE)
       !/ --------------------------------------------------------------------------------
       USE :: constants_env, ONLY : dp 
       INTEGER,     INTENT(IN)    :: N
       COMPLEX(dp), INTENT(INOUT) :: C
       REAL(dp),    INTENT(INOUT) :: WSAVE
     END SUBROUTINE CFFTF

     module procedure :: tc_cfftf_wrapper

  end interface tc_fftf

  !/ =====================================================================================
  interface tc_fftb
     !/ ----------------------------------------------------------------------------------

     !/ ==================================================================================
     SUBROUTINE CFFTB (N,C,WSAVE)
       !/ --------------------------------------------------------------------------------
       USE :: constants_env, ONLY : dp 
       INTEGER,     INTENT(IN)    :: N
       COMPLEX(dp), INTENT(INOUT) :: C
       REAL(dp),    INTENT(INOUT) :: WSAVE
     END SUBROUTINE CFFTB

     module procedure :: tc_cfftb_wrapper

  end interface tc_fftb



  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  
  
  !/ =====================================================================================
  subroutine tc_cffti_wrapper( N, W )
    !/ -----------------------------------------------------------------------------------
    integer,     intent(in)    :: N
    real(dp),    intent(inout) :: W(*)
    call cffti( N, W(1) )
  end subroutine tc_cffti_wrapper


  !/ =====================================================================================
  subroutine tc_cfftf_wrapper( N, C, W )
    !/ -----------------------------------------------------------------------------------
    integer,     intent(in)    :: N
    complex(dp), intent(inout) :: C(:)
    real(dp),    intent(inout) :: W(*)
    call cfftf( N, C(1), W(1) )
  end subroutine tc_cfftf_wrapper


  !/ =====================================================================================
  subroutine tc_cfftb_wrapper( N, C, W )
    !/ -----------------------------------------------------------------------------------
    integer,     intent(in)    :: N
    complex(dp), intent(inout) :: C(:)
    real(dp),    intent(inout) :: W(*)
    call cfftb( N, C(1), W(1) )
  end subroutine tc_cfftb_wrapper


end module dfftpack
