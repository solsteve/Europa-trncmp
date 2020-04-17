!/ ====================================================================== BEGIN FILE =====
!/ **                            R E A L _ M O D E L _ M O D                            **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 1994-2020, Stephen W. Soliday                                      **
!/ **                           stephen.soliday@trncmp.org                              **
!/ **                           http://research.trncmp.org                              **
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
module real_model_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  
  !/ =====================================================================================
  type :: RealModel
     !/ ----------------------------------------------------------------------------------
     integer               :: n_par = 0 !! number of model coefficients.
     integer               :: n_met = 0 !! number of model metrics.
     real(dp), allocatable :: coef(:)   !! model coefficients.
     real(dp), allocatable :: met(:)    !! model metrics.

contains

  procedure :: nPar    => tm_get_num_parameters
  procedure :: nMet    => tm_get_num_metrics
  procedure :: build   => tm_build_model
  procedure :: execute => tm_execute_model
  procedure :: display => tm_display_model

     final :: tm_destroy
     
  end type RealModel


  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  subroutine tm_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Free model allocations.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(RealModel), intent(inout) :: dts !! reference to this RealModel.
    !/ -----------------------------------------------------------------------------------

    if ( 0.lt.dts%n_par ) then
       deallocate( dts%coef )
       deallocate( dts%met )
    end if
    
  end subroutine tm_destroy


  !/ =====================================================================================
  function tm_get_num_parameters( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of evolvable parameters.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(inout) :: dts !! reference to this RealModel.
    integer                         :: n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_par

  end function tm_get_num_parameters

  
  !/ =====================================================================================
  function tm_get_num_metrics( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of model metrics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(inout) :: dts !! reference to this RealModel.
    integer                         :: n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_met

  end function tm_get_num_metrics

  
  !/ =====================================================================================
  subroutine tm_build_model( dts, np, nm )
    !/ -----------------------------------------------------------------------------------
    !! Allocate and initialize the model coefficients.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(inout) :: dts !! reference to this RealModel.
    integer,          intent(in)    :: np  !! number of model coefficients.
    integer,          intent(in)    :: nm  !! number of model metrics.
    !/ -----------------------------------------------------------------------------------

    if ( np.ne.dts%n_par ) then
       call tm_destroy( dts )
       allocate( dts%coef( np ) )
       allocate( dts%met( nm ) )
       dts%n_par = np
       dts%n_met  = nm
    end if
    
  end subroutine tm_build_model


  !/ =====================================================================================
  subroutine tm_execute_model( dts, score, param )
    !/ -----------------------------------------------------------------------------------
    !! Calculate the sum square error between model coefficients and the supplied parameter.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(inout) :: dts      !! reference to this RealModel.
    real(dp),         intent(in)    :: param(:) !! input parameters.
    real(dp),         intent(out)   :: score(:) !! return score.
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: mse, d
    !/ -----------------------------------------------------------------------------------

    n   = dts%n_par
    mse = D_ZERO
    do i=1,n
       d = dts%coef(i) - param(i)
       mse = mse + (d*d)
    end do
    
    score(1) = mse / real(n,dp)
    
  end subroutine tm_execute_model


  !/ =====================================================================================
  subroutine tm_display_model( dts, unit, PARAM, FMT, MFMT )
    !/ -----------------------------------------------------------------------------------
    !! Display the model coefficients, or a representaion based on the optionally
    !! provided evolvable parameters.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel),       intent(inout) :: dts      !! reference to this RealModel.
    integer,      optional, intent(in)    :: UNIT     !! file unit number.
    real(dp),     optional, intent(in)    :: PARAM(:) !! evolvable parameters
    character(*), optional, intent(in)    :: FMT      !! edit descriptor
    character(*), optional, intent(in)    :: MFMT     !! edit descriptor of metric
    !/ -----------------------------------------------------------------------------------
    integer                   :: i, un
    character(:), allocatable :: sfmt, sfmtm, fmt1, fmt2
    character(32)             :: buffer
    !/ -----------------------------------------------------------------------------------

    if ( present( FMT ) ) then
       sfmt = FMT
    else
       sfmt = 'G0'
    end if

    if ( present( MFMT ) ) then
       sfmtm = MFMT
    else
       sfmtm = sfmt
    end if

100 format("(",A,",' = {',",I0,"(1X,",A,"),' }')")
200 format("('Model: {',",I0,"(1X,",A,"),' }')")

    write( buffer, 100 ) sfmtm, dts%n_par, sfmt
    fmt1 = trim(adjustl(buffer))
    
    write( buffer, 200 ) dts%n_par, sfmt
    fmt2 = trim(adjustl(buffer))

    if ( present( UNIT ) ) then
       un = UNIT
    else
       un = OUTPUT_UNIT
    end if

    if ( present( PARAM ) ) then
       call dts%execute( dts%met, PARAM )
       write( un, fmt1 ) dts%met(1), (PARAM(i),i=1,dts%n_par)
    else
       write( un, fmt2 ) (dts%coef(i),i=1,dts%n_par)
    end if
    
  end subroutine tm_display_model
    

end module real_model_mod


!/ =======================================================================================
!/ **                            R E A L _ M O D E L _ M O D                            **
!/ ======================================================================== END FILE =====
