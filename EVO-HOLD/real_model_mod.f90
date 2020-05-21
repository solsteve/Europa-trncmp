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
     real(dp), allocatable :: met(:)    !! model metrics.

   contains

     procedure :: nPar     => tm_get_num_parameters
     procedure :: nMet     => tm_get_num_metrics
     procedure :: build    => tm_build_model
     procedure :: execute  => tm_execute_model
     procedure :: toString => tm_to_string
     procedure, nopass :: isLeftBetter => is_left_better

     final :: tm_destroy

  end type RealModel


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  pure function is_left_better( lhs, rhs ) result( truth )
    !/ -----------------------------------------------------------------------------------
    !! return a determination that the left metric is better than the right metric.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: lhs(:)
    real(dp), intent(in) :: rhs(:)
    logical              :: truth
    !/ -----------------------------------------------------------------------------------

    truth = .not.( lhs(1).gt.rhs(1) )  ! never use .le. or .ge. with floating-point

  end function is_left_better





  !/ =====================================================================================
  subroutine tm_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Free model allocations.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(RealModel), intent(inout) :: dts !! reference to this RealModel.
    !/ -----------------------------------------------------------------------------------

    if ( 0.lt.dts%n_par ) then
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
       d = param(i)
       mse = mse + (d*d)
    end do

    score(1) = mse

  end subroutine tm_execute_model


  !/ =====================================================================================
  function tm_to_string( dts, metric, param, FMT, MFMT, LONG ) result ( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel),       intent(in) :: dts
    real(dp),               intent(in) :: metric(:)
    real(dp),               intent(in) :: param(:)
    character(*), optional, intent(in) :: FMT
    character(*), optional, intent(in) :: MFMT
    logical,      optional, intent(in) :: LONG
    character(:), allocatable          :: str
    !/ -----------------------------------------------------------------------------------
    character(len=64)         :: work
    character(:), allocatable :: m_fmt, p_fmt
    integer                   :: i, n, m
    !/ -----------------------------------------------------------------------------------

    work = '(G0)'
    if ( present( FMT ) ) then
       write( work, "('(',A,')')" ) FMT
    end if

    p_fmt = trim(adjustl(work))

    ! work = defaults to the parameter edit descriptor
    if ( present( MFMT ) ) then
       write( work, "('(',A,')')" ) MFMT
    end if

    m_fmt = trim(adjustl(work))

    !/ -----------------------------------------------------------------------------------

    n = dts%n_par
    m = dts%n_met

    write(work,m_fmt) metric(1)
    str = '(' // trim(work)
    if ( 1.lt.m ) then
       do i=2,m
          write(work,m_fmt) metric(i)
          str = str // ' ' // trim(work)
       end do
    end if
    str = str // ')'

    if ( present( LONG ) ) then
       if ( LONG ) then

          write(work,p_fmt) param(1)
          str = str // ' = [' // trim(work)

          if ( 1.lt.n ) then
             do i=2,n
                write(work,p_fmt) param(i)
                str = str // ' ' // trim(work)
             end do
          end if
          str = str // ']'

       end if
    end if


  end function tm_to_string


end module real_model_mod


!/ =======================================================================================
!/ **                            R E A L _ M O D E L _ M O D                            **
!/ ======================================================================== END FILE =====
