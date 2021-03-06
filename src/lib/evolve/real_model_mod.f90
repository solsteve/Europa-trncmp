!/ ====================================================================== BEGIN FILE =====
!/ **                             P S O _ M O D E L _ M O D                             **
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
module real_model_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none


  !/ =====================================================================================
  type, abstract :: RealModel
     !/ ----------------------------------------------------------------------------------

     integer  :: num_metric   =  0      !! number of metrics
     integer  :: num_variable =  0      !! number of optimizable variables
     real(dp) :: min_var      = -D_ONE  !! lower bound of variables
     real(dp) :: max_var      =  D_ONE  !! upper bound of variables

     real(dp) :: S1 = D_ONE
     real(dp) :: S2 = D_ZERO

   contains

     procedure(rm_abstract_eval),   pass(dts), deferred :: evaluate
     procedure(rm_abstract_better), nopass,    deferred :: isLeftBetter

     procedure :: super    => rm_super_init
     procedure :: nMet     => rm_metric_size
     procedure :: nPar     => rm_parameter_size
     procedure :: scale    => rm_scale_parameter
     procedure :: toString => rm_to_string

  end type RealModel


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     subroutine rm_abstract_eval( dts, metric, param )
       use trncmp_env, only : dp
       import :: RealModel
       class(RealModel), intent(inout) :: dts       !! reference to this RealModel.
       real(dp),         intent(inout) :: metric(:) !! return metric.
       real(dp),         intent(in)    :: param(:)  !! input parameters.
     end subroutine rm_abstract_eval
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function rm_abstract_better( lhs, rhs ) result ( truth )
       use trncmp_env, only : dp
       real(dp), intent(in) :: lhs(:) !! left  hand side metric.
       real(dp), intent(in) :: rhs(:) !! right hand side metric.
       logical              :: truth  !! true if lhs is better than rhs.
     end function rm_abstract_better
  end interface




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine rm_super_init( dts, MET, VAR, MINV, MAXV )
    !/ -----------------------------------------------------------------------------------
    !! Init the super instance.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel),   intent(inout) :: dts  !! reference to this RealModel.
    integer,  optional, intent(in)    :: MET  !! number of metrics.
    integer,  optional, intent(in)    :: VAR  !! number of optimizable variables.
    real(dp), optional, intent(in)    :: MINV !! lower bound of variables.
    real(dp), optional, intent(in)    :: MAXV !! upper bound of variables.
    !/ -----------------------------------------------------------------------------------
    integer :: count
    !/ -----------------------------------------------------------------------------------

    count = 0

    if ( present( MET ) ) then
       count = count + 1
       dts%num_metric = MET
    end if

    if ( present( VAR ) ) then
       count = count + 1
       dts%num_variable = VAR
    end if

    if ( 2.ne.count ) then
       call log_error( 'Both MET= and VAR= are required by RealModel%super' )
    end if

    if ( present( MINV ) ) dts%min_var = MINV
    if ( present( MAXV ) ) dts%max_var = MAXV

    dts%S1 = D_HALF*(dts%max_var - dts%min_var)
    dts%S2 = D_HALF*(dts%max_var + dts%min_var)

  end subroutine rm_super_init


  !/ =====================================================================================
  function rm_metric_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Return the dimension of the fitness metric.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(in) :: dts
    integer                      :: n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_metric

  end function rm_metric_size


  !/ =====================================================================================
  function rm_parameter_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Return the number of parameters to be optimized.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(in) :: dts
    integer                      :: n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable

  end function rm_parameter_size


  !/ =====================================================================================
  pure function rm_scale_parameter( dts, x ) result( y )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealModel), intent(in) :: dts
    real(dp), intent(in)         :: x !! unscaled input (-1,1)
    real(dp)                     :: y !! scaled output
    !/ -----------------------------------------------------------------------------------
    y = dts%S1*x + dts%S2
  end function rm_scale_parameter


  !/ =====================================================================================
  function rm_to_string( dts, metric, param, FMT, MFMT, LONG ) result ( str )
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

    n = dts%num_variable
    m = dts%num_metric

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

          write(work,p_fmt) dts%scale(param(1))
          str = str // ' = [' // trim(work)

          if ( 1.lt.n ) then
             do i=2,n
                write(work,p_fmt) dts%scale(param(i))
                str = str // ' ' // trim(work)
             end do
          end if
          str = str // ']'

       end if
    end if


  end function rm_to_string


end module real_model_mod


!/ =======================================================================================
!/ **                             P S O _ M O D E L _ M O D                             **
!/ ======================================================================== END FILE =====
