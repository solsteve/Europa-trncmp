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
module pso_model_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none


  !/ =====================================================================================
  type, abstract :: PSO_Model
     !/ ----------------------------------------------------------------------------------

     integer  :: num_metric   =  0       !! number of metrics
     integer  :: num_variable =  0       !! number of optimizable variables
     real(dp) :: min_var      = -D_ONE  !! lower bound of variables
     real(dp) :: max_var      =  D_ONE   !! upper bound of variables

     real(dp) :: S1 = D_ONE
     real(dp) :: S2 = D_ZERO

   contains

     procedure(pm_abstract_eval),   pass(dts), deferred :: evaluate
     procedure(pm_abstract_better), nopass,    deferred :: isLeftBetter

     procedure :: super    => pm_super_init
     procedure :: nMet     => pm_metric_size
     procedure :: nVar     => pm_parameter_size
     procedure :: scale    => pm_scale_parameter
     procedure :: toString => pm_to_string

  end type PSO_Model


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     subroutine pm_abstract_eval( dts, metric, param )
       use trncmp_env, only : dp
       import :: PSO_Model
       class(PSO_Model), intent(inout) :: dts       !! reference to this PSO_Model.
       real(dp),         intent(inout) :: metric(:) !! return metric.
       real(dp),         intent(in)    :: param(:)  !! input parameters.
     end subroutine pm_abstract_eval
  end interface


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     function pm_abstract_better( lhs, rhs ) result ( truth )
       use trncmp_env, only : dp
       real(dp), intent(in) :: lhs(:) !! left  hand side metric.
       real(dp), intent(in) :: rhs(:) !! right hand side metric.
       logical              :: truth  !! true if lhs is better than rhs.
     end function pm_abstract_better
  end interface




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine pm_super_init( dts, MET, VAR, MINV, MAXV )
    !/ -----------------------------------------------------------------------------------
    !! Init the super instance.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO_Model),   intent(inout) :: dts  !! reference to this PSO_Model.
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
       call log_error( 'Both MET= and VAR= are required by PSO_Model%super' )
    end if

    if ( present( MINV ) ) dts%min_var = MINV
    if ( present( MAXV ) ) dts%max_var = MAXV

    dts%S1 = D_HALF*(dts%max_var - dts%min_var)
    dts%S2 = D_HALF*(dts%max_var + dts%min_var)

  end subroutine pm_super_init


  !/ =====================================================================================
  function pm_metric_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Return the dimension of the fitness metric.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO_Model), intent(in) :: dts
    integer                      :: n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_metric

  end function pm_metric_size


  !/ =====================================================================================
  function pm_parameter_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Return the number of parameters to be optimized.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO_Model), intent(in) :: dts
    integer                      :: n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_variable

  end function pm_parameter_size


  !/ =====================================================================================
   pure function pm_scale_parameter( dts, x ) result( y )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO_Model), intent(in) :: dts
    real(dp), intent(in)         :: x !! unscaled input (-1,1)
    real(dp)                     :: y !! scaled output
    !/ -----------------------------------------------------------------------------------
    y = dts%S1*x + dts%S2
  end function pm_scale_parameter
  

  !/ =====================================================================================
  function pm_to_string( dts, metric, param, MFMT, PFMT ) result ( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO_Model),       intent(in) :: dts
    real(dp),               intent(in) :: metric(:)
    real(dp),               intent(in) :: param(:)
    character(*), optional, intent(in) :: MFMT
    character(*), optional, intent(in) :: PFMT
    character(:), allocatable          :: str
    !/ -----------------------------------------------------------------------------------
    character(len=64)         :: work
    character(:), allocatable :: m_fmt, p_fmt
    integer                   :: i, n, m
    !/ -----------------------------------------------------------------------------------

    work = '(G0)'
    if ( present( MFMT ) ) then
       write( work, "('(',A,')')" ) MFMT
    end if

    m_fmt = trim(adjustl(work))

    work = '(G0)'
    if ( present( PFMT ) ) then
       write( work, "('(',A,')')" ) PFMT
    end if

    p_fmt = trim(adjustl(work))

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

    write(work,p_fmt) param(1)
    str = str // ') = [' // trim(work)

    if ( 1.lt.n ) then
       do i=2,n
          write(work,p_fmt) param(i)
          str = str // ' ' // trim(work)
       end do
    end if
    str = str // ']'

  end function pm_to_string


end module pso_model_mod


!/ =======================================================================================
!/ **                             P S O _ M O D E L _ M O D                             **
!/ ======================================================================== END FILE =====
