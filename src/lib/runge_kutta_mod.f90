!/ ====================================================================== BEGIN FILE =====
!/ **                           R U N G E _ K U T T A _ M O D                           **
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
module runge_kutta_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides an interface for Runge-Kutta 4th order numerical integration.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-FEB-27
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use string_tools
  implicit none


  !/ =====================================================================================
  type, abstract :: RK4
     !/ ----------------------------------------------------------------------------------
     !! This class defines an abstract type for a Runge-Kutta 4th order numerical integrator
     !/ ----------------------------------------------------------------------------------

     integer               :: dim  = 0 !! number of coupled first order equations.
!     real(dp), allocatable :: A(:)     !! stage one   state vector.
!     real(dp), allocatable :: B(:)     !! stage two   state vector.
!     real(dp), allocatable :: C(:)     !! stage three state vector.
!     real(dp), allocatable :: D(:)     !! stage four  state vector.
!     real(dp), allocatable :: W(:)     !! DIFEQ input vector.
     real(dp), pointer, dimension(:) :: A => null() !< stage one   state vector
     real(dp), pointer, dimension(:) :: B => null() !< stage two   state vector
     real(dp), pointer, dimension(:) :: C => null() !< stage three state vector
     real(dp), pointer, dimension(:) :: D => null() !< stage four  state vector
     real(dp), pointer, dimension(:) :: W => null() !< DIFEQ input vector

   contains

     procedure(rk4_abstract_diffeq), pass(dts), deferred :: DIFFEQ
     procedure(rk4_abstract_check),  pass(dts), deferred :: CHECK

     procedure :: init      => rk4_init
     procedure :: integrate => rk4_integration

     !final :: rk4_destroy

  end type RK4


  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     !! User supplied coupled partial diferential equations.
     !/ ----------------------------------------------------------------------------------
     subroutine rk4_abstract_diffeq( dts, Qd, Q, t, P )
       use trncmp_env, only : dp
       import :: RK4
       class(RK4),         intent(inout) :: dts   !! reference to this integrator.
       real(dp),           intent(out)   :: Qd(:) !! partial derivatives of the state vector.
       real(dp),           intent(in)    :: Q(:)  !! state vector.
       real(dp),           intent(in)    :: t     !! time.
       real(dp), optional, intent(in)    :: P(:)  !! fixed parameters.
     end subroutine rk4_abstract_diffeq
  end interface

  !/ =====================================================================================
  abstract interface
     !/ ----------------------------------------------------------------------------------
     !! User supplied check function. Return value greather than zero to trigger
     !! early termination. Return a zero if everything is okay.
     !/ ----------------------------------------------------------------------------------
     function rk4_abstract_check( dts, Q, t, P ) result ( tcond )
       use trncmp_env, only : dp
       import :: RK4
       class(RK4),         intent(inout) :: dts   !! reference to this integrator.
       real(dp),           intent(inout) :: Q(:)  !! state vector.
       real(dp),           intent(in) :: t     !! time.
       real(dp), optional, intent(in) :: P(:)  !! fixed parameters.
       integer                        :: tcond !! termination condition.  
     end function rk4_abstract_check
  end interface


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine rk4_init( dts, n )
    ! ------------------------------------------------------------------------------------
    !! Intitialize this integrator to a number of state variables.
    !! This procedure may be used to change the number of states.
    ! ------------------------------------------------------------------------------------
    implicit none
    class(RK4), intent(inout) :: dts !! reference to this integrator.
    integer,    intent(in)    :: n   !! number of state variables.
    ! ------------------------------------------------------------------------------------

    dts%dim = n
    
    if ( associated( dts%A ) ) deallocate( dts%A )
    if ( associated( dts%B ) ) deallocate( dts%B )
    if ( associated( dts%C ) ) deallocate( dts%C )
    if ( associated( dts%D ) ) deallocate( dts%D )
    if ( associated( dts%W ) ) deallocate( dts%W )

!    if ( allocated( dts%A ) ) deallocate( dts%A )
!    if ( allocated( dts%B ) ) deallocate( dts%B )
!    if ( allocated( dts%C ) ) deallocate( dts%C )
!    if ( allocated( dts%D ) ) deallocate( dts%D )
!    if ( allocated( dts%W ) ) deallocate( dts%W )

    allocate( dts%A(n) )
    allocate( dts%B(n) )
    allocate( dts%C(n) )
    allocate( dts%D(n) )
    allocate( dts%W(n) )

  end subroutine rk4_init


  !  !/ =====================================================================================
  !  subroutine rk4_destroy( rk )
  !    ! ------------------------------------------------------------------------------------
  !    !! Finalization. Deallocate memory.
  !    ! ------------------------------------------------------------------------------------
  !    implicit none
  !    type(RK4), intent(inout) :: rk   !! reference to an integrator.
  !    ! ------------------------------------------------------------------------------------
  !    rk%dim = 0
!    if ( associated( self%A ) ) deallocate( self%A )
!    if ( associated( self%B ) ) deallocate( self%B )
!    if ( associated( self%C ) ) deallocate( self%C )
!    if ( associated( self%D ) ) deallocate( self%D )
!    if ( associated( self%W ) ) deallocate( self%W )
!    nullify( self%A )
!    nullify( self%B )
!    nullify( self%C )
!    nullify( self%D )
!    nullify( self%W )
  !    if ( allocated( rk%A ) ) deallocate( rk%A )
  !    if ( allocated( rk%B ) ) deallocate( rk%B )
  !    if ( allocated( rk%C ) ) deallocate( rk%C )
  !    if ( allocated( rk%D ) ) deallocate( rk%D )
  !    if ( allocated( rk%W ) ) deallocate( rk%W )
  !  end subroutine rk4_destroy



  !/ =====================================================================================
  function rk4_integration( dts, Q, t0, t1, steps, P, TCOND ) result( t )
    ! ------------------------------------------------------------------------------------
    !! Performs integration steps using a Runge-Kutta fourth order numerical integrartor
    !! with fixed step sizes.
    ! ------------------------------------------------------------------------------------
    implicit none
    class(RK4),         intent(inout) :: dts   !! reference to this integrator.
    real(dp),           intent(inout) :: Q(:)  !! vector containing the state.
    real(dp),           intent(in)    :: t0    !! initial time.
    real(dp),           intent(in)    :: t1    !! final time.
    integer,            intent(in)    :: steps !! number of steps between current time
    !!                                            \a t0 and final time \a t1.
    real(dp), optional, intent(inout) :: P(:)  !! vector containing fixed parameters.
    integer,  optional, intent(out)   :: TCOND !! early termination condition (TCOND.gt.0)
    real(dp)                          :: t     !! time after final integration
    ! ------------------------------------------------------------------------------------
    real(dp) :: h, h2
    integer :: n, j, k, r
    ! ------------------------------------------------------------------------------------
    h  = ( t1 - t0 ) / real(steps,dp)
    h2 = D_HALF * h
    n  = dts%dim

    t  = t0
    r  = 0

    do k =1,steps
        r = dts%CHECK( Q, t, P )
       if ( r.gt.0 ) goto 999

       !/ ----- stage 1 --------------------
       do concurrent( j=1:n )
          dts%W(j) = Q(j)
       end do
       call dts%DIFFEQ( dts%A, dts%W, t, P )

       !/ ----- stage 2 --------------------
       do concurrent( j=1:n )
          dts%W(j) = Q(j) + ( dts%A(j)*h2 )
       end do
       call dts%DIFFEQ( dts%B, dts%W, t+h2, P )

       !/ ----- stage 3 --------------------
       do concurrent( j=1:n )
          dts%W(j) = Q(j) + ( dts%B(j)*h2 )
       end do
       call dts%DIFFEQ( dts%C, dts%W, t+h2, P )

       !/ ----- stage 4 --------------------
       do concurrent( j=1:n )
          dts%W(j) = Q(j) + ( dts%C(j)*h )
       end do
       call dts%DIFFEQ( dts%D, dts%W, t+h, P )

       !/ ----- average --------------------
       do concurrent( j=1:n )
          Q(j) = Q(j) + ( h * (dts%A(j) + D_TWO*( dts%B(j) + dts%C(j) ) + dts%D(j)) / D_SIX )
       end do

       t = t + h
    end do

999 continue

    if ( present( TCOND ) ) TCOND = r

  end function rk4_integration


end module runge_kutta_mod


!/ =======================================================================================
!/ **                           R U N G E _ K U T T A _ M O D                           **
!/ ======================================================================== END FILE =====
