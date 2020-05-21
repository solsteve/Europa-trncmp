!/ ====================================================================== BEGIN FILE =====
!/ **                               D R O P O F F _ M O D                               **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2016-2020, Stephen W. Soliday                                      **
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
module dropoff_mod
  !/ -------------------------------------------------------------------------------------
  !! brief  Drop Off Generator.
  !! file   DropOff.hh
  !! author Stephen W. Soliday
  !! date   2016-Aug-09 Original release.
  !! date   2019-Jun-27 CMake refactorization.
  !!
  !! Provides the interface for a drop off generator.
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  
  integer, private, parameter :: DO_LINEAR      = 1
  integer, private, parameter :: DO_EXPONENTIAL = 2
  integer, private, parameter :: DO_GAUSSIAN    = 3


  !/ =====================================================================================
  type :: DropOff
     !/ ----------------------------------------------------------------------------------
     integer  :: index  = 0
     real(dp) :: A      = D_ZERO
     real(dp) :: B      = D_ZERO
     integer  :: d_type = 0

   contains

     procedure :: build => do_build
     procedure :: next  => do_next
     procedure :: get   => do_get
     procedure :: reset => do_reset
  end type DropOff




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine do_build( dts, Vo, Vf, n, dt )
    !/ -----------------------------------------------------------------------------------
    !! Set up a drop off function such that V=Vo @ 0 and V=Vf @ n-1.
    !/ -----------------------------------------------------------------------------------
    class(DropOff), intent(inout) :: dts !! reference to this DropOff object.
    real(dp),       intent(in)    :: Vo  !! starting value.
    real(dp),       intent(in)    :: Vf  !! final value.
    integer,        intent(in)    :: n   !! number of samples.
    character(1),   intent(in)    :: dt  !! drop off type (default linear).
    !/ -----------------------------------------------------------------------------------

    dts%index = 0
    
    if ( ( 'L'.eq.dt ).or.( 'l'.eq.dt ) ) then

       dts%A      = (Vf - Vo) / real(n-1, dp)
       dts%B      = Vo
       dts%d_type = DO_LINEAR
       
    else if ( ( 'E'.eq.dt ).or.( 'e'.eq.dt ) ) then

       dts%A      = Vo
       dts%B      = ( log(Vf) - log(Vo) ) / real(n-1, dp)
       dts%d_type = DO_EXPONENTIAL
       
    else if ( ( 'G'.eq.dt ).or.( 'g'.eq.dt ) ) then

       dts%A      = Vo
       dts%B      = ( log(Vf) - log(Vo) ) / real((n-1)*(n-1), dp)
       dts%d_type = DO_GAUSSIAN
       
    else
       
       write( ERROR_UNIT, 100 )Vo, Vf, dt
       
    end if

100 format( 'DropOff%build(',G0,',',G0,',',A,') - failed' )

  end subroutine do_build


  !/ =====================================================================================
  function do_next( dts ) result( rv )
    !/ -----------------------------------------------------------------------------------
    !! Get the current value and increment the index.
    !/ -----------------------------------------------------------------------------------
    class(DropOff), intent(inout) :: dts !! reference to this DropOff object.
    real(dp)                      :: rv  !! current value.
    !/ -----------------------------------------------------------------------------------

    rv = D_ZERO
    
    if ( DO_LINEAR.eq.dts%d_type ) then
       
       rv = dts%B + dts%A * real(dts%index, dp)
       dts%index = dts%index + 1
       
    else if ( DO_EXPONENTIAL.eq.dts%d_type ) then

       rv =  dts%A * exp( dts%B * real(dts%index, dp)  )
       dts%index = dts%index + 1
       
    else if ( DO_GAUSSIAN.eq.dts%d_type ) then

       rv = dts%A * exp( real(dts%index*dts%index, dp) * dts%B )
       dts%index = dts%index + 1
       
    else
       
       write( ERROR_UNIT, 100 ) dts%index, dts%d_type
       
    end if

100 format('DropOff(',I0,',',I0,') - failed')
    
  end function do_next


  !/ =====================================================================================
  function do_get( dts, n ) result( rv )
    !/ -----------------------------------------------------------------------------------
    !! Set the index, return that value and then increment.
    !! Example:  get(5)  returns V(5) and then increments index to 6.
    !/ -----------------------------------------------------------------------------------
    class(DropOff), intent(inout) :: dts !! reference to this DropOff object.
    integer,        intent(in)    :: n   !! new index.
    real(dp)                      :: rv  !! current value.
    !/ -----------------------------------------------------------------------------------

    dts%index = n-1
    rv = dts%next()

  end function do_get


  !/ =====================================================================================
  subroutine do_reset( dts )
    !/ -----------------------------------------------------------------------------------
    !! Reset the DropOff to the initial value. Set the index to 1.
    !/ -----------------------------------------------------------------------------------
    class(DropOff), intent(inout) :: dts !! reference to this DropOff object.
    !/ -----------------------------------------------------------------------------------

    dts%index = 1

  end subroutine do_reset


end module dropoff_mod


!/ =======================================================================================
!/ **                               D R O P O F F _ M O D                               **
!/ ======================================================================== END FILE =====
