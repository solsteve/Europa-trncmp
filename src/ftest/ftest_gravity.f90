!/ ====================================================================== BEGIN FILE =====
!/ **                             F T E S T _ G R A V I T Y                             **
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
module ftest_gravity
  use trncmp_env
  use runge_kutta_mod
  use stopwatch_class

  real(dp), parameter :: M1  = 1.988435d+30       !! Mass of the SUN
  real(dp), parameter :: M2  = 1.899d+27          !! Mass of the JUPITER
  real(dp), parameter :: M3  = 5.9742d+24         !! Mass of the EARTH
  real(dp), parameter :: M4  = 7.347673d+22       !! Mass of the MOON

  real(dp), parameter :: R1  = 695000000.0d0      !! Average Radius of the SUN
  real(dp), parameter :: R2  = 69924754.6d0       !! Average Radius of the JUPITER
  real(dp), parameter :: R3  = 6370996.16d0       !! Average Radius of the EARTH
  real(dp), parameter :: R4  = 1737146.5d0        !! Average Radius of the MOON

  real(dp), parameter :: X1  = -7.0675082353d+8   !! Initial Y Pos of the SUN
  real(dp), parameter :: Y1  = 0.0d0              !! Initial Y Pos of the SUN
  real(dp), parameter :: X2  = 7.40035847176d+11  !! Initial X Pos of the JUPITER
  real(dp), parameter :: Y2  = 0.0d0              !! Initial Y Pos of the JUPITER
  real(dp), parameter :: X3  = 1.47102485561d+11  !! Initial X Pos of the EARTH
  real(dp), parameter :: Y3  = 0.0d0              !! Initial Y Pos of the EARTH
  real(dp), parameter :: X4  = 1.46739381561d+11  !! Initial X Pos of the MOON
  real(dp), parameter :: Y4  = 0.0d0              !! Initial Y Pos of the MOON

  real(dp), parameter :: VX1 = 0.0d0              !! Init X Velocity of the SUN
  real(dp), parameter :: VY1 = -11.861d0          !! Init Y Velocity of the SUN
  real(dp), parameter :: VX2 = 0.0d0              !! Init X Velocity of the JUPITER
  real(dp), parameter :: VY2 = 13712.0d0          !! Init Y Velocity of the JUPITER
  real(dp), parameter :: VX3 = 0.0d0              !! Init X Velocity of the EARTH
  real(dp), parameter :: VY3 = 30287.0d0          !! Init Y Velocity of the EARTH
  real(dp), parameter :: VX4 = 0.0d0              !! Init X Velocity of the MOON
  real(dp), parameter :: VY4 = 29205.0d0          !! Init Y Velocity of the MOON

  real(dp), parameter :: MAXT      = 1200.0d0     !! Time in days of plot
  real(dp), parameter :: DELTATIME = 21600.0d0    !! Delta Time (seconds) between plots
  integer,  parameter :: ISTEP     = 360          !! Number of Steps per Plot
  real(dp), parameter :: GRAV      = 6.6742d-11   !! Newtons Gravitational constant (m^3/(kg*s^2)

  integer,  parameter :: iVX1 =  1
  integer,  parameter :: iVY1 =  2
  integer,  parameter ::  iX1 =  3
  integer,  parameter ::  iY1 =  4
  integer,  parameter :: iVX2 =  5
  integer,  parameter :: iVY2 =  6
  integer,  parameter ::  iX2 =  7
  integer,  parameter ::  iY2 =  8
  integer,  parameter :: iVX3 =  9
  integer,  parameter :: iVY3 = 10
  integer,  parameter ::  iX3 = 11
  integer,  parameter ::  iY3 = 12
  integer,  parameter :: iVX4 = 13
  integer,  parameter :: iVY4 = 14
  integer,  parameter ::  iX4 = 15
  integer,  parameter ::  iY4 = 16

  integer,  parameter ::   jG = 1
  integer,  parameter ::  jM1 = 2
  integer,  parameter ::  jR1 = 3
  integer,  parameter ::  jM2 = 4
  integer,  parameter ::  jR2 = 5
  integer,  parameter ::  jM3 = 6
  integer,  parameter ::  jR3 = 7
  integer,  parameter ::  jM4 = 8
  integer,  parameter ::  jR4 = 9

  integer,  parameter ::   NN = 4

  real(dp), parameter :: AccInc = DELTATIME / real(ISTEP,dp)
  real(dp), parameter :: Acc    = AccInc * 9.81d0 * 7.0d0

  real(dp), parameter :: ACTION_TIME = 21912800.0d0

  !/ =====================================================================================
  type, extends(RK4) :: FourBody
     !/ ----------------------------------------------------------------------------------
     !! Simulates a four body gravitational system.
     !/ ----------------------------------------------------------------------------------

     integer  :: iCount = 0
     real(dp) :: tCount = 0.0d0

     integer  :: finished = 0

     real(dp) :: dx(NN,NN)
     real(dp) :: dy(NN,NN)

     real(dp) :: den(NN,NN)

     real(dp) :: x(NN), xdd(NN)
     real(dp) :: y(NN), ydd(NN)

     real(dp) :: m(NN)


   contains

     procedure, pass(dts) :: DIFFEQ => four_diffeq
     procedure, pass(dts) :: CHECK  => four_check

  end type FourBody




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine four_diffeq( dts, Qd, Q, t, P )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FourBody),    intent(inout) :: dts   !! reference to this integrator.
    real(dp),           intent(out)   :: Qd(:) !! partial derivatives of the state vector.
    real(dp),           intent(in)    :: Q(:)  !! state vector.
    real(dp),           intent(in)    :: t     !! time.
    real(dp), optional, intent(in)    :: P(:)  !! fixed parameters.
    !/ -----------------------------------------------------------------------------------
    integer :: i, k
    real(dp) :: G, tt, sumx, sumy, tk

    !/ -----------------------------------------------------------------------------------
    if (0.eq.dts%finished ) then
       if ( present( P ) ) then
          !/ -------------------------------------------------------------------
          G    = P(jG)

          dts%x(1) = Q(iX1)
          dts%x(2) = Q(iX2)
          dts%x(3) = Q(iX3)
          dts%x(4) = Q(iX4)

          dts%y(1) = Q(iY1)
          dts%y(2) = Q(iY2)
          dts%y(3) = Q(iY3)
          dts%y(4) = Q(iY4)

          dts%m(1) = P(jM1)
          dts%m(2) = P(jM2)
          dts%m(3) = P(jM3)
          dts%m(4) = P(jM4)

          !/ -------------------------------------------------------------------

          do k=2,NN
             do i=1,k-1
                dts%dx(k,i) = dts%x(k) - dts%x(i)
                dts%dy(k,i) = dts%y(k) - dts%y(i)
                dts%dx(i,k) = -dts%dx(k,i)
                dts%dy(i,k) = -dts%dy(k,i)
             end do
          end do

          !/ -------------------------------------------------------------------

          do k=2,NN
             do i=1,k-1
                tt = ( dts%dx(k,i) * dts%dx(k,i) ) + ( dts%dy(k,i) * dts%dy(k,i) )
                dts%den(k,i) = tt**(-1.5d0)
                dts%den(i,k) = dts%den(k,i)
             end do
          end do

          !/ -------------------------------------------------------------------

          do i=1,NN
             sumx = D_ZERO
             sumy = D_ZERO
             do k=1,NN
                if ( k.ne.i ) then
                   tk = dts%m(k) * dts%den(k,i)
                   sumx = sumx + ( tk * dts%dx(k,i) )
                   sumy = sumy + ( tk * dts%dy(k,i) )
                end if
             end do
             dts%xdd(i) = G * sumx;
             dts%ydd(i) = G * sumy;
          end do

          !/ -------------------------------------------------------------------

          Qd(iVX1) = dts%xdd(1)
          Qd(iVX2) = dts%xdd(2)
          Qd(iVX3) = dts%xdd(3)
          Qd(iVX4) = dts%xdd(4)

          Qd(iVY1) = dts%ydd(1)
          Qd(iVY2) = dts%ydd(2)
          Qd(iVY3) = dts%ydd(3)
          Qd(iVY4) = dts%ydd(4)

          Qd(iX1)  = Q(iVX1)
          Qd(iX2)  = Q(iVX2)
          Qd(iX3)  = Q(iVX3)
          Qd(iX4)  = Q(iVX4)

          Qd(iY1)  = Q(iVY1)
          Qd(iY2)  = Q(iVY2)
          Qd(iY3)  = Q(iVY3)
          Qd(iY4)  = Q(iVY4)

          !/ -------------------------------------------------------------------
       end if
    end if
  end subroutine four_diffeq



  !/ =====================================================================================
  function four_check( dts, Q, t, P ) result ( tcond )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FourBody),    intent(inout) :: dts   !! reference to this integrator.
    real(dp),           intent(inout) :: Q(:)  !! state vector.
    real(dp),           intent(in) :: t     !! time.
    real(dp), optional, intent(in) :: P(:)  !! fixed parameters.
    integer                        :: tcond !! termination condition.  
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x1, y1, rad1
    real(dp) :: x2, y2, rad2
    real(dp) :: x3, y3, rad3
    real(dp) :: x4, y4, rad4
    real(dp) :: r12, dx12, dy12
    real(dp) :: r13, dx13, dy13
    real(dp) :: r14, dx14, dy14
    real(dp) :: r23, dx23, dy23
    real(dp) :: r24, dx24, dy24
    real(dp) :: r34, dx34, dy34
    real(dp) :: v2, cc
    !/ -----------------------------------------------------------------------------------
    x1 = Q(iX1)
    x2 = Q(iX2)
    x3 = Q(iX3)
    x4 = Q(iX4)

    y1 = Q(iY1)
    y2 = Q(iY2)
    y3 = Q(iY3)
    y4 = Q(iY4)

    rad1 = P(jR1)
    rad2 = P(jR2)
    rad3 = P(jR3)
    rad4 = P(jR4)

    r12 = rad1 + rad2
    r13 = rad1 + rad3
    r14 = rad1 + rad4
    r23 = rad2 + rad3
    r24 = rad2 + rad4
    r34 = rad3 + rad4

    dx12 = x1 - x2
    dx13 = x1 - x3
    dx14 = x1 - x4
    dx23 = x2 - x3
    dx24 = x2 - x4
    dx34 = x3 - x4

    dy12 = y1 - y2
    dy13 = y1 - y3
    dy14 = y1 - y4
    dy23 = y2 - y3
    dy24 = y2 - y4
    dy34 = y3 - y4

    if ( t .gt. ACTION_TIME ) then
      v2 = ( Q(iVX4) * Q(iVX4) ) + ( Q(iVY4) * Q(iVY4) )
       if ( v2 < 1.0d9 ) then
           cc = sqrt(v2)
          !write(ERROR_UNIT,1000) t, Q(iX4), Q(iY4), Q(iVX4), Q(iVY4), Acc, cc
          Q(iVX4) = Q(iVX4) + Acc*Q(iVX4)/cc 
          Q(iVY4) = Q(iVY4) + Acc*Q(iVY4)/cc 
          dts%icount = dts%icount + 1
          dts%tcount = dts%tcount + AccInc
    end if
   end if

    tcond = 0
    if ( (r12 * r12).gt.((dx12 * dx12) + (dy12 * dy12)) ) then
       tcond = 1
       goto 999
    end if
    if ( (r13 * r13).gt.((dx13 * dx13) + (dy13 * dy13)) ) then
       tcond = 2
       goto 999
    end if
    if ( (r14 * r14).gt.((dx14 * dx14) + (dy14 * dy14)) ) then
       tcond = 3
       goto 999
    end if
    if ( (r23 * r23).gt.((dx23 * dx23) + (dy23 * dy23)) ) then
       tcond = 4
       goto 999
    end if
    if ( (r24 * r24).gt.((dx24 * dx24) + (dy24 * dy24)) ) then
       tcond = 5
       goto 999
    end if
    if ( (r34 * r34).gt.((dx34 * dx34) + (dy34 * dy34)) ) then
       tcond = 6
       goto 999
    end if

999 continue

    dts%finished = tcond

!1000 format( F11.1,3(' : ', ES13.6,1X,ES13.6))

        end function four_check


end module ftest_gravity


!/ =======================================================================================
program main
  use ftest_gravity
  use psgraph_mod
  implicit none
  !/ -----------------------------------------------------------------------------------
  real(dp), parameter :: Xc  = 3.0d11
  real(dp), parameter :: Yc  = 3.5d11
  real(dp), parameter :: win = 6.0d11
  !/ -----------------------------------------------------------------------------------
  real(dp) :: param(16)
  real(dp) :: state(32)
  real(dp) :: t, maxTime, px1, px2, px3, px4, py1, py2, py3, py4, kk
  class(PSGraph), pointer :: ps
  class(PSDraw),  pointer :: pd
  type(FourBody)  :: model
  real(dp)        :: elapsed
  type(stopwatch) :: SW
  !/ -----------------------------------------------------------------------------------

  t = D_ZERO
  maxTime = MAXT * 86400.d0

  param(jG)  = GRAV

  param(jM1) = M1
  param(jM2) = M2
  param(jM3) = M3
  param(jM4) = M4

  param(jR1) = R1
  param(jR2) = R2
  param(jR3) = R3
  param(jR4) = R4

  state(iVX1) = VX1
  state(iVX2) = VX2
  state(iVX3) = VX3
  state(iVX4) = VX4

  state(iVY1) = VY1
  state(iVY2) = VY2
  state(iVY3) = VY3
  state(iVY4) = VY4

  state(iX1) = X1
  state(iX2) = X2
  state(iX3) = X3
  state(iX4) = X4

  state(iY1) = Y1
  state(iY2) = Y2
  state(iY3) = Y3
  state(iY4) = Y4

  px1 = X1
  px2 = X2
  px3 = X3
  px4 = X4

  py1 = Y1
  py2 = Y2
  py3 = Y3
  py4 = Y4

  pd => PSDraw(7.0d0, 7.0d0, Xc-win, Yc-win, Xc+win, Yc+win )
  ps => PSGraph(1)

  call pd%drawBorder

  call model%init(NN*4)

  call SW%reset
  
  100 continue

  t = model%integrate( state, t, t+DELTATIME, ISTEP, P=param )

!  call pd%drawline( px1, py1, state(iX1), state(iY1) )
!  call pd%drawline( px2, py2, state(iX2), state(iY2) )
!  call pd%drawline( px3, py3, state(iX3), state(iY3) )
!  call pd%drawline( px4, py4, state(iX4), state(iY4) )

  px1 = state(iX1)
  px2 = state(iX2)
  px3 = state(iX3)
  px4 = state(iX4)

  py1 = state(iY1)
  py2 = state(iY2)
  py3 = state(iY3)
  py4 = state(iY4)

  if ( t.lt.maxTime) then
     if (model%finished.eq.0) goto 100
  end if

  elapsed = SW%check()

  call pd%drawCircle( state(iX1), state(iY1), param(jR1) )
  call pd%drawCircle( state(iX2), state(iY2), param(jR2) )
  call pd%drawCircle( state(iX3), state(iY3), param(jR3) )
  call pd%drawCircle( state(iX4), state(iY4), param(jR4) )

  call ps%add( pd, 1, 0.5d0, 0.5d0 )

  call ps%pswrite( 'gravity.ps' )

  kk = sqrt( (state(iVX4) * state(iVX4)) + (state(iVY4) * state(iVY4)) )

  write( *, 1100 ) t
  write( *, 1200 ) state(iX1), state(iY1), state(iVX1), state(iVY1)
  write( *, 1300 ) state(iX2), state(iY2), state(iVX2), state(iVY2)
  write( *, 1400 ) state(iX3), state(iY3), state(iVX3), state(iVY3)
  write( *, 1500 ) state(iX4), state(iY4), state(iVX4), state(iVY4), kk
  write( *, 1600 ) model%iCount
  write( *, 1700 ) model%tCount
  write( *, 1800 ) elapsed


1100 format('Time    ', F11.1)
1200 format('Sun     ', 4(1X,ES13.6))
1300 format('Jupiter ', 4(1X,ES13.6))
1400 format('Earth   ', 4(1X,ES13.6))
1500 format('Moon    ', 5(1X,ES13.6))
1600 format('Count   ', I0 )
1700 format('Burn    ', F6.2,' seconds')
1800 format('Elapsed ', F9.6,' seconds')

end program main


!/ =======================================================================================
!/ **                             F T E S T _ G R A V I T Y                             **
!/ =========================================================================== END FILE ==
