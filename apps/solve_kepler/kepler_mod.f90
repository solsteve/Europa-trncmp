!/ ====================================================================== BEGIN FILE =====
!/ **                                K E P L E R _ M O D                                **
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
module kepler_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides several implementations of solving Kepler's Equation
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-06-04
  !! license: GPL
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  !private

  integer,  public, parameter :: D_LOOP    = 100
  real(dp), public, parameter :: D_GMs     = 1.32712442099D20 ! m^3/s^2
  real(dp), public, parameter :: D_AU      = 1.49597870700D11 ! meters

  public ::  Kepler

  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  !/ =====================================================================================
  function Kepler( e0, angle, SRC, DST, EPS, MXI, CNT ) result( res )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                            :: res   !! Resulting angle.
    real(dp),               intent(in)  :: e0    !! Eccentricity.
    real(dp),               intent(in)  :: angle !! Source angle.
    character(1), optional, intent(in)  :: SRC   !! Source      'E', 'M', or 'V'
    character(1), optional, intent(in)  :: DST   !! Destination 'E', 'M', or 'V'
    real(dp),     optional, intent(in)  :: EPS   !! Small positive real.
    integer,      optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,      optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------
    character(1) :: source, destination
    real(dp)     :: small_value
    integer      :: max_loop, count
    !/ -----------------------------------------------------------------------------------
    
    source      = 'E'
    destination = 'M'
    small_value = 1.0D-10
    max_loop    = 20
    res         = 0.0D0

    if ( present(SRC) ) source      = SRC
    if ( present(DST) ) destination = DST
    if ( present(EPS) ) small_value = EPS
    if ( present(MXI) ) max_loop    = MXI

    if ('E'.eq.source) then
       if ( 'E'.eq.destination ) then
          res   = angle
          count = 1
       else
          if ( 'M'.eq.destination ) then
             res = kepler_mean_ecc(e0,angle,EPS=small_value,MXI=max_loop,CNT=count)
          else
             if ( 'V'.eq.destination ) then
                res = kepler_true_ecc(e0,angle,EPS=small_value,MXI=max_loop,CNT=count)
             end if
          end if
       end if
    else
       if ('M'.eq.source) then
          if ( 'E'.eq.destination ) then
             res = kepler_ecc_mean(e0,angle,EPS=small_value,MXI=max_loop,CNT=count)
          else
             if ( 'M'.eq.destination ) then
                res   = angle
                count = 1
             else
                if ( 'V'.eq.destination ) then
                   res = kepler_true_mean(e0,angle,EPS=small_value,MXI=max_loop,CNT=count)
                end if
             end if
          end if
       else
          if ('V'.eq.source) then
             if ( 'E'.eq.destination ) then
                res = kepler_ecc_true(e0,angle,EPS=small_value,MXI=max_loop,CNT=count)
             else
                if ( 'M'.eq.destination ) then
                   res = kepler_mean_true(e0,angle,EPS=small_value,MXI=max_loop,CNT=count)
                else
                   if ( 'V'.eq.destination ) then
                      res   = angle
                      count = 1
                   end if
                end if
             end if
          end if
       end if
    end if

    if ( present( CNT ) ) CNT = count
    
  end function Kepler

!$pragma GCC diagnostic push
!$pragma GCC diagnostic ignored("-Wunused-dummy-argument")
  
  !/ =====================================================================================
  function kepler_mean_ecc( e0, E, EPS, MXI, CNT ) result( M )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: M     !! Mean anom.
    real(dp),           intent(in)  :: e0    !! Eccentricity.
    real(dp),           intent(in)  :: E     !! Eccentric anomaly.
    real(dp), optional, intent(in)  :: EPS   !! Small positive real.
    integer,  optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,  optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------

    M = E - e0*sin(E)

    if ( present( CNT ) ) CNT = 1

  end function kepler_mean_ecc


  !/ =====================================================================================
  function kepler_true_ecc( e0, E, EPS, MXI, CNT ) result( V )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: V     !! True anomaly.
    real(dp),           intent(in)  :: e0    !! Eccentricity.
    real(dp),           intent(in)  :: E     !! Eccentric anomaly.
    real(dp), optional, intent(in)  :: EPS   !! Small positive real.
    integer,  optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,  optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------

    V = 0.0D0

    write(ERROR_UNIT,*) 'Kepler TRUE <== ECC not yet implemented'

    if ( present( CNT ) ) CNT = 1

  end function kepler_true_ecc


  !/ =====================================================================================
  function kepler_ecc_mean( e0, M, EPS, MXI, CNT ) result( E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: E     !! Eccentric anomaly.
    real(dp),           intent(in)  :: e0    !! Eccentricity.
    real(dp),           intent(in)  :: M     !! Mean anomaly.
    real(dp), optional, intent(in)  :: EPS   !! Small positive real.
    integer,  optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,  optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------

    E = 0.0D0

    write(ERROR_UNIT,*) 'Kepler ECC <== MEAN not yet implemented'

    if ( present( CNT ) ) CNT = 1

  end function kepler_ecc_mean


  !/ =====================================================================================
  function kepler_true_mean( e0, M, EPS, MXI, CNT ) result( V )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: V     !! True anomaly.
    real(dp),           intent(in)  :: e0    !! Eccentricity.
    real(dp),           intent(in)  :: M     !! Mean anomaly.
    real(dp), optional, intent(in)  :: EPS   !! Small positive real.
    integer,  optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,  optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------

    V = 0.0D0

    write(ERROR_UNIT,*) 'Kepler TRUE <== MEAN not yet implemented'

    if ( present( CNT ) ) CNT = 1

  end function kepler_true_mean


  !/ =====================================================================================
  function kepler_ecc_true( e0, V, EPS, MXI, CNT ) result( E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: E     !! Eccentric anomaly.
    real(dp),           intent(in)  :: e0    !! Eccentricity.
    real(dp),           intent(in)  :: V     !! True anomaly.
    real(dp), optional, intent(in)  :: EPS   !! Small positive real.
    integer,  optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,  optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------

    E = 0.0D0

    write(ERROR_UNIT,*) 'Kepler ECC <== TRUE not yet implemented'

    if ( present( CNT ) ) CNT = 1

  end function kepler_ecc_true


  !/ =====================================================================================
  function kepler_mean_true( e0, V, EPS, MXI, CNT ) result( M )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: M     !! Mean anomaly.
    real(dp),           intent(in)  :: e0    !! Eccentricity.
    real(dp),           intent(in)  :: V     !! True anomaly.
    real(dp), optional, intent(in)  :: EPS   !! Small positive real.
    integer,  optional, intent(in)  :: MXI   !! Maximum iteration Count
    integer,  optional, intent(out) :: CNT   !! Resulting iteration count.
    !/ -----------------------------------------------------------------------------------

    M = 0.0D0

    write(ERROR_UNIT,*) 'Kepler MEAN <== TRUE not yet implemented'

    if ( present( CNT ) ) CNT = 1

  end function kepler_mean_true
  
!$pragma GCC diagnostic pop
































  !/ =====================================================================================
  function Simple( ecc, M, epsilon, maxit, accit ) result( E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: E
    real(dp),           intent(in)  :: ecc
    real(dp),           intent(in)  :: M
    real(dp), optional, intent(in)  :: epsilon
    integer,  optional, intent(in)  :: maxit
    integer,  optional, intent(out) :: accit
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n, c
    real(dp) :: ep, E0
    !/ -----------------------------------------------------------------------------------

    n = D_LOOP
    if ( present(maxit) ) n = maxit

    ep = D_EPSILON
    if ( present(epsilon) ) ep = epsilon

    E  = M
    E0 = M
    c  = 0
    do i=1,n
       E = M + ecc*sin(E0)
       c = c + 1
       if ( ABS(E-E0) .LT. ep ) exit
       E0 = E
    end do

    if ( present(accit) ) accit = c

  end function Simple

  !/ =====================================================================================
  function K_Newton( ecc, M, epsilon, maxit, accit ) result( E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: E
    real(dp),           intent(in)  :: ecc
    real(dp),           intent(in)  :: M
    real(dp), optional, intent(in)  :: epsilon
    integer,  optional, intent(in)  :: maxit
    integer,  optional, intent(out) :: accit
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n, c
    real(dp) :: ep, E0
    !/ -----------------------------------------------------------------------------------

    E0 = M + ecc
    if ((M.GT.D_PI).OR.((-D_PI.LT.M).AND.(M.LT.0.0D0))) then
       E0 = M - ecc
    end if

    n = D_LOOP
    if ( present(maxit) ) n = maxit

    ep = D_EPSILON
    if ( present(epsilon) ) ep = epsilon

    E  = E0
    c  = 0
    do i=1,n
       E = E0 + ((M - E0 + ecc*sin(E0)) / (1.0D0 - ecc*cos(E0)))
       c = c + 1
       if ( ABS(E-E0) .LT. ep ) exit
       E0 = E
    end do

    if ( present(accit) ) accit = c

  end function K_Newton

  !/ =====================================================================================
  function Center3( ecc, M, epsilon, maxit, accit ) result( E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: E
    real(dp),           intent(in)  :: ecc
    real(dp),           intent(in)  :: M
    real(dp), optional, intent(in)  :: epsilon
    integer,  optional, intent(in)  :: maxit
    integer,  optional, intent(out) :: accit
    !/ -----------------------------------------------------------------------------------
    real(dp) ::  a1, a2, a3, m1, m2, m3
    !/ -----------------------------------------------------------------------------------

    m1 = sin(M)
    m2 = sin( 2.0D0 * M )
    m3 = sin( 3.0D0 * M )

    a1 = m1
    a2 = m2 / 2.0d0
    a3 = (3.0D0 * m3 - m1) / 8.0D0

    E  = M + ecc*( a1 + ecc*(a2 + ecc*a3))

    if ( present(accit) ) accit = 1

  end function Center3

  !/ =====================================================================================
  function Center6( ecc, M, epsilon, maxit, accit ) result( E )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp)                        :: E
    real(dp),           intent(in)  :: ecc
    real(dp),           intent(in)  :: M
    real(dp), optional, intent(in)  :: epsilon
    integer,  optional, intent(in)  :: maxit
    integer,  optional, intent(out) :: accit
    !/ -----------------------------------------------------------------------------------
    real(dp) :: a1, a2, a3, a4, a5, a6, m1, m2, m3, m4, m5, m6
    !/ -----------------------------------------------------------------------------------

    m1 = sin(M)
    m2 = sin( 2.0D0 * M )
    m3 = sin( 3.0D0 * M )
    m4 = sin( 4.0D0 * M )
    m5 = sin( 5.0D0 * M )
    m6 = sin( 6.0D0 * M )

    a1 = m1
    a2 = m2 / 2.0d0
    a3 = (3.0D0 * m3 - m1) / 8.0D0
    a4 = (2.0D0 * m4 - m2) / 6.0D0
    a5 = (2.0D0*m1 - 8.1D1*m3 + 1.25D2*m5) / 3.84D2
    a6 = (5.0D0*m2 - 6.4D1*m4 + 8.1D1*m6)  / 2.4D2 

    E  = M + ecc*( a1 + ecc*(a2 + ecc*(a3 + ecc*(a4 + ecc*(a5 + ecc*a6)))))

    if ( present(accit) ) accit = 1

  end function Center6

  
end module kepler_mod


!/ =======================================================================================
!/ **                                K E P L E R _ M O D                                **
!/ =========================================================================== END FILE ==
