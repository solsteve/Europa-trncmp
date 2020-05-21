!/ ====================================================================== BEGIN FILE =====
!/ **                           E V O _ E N T R O P Y _ M O D                           **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2011-2020, Stephen W. Soliday                                      **
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
module evo_entropy_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  integer,  parameter :: MAX_RAND = 2147483647 !! 2**31 - 1

  !/ =====================================================================================
  type :: Entropy
     !/ ----------------------------------------------------------------------------------

     integer,  private :: inext
     integer,  private :: inextp
     integer,  private :: table(55)
     
     logical,  private :: have_spare  = .false.     !! 
     real(dp), private :: rand1       = 0.0d0       !! 
     real(dp), private :: rand2       = 0.0d0       !! 

   contains

     procedure, private :: ent_seed_set_integer
     procedure, private :: ent_seed_set_system
     procedure, private :: ent_native

     generic :: seed => ent_seed_set_integer, ent_seed_set_system

     procedure :: index   => ent_index
     procedure :: uniform => ent_uniform
     procedure :: normal  => ent_normal
     procedure :: boolean => ent_boolean
     procedure :: dtest   => ent_native

  end type Entropy




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine ent_seed_set_integer( ent, s )
    !/ -----------------------------------------------------------------------------------
    !! Set the seed from a user provided value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: ent  !! reference to this Entropy object.
    integer,        intent(in)    :: s    !! seed value.
    !/ -----------------------------------------------------------------------------------
    integer :: mj, i, ii, k, mk
    !/ -----------------------------------------------------------------------------------

    mj = iabs(s)
    mj = modulo( mj, MAX_RAND )
    ent%table(55) = mj
    mk = 1
    do i=1,54
       ii = modulo(21*i , 55)
       ent%table(ii)=mk
       mk = mj - mk
       if ( mk.lt.0 ) mk = mk + MAX_RAND
       mj = ent%table(11)
    end do
    do k=1,4
       do i=1,55
          ent%table(i) = ent%table(i) - ent%table(1 + modulo(i+30,55))
          if ( ent%table(i).lt.0 ) ent%table(i) = ent%table(i) + MAX_RAND
       end do
    end do
    ent%inext  =  0
    ent%inextp = 31

  end subroutine ent_seed_set_integer


  !/ =====================================================================================
  subroutine ent_seed_set_system( ent )
    !/ -----------------------------------------------------------------------------------
    !! Set the seed from a system supplied value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: ent  !! reference to this Entropy object.
    !/ -----------------------------------------------------------------------------------
    integer :: temp(2)
    call URANDOM(temp)
    call ent%ent_seed_set_integer(temp(1))
    
  end subroutine ent_seed_set_system


  !/ =====================================================================================
  function ent_native( ent ) result( rnd3 )
    !/ -----------------------------------------------------------------------------------
    !! Return the native value from this random number generator.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: ent  !! reference to this Entropy object.
    integer                       :: rnd3 !! native output [0,MAX_RAND]
    !/ -----------------------------------------------------------------------------------
    integer :: mj
    !/ -----------------------------------------------------------------------------------
    ent%inext = ent%inext + 1
    if ( ent%inext.ge.56 ) ent%inext = 1

    ent%inextp = ent%inextp + 1
    if ( ent%inextp.ge.56 ) ent%inextp = 1

    mj = ent%table(ent%inext) - ent%table(ent%inextp)
    if ( mj.lt.0 ) mj = mj + MAX_RAND
    ent%table(ent%inext) = mj
    rnd3 = mj
  end function ent_native


  !/ =====================================================================================
  function ent_index( ent, maxi ) result( idx )
    !/ -----------------------------------------------------------------------------------
    !! Return an integer [1,maxi]
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: ent   !! reference to this Entropy object.
    integer,        intent(in)    :: maxi  !! maximum index value.
    integer                       :: idx   !! index output [1,maxi].
    !/ -----------------------------------------------------------------------------------

    idx = modulo( ent%ent_native(), maxi ) + 1

  end function ent_index


  !/ =====================================================================================
  function ent_uniform( ent ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a unifomily distributed random value [0,1)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy), intent(inout) :: ent  !! reference to this Entropy object.
    real(dp)                      :: x    !! unifomily distributed output [0,1).
    !/ -----------------------------------------------------------------------------------

    x = real(ent%ent_native(),dp) / real(MAX_RAND,dp)
    
  end function ent_uniform


  !/ =====================================================================================
  function ent_normal( ent, MEAN, STD, MINV, MAXV ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a normally distributed value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy),     intent(inout) :: ent  !! reference to this Entropy object.
    real(dp), optional, intent(in)    :: MEAN !! mean.
    real(dp), optional, intent(in)    :: STD  !! standard deviation.
    real(dp), optional, intent(in)    :: MINV !! minimum value.
    real(dp), optional, intent(in)    :: MAXV !! maximum vallue.
    real(dp)                          :: x    !! normally distributed output.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: r, mu, sig
    !/ -----------------------------------------------------------------------------------

    mu = D_ZERO
    if ( present( MEAN) ) mu = MEAN

    sig = D_ONE
    if ( present( STD) ) sig = STD

    !/ -----------------------------------------------------------------------------------
    !  Use Box-Muller algorithm to generate numbers with a normal distribution.
    !  The mean is 0.0, the standard deviation is 1.0 and limits +/- 6.15 * StdDev,
    !  based on experimental results of rolling 1 trillion values.
    !/ -----------------------------------------------------------------------------------
    if ( ent%have_spare ) then
       ent%have_spare = .false.
       r = sqrt(ent%rand1) * sin(ent%rand2)
    else
       ent%have_spare = .true.
       ent%rand1 = ent%uniform()
       if (ent%rand1.lt.1.0d-100) then
          ent%rand1 = 1.0d-100
       end if
       ent%rand1 = -2.0d0 * log(ent%rand1)
       ent%rand2 = ent%uniform()
       ent%rand2 = ent%rand2 * D_2PI
       r = sqrt(ent%rand1) * cos(ent%rand2)
    end if
    !/ -----------------------------------------------------------------------------------

    x = sig * r + mu

    if ( present( MINV ) ) then
       if ( x.lt.MINV ) x = MINV
    end if

    if ( present( MAXV ) ) then
       if ( x.gt.MAXV ) x = MAXV
    end if

  end function ent_normal


  !/ =====================================================================================
  function ent_boolean( ent, THRESHOLD ) result( truth )
    !/ -----------------------------------------------------------------------------------
    !! Return true if uniform value is less than THRESHOLD, else true.
    !! THRESHOLD defaults to 1/2.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Entropy),     intent(inout) :: ent       !! reference to this Entropy object.
    real(dp), optional, intent(in)    :: THRESHOLD !! threshold between [0,1)
    logical                           :: truth     !! truth value output true/false.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x, t
    !/ -----------------------------------------------------------------------------------

    if ( present( THRESHOLD ) ) then
       t = THRESHOLD
    else
       t = D_HALF
    end if
    
    x = ent%uniform()
    
    truth = ( x.lt.t )
    
  end function ent_boolean


end module evo_entropy_mod


!/ =======================================================================================
!/ **                           E V O _ E N T R O P Y _ M O D                           **
!/ ======================================================================== END FILE =====
