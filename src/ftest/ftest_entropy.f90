!/ ====================================================================== BEGIN FILE =====
!/ **                              F T E S T _ S I M P L E                              **
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
module ftest_entropy
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-12-21
  !! license: GPL
  !!
  !!## Test of entropy 
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use dice_mod
  use statistics_mod
  use stopwatch_class
  implicit none


  integer, parameter :: N_SAMP = 100000000

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine test_R64
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(entropy_source) :: ent
    type(running_stats)  :: RS
    type(stopwatch)      :: SW
    integer              :: i
    real(dp)             :: x
    real(dp)             :: elp
    !/ -----------------------------------------------------------------------------------
    call ent%seed_set

    call RS%reset()

    call SW%reset
    do i=1,N_SAMP
       x = ent%R64()
       call RS%sample( x )
    end do
    elp = SW%check()
    write( OUTPUT_UNIT, * )
    write( OUTPUT_UNIT, * ) 'elapsed time =', elp, 'seconds'
    write( OUTPUT_UNIT, * ) 'rate =', 1.0d8 / elp, 'numbers per seconds'
    call RS%report( UNIT=OUTPUT_UNIT, FMT='F9.6' )

  end subroutine test_R64


  !/ =====================================================================================
  subroutine test_R32
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(entropy_source) :: ent
    type(running_stats)  :: RS
    type(stopwatch)      :: SW
    integer              :: i
    real(sp)             :: x
    real(dp)             :: elp
    !/ -----------------------------------------------------------------------------------
    call ent%seed_set

    call RS%reset()

    call SW%reset
    do i=1,N_SAMP
       x = ent%R32()
       call RS%sample( x )
    end do
    elp = SW%check()
    write( OUTPUT_UNIT, * )
    write( OUTPUT_UNIT, * ) 'elapsed time =', elp, 'seconds'
    write( OUTPUT_UNIT, * ) 'rate =', 1.0d8 / elp, 'numbers per seconds'
    call RS%report( UNIT=OUTPUT_UNIT, FMT='F9.6' )

  end subroutine test_R32


  !/ =====================================================================================
  subroutine test_I32
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(entropy_source) :: ent
    type(running_stats)  :: RS
    type(stopwatch)      :: SW
    integer              :: i
    integer(int32)       :: x
    real(dp)             :: elp
    !/ -----------------------------------------------------------------------------------
    call ent%seed_set

    call RS%reset()

    call SW%reset
    do i=1,N_SAMP
       x = ent%I32(10000)
       call RS%sample( x )
    end do
    elp = SW%check()
    write( OUTPUT_UNIT, * )
    write( OUTPUT_UNIT, * ) 'elapsed time =', elp, 'seconds'
    write( OUTPUT_UNIT, * ) 'rate =', 1.0d8 / elp, 'numbers per seconds'
    call RS%report( UNIT=OUTPUT_UNIT, FMT='ES13.6' )

  end subroutine test_I32


  !/ =====================================================================================
  subroutine test_I16
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(entropy_source) :: ent
    type(running_stats)  :: RS
    type(stopwatch)      :: SW
    integer              :: i
    integer(int16)       :: x
    real(dp)             :: elp
    !/ -----------------------------------------------------------------------------------
    call ent%seed_set

    call RS%reset()

    call SW%reset
    do i=1,N_SAMP
       x = ent%I16(1000_int16)
       call RS%sample( x )
    end do
    elp = SW%check()
    write( OUTPUT_UNIT, * )
    write( OUTPUT_UNIT, * ) 'elapsed time =', elp, 'seconds'
    write( OUTPUT_UNIT, * ) 'rate =', 1.0d8 / elp, 'numbers per seconds'
    call RS%report( UNIT=OUTPUT_UNIT, FMT='ES13.6' )

  end subroutine test_I16


  !/ =====================================================================================
  subroutine test_dice
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(Dice)           :: dd
    type(running_stats)  :: RS
    type(stopwatch)      :: SW
    integer              :: i, x
    real(dp)             :: elp
    !/ -----------------------------------------------------------------------------------

    call RS%reset()

    call SW%reset
    do i=1,N_SAMP
       x = dd%roll( NUM=10, SIDE=10 )
       call RS%sample( x )
    end do
    elp = SW%check()
     write( OUTPUT_UNIT, * )
    write( OUTPUT_UNIT, * ) 'elapsed time =', elp, 'seconds'
    write( OUTPUT_UNIT, * ) 'rate =', 1.0d8 / elp, 'numbers per seconds'
    call RS%report( UNIT=OUTPUT_UNIT, FMT='ES17.10' )
    print *, RS%stdev()
  end subroutine test_dice
  


end module ftest_entropy


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_entropy
  implicit none
  !/ -------------------------------------------------------------------------------------

  !call test_R64
  !call test_R32
  !call test_I32
  !call test_I16
  call test_dice

end program main

!/ =======================================================================================
!/ **                                 F T E S T _ V L A                                 **
!/ =========================================================================== END FILE ==
