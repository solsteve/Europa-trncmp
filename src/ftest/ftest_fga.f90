!/ ====================================================================== BEGIN FILE =====
!/ **                                 F T E S T _ F G A                                 **
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
module ftest_fga
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  integer, parameter :: POP_SIZE = 256
  integer, parameter :: MAX_GEN  = 10
  integer, parameter :: NUM_VAR  = 10



  integer,  parameter :: MBIG  = 2147483647
  integer,  parameter :: MSEED = 161803398
  integer,  parameter :: MZ    = 0
  real(dp), parameter :: FAC   = 1.0D-9
  
  type :: Knuth

     integer :: inext
     integer :: inextp
     integer :: ma(55)

   contains

     procedure :: seed => k_seed
     procedure :: get  => k_get_next
     
  end type Knuth
  


  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  subroutine k_seed( dts, idum )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Knuth), intent(inout) :: dts
    integer,      intent(in)    :: idum
    !/ -----------------------------------------------------------------------------------
    integer :: mj, i, ii, k, mk
    !/ -----------------------------------------------------------------------------------

    mj = MSEED - idum
    mj = modulo( mj, MBIG )
    dts%ma(55) = mj
    mk = 1
    do i=1,54
       ii = modulo(21*i , 55)
       dts%ma(ii)=mk
       mk = mj - mk
       if ( mk.lt.MZ ) mk = mk + MBIG
       mj = dts%ma(11)
    end do
    do k=1,4
       do i=1,55
          dts%ma(i) = dts%ma(i) - dts%ma(1 + modulo(i+30,55))
          if ( dts%ma(i).lt.MZ ) dts%ma(i) = dts%ma(i) + MBIG
       end do
    end do
    dts%inext  =  0
    dts%inextp = 31
  end subroutine k_seed

  
  !/ =====================================================================================
  function k_get_next( dts ) result( rnd3 )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Knuth), intent(inout) :: dts
    integer                     :: rnd3
    !/ -----------------------------------------------------------------------------------
    integer :: mj
    !/ -----------------------------------------------------------------------------------
    dts%inext = dts%inext + 1
    if ( dts%inext.ge.56 ) dts%inext = 1

    dts%inextp = dts%inextp + 1
    if ( dts%inextp.ge.56 ) dts%inextp = 1

    mj = dts%ma(dts%inext) - dts%ma(dts%inextp)
    if ( mj.lt.MZ ) mj = mj + MBIG
    dts%ma(dts%inext) = mj
    rnd3 = mj
  end function k_get_next
  
    



  !/ =====================================================================================
  subroutine TESTK001
    !/ -----------------------------------------------------------------------------------
    use statistics_mod
     implicit none
   
     type(Knuth)         :: rnd
     type(running_stats) :: RS
     integer :: i, fh, x, ierr

     call RS%reset
     call rnd%seed( 3317 )

    open( NEWUNIT=fh, FILE='random-1M.dat', FORM='unformatted', ACTION='WRITE', &
         &            STATUS='REPLACE', IOSTAT=ierr, ACCESS='stream' )

    do i=1,1000000000
       x = rnd%get()
        call RS%sample( x )
        write( fh ) x
     end do

     close( fh )

     call RS%report( UNIT=OUTPUT_UNIT )

   end subroutine TESTK001







  
  !/ =====================================================================================
  subroutine TEST01
    !/ -----------------------------------------------------------------------------------
    use omp_lib
    implicit none
    
    integer, allocatable :: buffer(:)
    integer :: i,n,tid

    n = omp_get_num_procs()

    allocate( buffer(n) )

    do i=1,n
       buffer(i) = -1
    end do

    !$OMP PARALLEL DEFAULT(SHARED) PRIVATE(tid)
    tid = omp_get_thread_num() + 1
    buffer(tid) = tid * 10
       !$OMP END PARALLEL

    do i=1,n
       print *, buffer(i)
    end do

    deallocate( buffer )

  end subroutine TEST01


  !/ =====================================================================================
  subroutine TEST02
    !/ -----------------------------------------------------------------------------------
    use evo_entropy_mod
    implicit none

    integer       :: i, fh, ierr
    type(Entropy) :: ent
    real(dp)      :: x

    call ent%seed
    
    open( NEWUNIT=fh, FILE='random-1M.dat', FORM='unformatted', ACTION='WRITE', &
         &            STATUS='REPLACE', IOSTAT=ierr, ACCESS='stream' )

    do i=1,1000000
       x = ent%uniform()
       write( fh ) x
    end do

    close( fh )
    
  end subroutine TEST02




  

  !/ =====================================================================================
  subroutine TEST03
    !/ -----------------------------------------------------------------------------------
    use evo_entropy_mod
    use statistics_mod
    implicit none

    integer       :: i, fh, ierr, n
    type(Entropy) :: ent
    type(running_stats) :: RS
    integer      :: x

    call ent%seed
    call RS%reset
    
    open( NEWUNIT=fh, FILE='random-1G.dat', FORM='unformatted', ACTION='WRITE', &
         &            STATUS='REPLACE', IOSTAT=ierr, ACCESS='stream' )

    n = 1024*1024*1024
    
    do i=1,n
       x = ent%dtest()
       write(fh) x
       call RS%sample(x)
    end do

    close( fh )

    call RS%report( UNIT=OUTPUT_UNIT )
    
  end subroutine TEST03



  !/ =====================================================================================
  subroutine TEST04
    !/ -----------------------------------------------------------------------------------
    use evo_entropy_mod
    implicit none

    integer, parameter :: ia   = 16807       !! 7^5
    integer, parameter :: ib15 = 32768       !! 2^15
    integer, parameter :: ib16 = 65536       !! 2^16
    integer, parameter :: ip   = 2147483647  !! 2^31 - 1

    integer :: i, fh, ierr, n

    integer iprhi
    integer ixhi
    integer k
    integer leftlo
    integer loxa
    integer seed
    
    open( NEWUNIT=fh, FILE='random-1G.dat', FORM='unformatted', ACTION='WRITE', &
         &            STATUS='REPLACE', IOSTAT=ierr, ACCESS='stream' )

    n = 1024*1024*1024

    seed = 8388607
    
    do i=1,n
       ixhi   = seed / ib16
       loxa   = ( seed - ixhi * ib16 ) * ia
       leftlo = loxa / ib16
       iprhi  = ixhi * ia + leftlo
       k      = iprhi / ib15
       seed   = (((loxa - leftlo*ib16) - ip) + (iprhi - k*ib15) * ib16) + k
       if ( seed < 0 ) then
          seed = seed + ip
       end if
       write(fh) seed
    end do

    close( fh )
    
  end subroutine TEST04































  


 !/ =====================================================================================
  subroutine TEST_FGA
    !/ -----------------------------------------------------------------------------------
    use fga_mod
    use test_models
    
    implicit none

    type(Sphere)  :: model
    type(FGA) :: ga

    call model%build( NUM_VAR )

    call ga%build( model, POP_SIZE )
    call ga%evolve( MAX_GEN )
  end subroutine TEST_FGA



  !/ =====================================================================================
  subroutine TEST_FGA_OMP
    !/ -----------------------------------------------------------------------------------
    use fga_omp_mod
    use test_models
    implicit none

    type(Sphere)  :: model
    type(FGA_OMP) :: ga

    call model%build( NUM_VAR )

    call ga%build( model, POP_SIZE )
    call ga%evolve( MAX_GEN )
  end subroutine TEST_FGA_OMP







end module ftest_fga

!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_fga
  implicit none

 print *, ''
 print *, 'FGA'
 print *, ''

 call TEST_FGA
 
 print *, ''
  print *, 'FGA OMP'
  print *, ''

 call TEST_FGA_OMP

  print *, ''


  

end program main

!/ =======================================================================================
!/ **                                 F T E S T _ F G A                                 **
!/ =========================================================================== END FILE ==
