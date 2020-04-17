!/ ====================================================================== BEGIN FILE =====
!/ **                              F T E S T _ O P E N M P                              **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module ftest_openmp
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-15
  !! license: GPL
  !!
  !!##Test of MPMD capabilities of OpenMP.
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use dice_mod
  use omp_lib
  implicit none

  integer, parameter :: NPOP = 32
  integer, parameter :: NVAR = 4

  type(Dice) :: dd
  
  !/ =====================================================================================
  type :: PopMember
     !/ ----------------------------------------------------------------------------------
     integer  :: thread_service = -1
     integer  :: index          = -1
     real(dp) :: score          =  D_ZERO
     real(dp) :: param(NVAR)
  end type PopMember


  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine InitOneMember( member, id  )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PopMember), intent(inout) :: member
    integer,         intent(in)    :: id
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    !/ -----------------------------------------------------------------------------------
    member%index          = id
    member%thread_service = -1
    do i=1,NVAR
       member%param(i) = D_TWO * dd%uniform() - D_ONE
    end do
  end subroutine InitOneMember


  !/ =====================================================================================
  subroutine ScoreOneMember( member, model )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PopMember), intent(inout) :: member
    type(PopMember), intent(in)    :: model
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    real(dp) :: d, mse
    !/ -----------------------------------------------------------------------------------

    mse = D_ZERO
    do i=1,NVAR
       d = model%param(i) - member%param(i)
       mse = mse + (d*d)
    end do

  end subroutine ScoreOneMember


  !/ =====================================================================================
  subroutine ScoreAllMembers( member, model )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PopMember), intent(inout) :: member(:)
    type(PopMember), intent(in)    :: model
    !/ -----------------------------------------------------------------------------------
    integer  :: id, i, n
    !/ -----------------------------------------------------------------------------------

    n = size(member)

    !$OMP PARALLEL NUM_THREADS(18) SHARED( member, model, n ) PRIVATE( i, id )
    !$OMP DO
    do i=1,n
       id = omp_get_thread_num() + 1
       member(i)%thread_service = id
       call ScoreOneMember( member(i), model )
    end do
    !$OMP END DO
    !$OMP MASTER
       print *, id, omp_get_num_procs(), omp_get_max_threads(), omp_get_num_threads()
    !$OMP END MASTER
    !$OMP END PARALLEL

  end subroutine ScoreAllMembers


  !/ =====================================================================================
  subroutine InitAllMembers( member )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PopMember), intent(inout) :: member(:)
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------

    n = size(member)
    
    do i=1,n
       call InitOneMember( member(i), i )
    end do

  end subroutine InitAllMembers


  !/ =====================================================================================
  subroutine DisplayMember( member )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PopMember), intent(in) :: member
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    write(*,100) member%index, member%thread_service, &
         &                    (member%param(i),i=1,NVAR)

100 format( 'ID:',I3,' TD:',I3,' : ',ES12.5,' =',*(1X,F7.4) )

  end subroutine DisplayMember


end module ftest_openmp



!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_openmp
  implicit none
  !/ -------------------------------------------------------------------------------------
  type(PopMember) :: model
  type(PopMember) :: pop(NPOP)
  !/ -------------------------------------------------------------------------------------
  integer :: i, s(2)
  !/ -------------------------------------------------------------------------------------
  s = [ 31415, 27823 ]
  
  call dd%seed_set(s)
  
  call InitOneMember( model, 0 )
  call InitAllMembers( pop )

  call ScoreAllMembers( pop, model )

  do i=1,NPOP
     call DisplayMember( pop(i) )
  end do

end program main

!/ =======================================================================================
!/ **                              F T E S T _ O P E N M P                              **
!/ =========================================================================== END FILE ==
