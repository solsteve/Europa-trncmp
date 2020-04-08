!/ ====================================================================== BEGIN FILE =====
!/ **                                 F T E S T _ S G D                                 **
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
module ftest_sgd
  use exemplar_class
  use ffnn_sgd_mod


  character(*), parameter :: TEST_DATA = '../data/Yacht/airfoil_sig.exm'





  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine TEST_01
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(exemplar_pair_t)  :: E
    type(FFNN) :: net
    integer  :: num_inp, num_out, num_sam
    real(dp) :: score
    type(Dice) :: dd
    !/ -----------------------------------------------------------------------------------

    call dd%seed_set
    
    call read_pair( PAIR=E, FILE=TEST_DATA )

    num_inp = size( E%X, 1 )
    num_sam = size( E%X, 2 )
    num_out = size( E%Y, 1 )

    print *, num_sam, num_inp, num_out

    call net%build( num_inp, 3, ALPHA=0.005d0 )
    call net%setupLayer( 1, num_inp+num_out )
    call net%setupLayer( 2, num_inp / 2 )
    call net%setupLayer( 3, num_out )

    call net%init
    
    call SGD_Train( INPUT=E%X, DESIRED=E%Y, REPLACE=.false., &
         &          NETWORK=net, EPOCHS=100, BATCH=10, VERBOSE=1 )

    !call sgd_validate( INPUT=E%X, OUTPUT=E%Y, MSE=score )
    !print *,'Score =', score


    
  end subroutine TEST_01


end module ftest_sgd


!/ =======================================================================================
program main
  use ftest_sgd
  implicit none
  !/ -------------------------------------------------------------------------------------
  call TEST_01
end program main


!/ =======================================================================================
!/ **                                 F T E S T _ S G D                                 **
!/ =========================================================================== END FILE ==
