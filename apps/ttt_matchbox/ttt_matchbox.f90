!/ ====================================================================== BEGIN FILE =====
!/ **                              T T T _ M A T C H B O X                              **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
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
!/ ----- Modification History ------------------------------------------------------------
!
!> @brief   Teach matchboxes to play tic tac toe.
!!
!! @details Provides Console game play.
!!
!! @author  Stephen W. Soliday
!! @date    2018-12-28
!
!/ =======================================================================================
module ttt_matchbox
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tictactoe
  use matchbox_mod

  implicit none

  logical :: SHOW_BOARD = .false.
  logical :: SHOW_FINAL = .false.
  
  !/ =====================================================================================
  type, extends(Player) :: ManualPlayer
     !/ -----------------------------------------------------------------------------------

   contains
     procedure, public :: init => mp_set_order
     procedure, public :: play => mp_play
  end type ManualPlayer


  !/ =====================================================================================
  type, extends(Player) :: RandomPlayer
     !/ -----------------------------------------------------------------------------------

   contains
     procedure, public :: init => rp_set_order
     procedure, public :: play => rp_play
  end type RandomPlayer




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine mp_set_order( self, order, BLANK )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ManualPlayer), intent(inout) :: self
    integer,             intent(in)    :: order
    character(len=1), optional, intent(in)    :: BLANK ! token empty cell
    !/ -----------------------------------------------------------------------------------
    self%order = order
  end subroutine mp_set_order


  !/ =====================================================================================
  subroutine mp_play( self, row, col, state, move )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ManualPlayer), intent(inout) :: self
    integer,             intent(out)   :: row
    integer,             intent(out)   :: col
    character(len=1),    intent(inout) :: state(:,:)
    integer,             intent(in)    :: move
    !/ -----------------------------------------------------------------------------------
    write(OUTPUT_UNIT,100,ADVANCE='NO')
    read(INPUT_UNIT,*) row, col
100 format('1>> ')
  end subroutine mp_play


  !/ =====================================================================================
  subroutine rp_set_order( self, order, BLANK )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RandomPlayer), intent(inout) :: self
    integer,             intent(in)    :: order
    character(len=1), optional, intent(in)    :: BLANK ! token empty cell
    !/ -----------------------------------------------------------------------------------
    call RANDOM_SEED 
    self%order = order
  end subroutine rp_set_order


  !/ =====================================================================================
  subroutine rp_play( self, row, col, state, move )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RandomPlayer), intent(inout) :: self
    integer,             intent(out)   :: row
    integer,             intent(out)   :: col
    character(len=1),    intent(inout) :: state(:,:)
    integer,             intent(in)    :: move
    !/ -----------------------------------------------------------------------------------
    real(dp) :: x(2)

    call RANDOM_NUMBER(x)

    row = int( x(1) * real(self%order,dp) ) + 1
    col = int( x(2) * real(self%order,dp) ) + 1

    if ( row.lt.0 )          row = 0
    if ( row.gt.self%order ) row = self%order
    if ( col.lt.0 )          col = 0
    if ( col.gt.self%order ) col = self%order

  end subroutine rp_play


end module ttt_matchbox


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ttt_matchbox
  use matchbox_mod
  implicit none
  !/ -------------------------------------------------------------------------------------
  integer, parameter :: GORD = 3

  type(ttt_game) :: game
  logical :: res
  integer :: row,col,test
  character(len=1), allocatable :: ms1(:,:)
  character(len=1), allocatable :: ms2(:,:)
  character(len=1) :: dummy

  type(RandomPlayer)   :: player2
  type(MatchBoxPlayer) :: player1

  integer :: PWIN1, PWIN2, GC, STALE, move
  !/ -------------------------------------------------------------------------------------
  
  PWIN1 = 0
  PWIN2 = 0
  STALE = 0

  SHOW_BOARD = .false.
  SHOW_FINAL = .true.
  MB_DEBUG   = .true.
  
  call game%setOrder( GORD, PLAY1='O', PLAY2='X', BLANK='.' )

  allocate( ms1(GORD,GORD) )
  allocate( ms2(GORD,GORD) )

  call player1%init( GORD, BLANK='.' )
  call player2%init( GORD, BLANK='.' )

  GC = 0

5 continue

  GC = GC + 1

  test = -1

  call game%reset

  if ( SHOW_BOARD ) call game%display

  move = 0
  
10 continue
  move = move + 1
  if ( SHOW_BOARD ) write(OUTPUT_UNIT,*)

  !/ --------------------------------------------
20 continue
  call game%store( ms1 )
  call player1%play(row,col,ms1,move)
  if ( game%play(row, col,'O') ) then
     if ( SHOW_BOARD ) write(OUTPUT_UNIT,*)
     if ( SHOW_BOARD ) call game%display
  else
     !write(OUTPUT_UNIT,*) 'bad move'
     goto 20
  end if

  test = game%check()
  if ( 0.ne.test ) goto 40

  !/ --------------------------------------------
30 continue
  call game%store( ms2 )
  call player2%play(row,col,ms2,move)
  if ( game%play(row, col,'X') ) then
     if ( SHOW_BOARD ) write(OUTPUT_UNIT,*)
     if ( SHOW_BOARD ) call game%display
  else
     goto 30
  end if

  test = game%check()
  if ( 0.eq.test) goto 10

  !/ --------------------------------------------
40 continue

if ( SHOW_FINAL ) call game%display

  if ( 1.eq.test ) then
     PWIN1 = PWIN1 + 1
     write(OUTPUT_UNIT,1000) GC, PWIN1, size(player1,DIM=2), PWIN2, size(player2,DIM=2), STALE
     call player1%win
     call player2%lose
  else
     if ( 2.eq.test ) then
        PWIN2 = PWIN2 + 1
        write(OUTPUT_UNIT,1100) GC, PWIN1, size(player1,DIM=2), PWIN2, size(player2,DIM=2), STALE
        call player1%lose
        call player2%win
     else
        if ( 3.eq.test ) then
           STALE = STALE + 1
           write(OUTPUT_UNIT,1200) GC, PWIN1, size(player1,DIM=2), PWIN2, size(player2,DIM=2), STALE
           call player1%stalemate
           call player2%stalemate
        else
           write(OUTPUT_UNIT,*) 'error'
           goto 999
        end if
     end if
  end if
  
!read(*,*) dummy
  write(*,*) '=========================================================='
  goto 5

999 continue


1000 format( I0, ': player one wins (',I0,'/',I0,') (',I0,'/',I0,')  ',I0 )
1100 format( I0, ': player two wins (',I0,'/',I0,') (',I0,'/',I0,')  ',I0 )
1200 format( I0, ': stale mate      (',I0,'/',I0,') (',I0,'/',I0,')  ',I0 )


end program main


!/ =======================================================================================
!/ **                              T T T _ M A T C H B O X                              **
!/ =========================================================================== END FILE ==
