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
module tictactoe
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  !/ =====================================================================================
  type , public :: ttt_game
     !/ ----------------------------------------------------------------------------------
     character(len=1), allocatable :: board(:,:)
     integer                       :: order         =  0
     character(len=1)              :: blank_token   = '.'
     character(len=1)              :: player_token1 = 'O'
     character(len=1)              :: player_token2 = 'X'
  
   contains

     procedure, public :: delete   => ttt_delete
     procedure, public :: setOrder => ttt_set_order
     procedure, public :: reset    => ttt_reset
     procedure, public :: play     => ttt_play
     procedure, public :: get      => ttt_get_cell
     procedure, public :: check    => ttt_check_win
     procedure, public :: display  => ttt_display
     
     procedure :: ttt_load_state_vector
     procedure :: ttt_load_state_matrix
     procedure :: ttt_store_state_vector
     procedure :: ttt_store_state_matrix

     generic, public :: load  => ttt_load_state_vector,  ttt_load_state_matrix
     generic, public :: store => ttt_store_state_vector, ttt_store_state_matrix

     final :: ttt_destroy

  end type ttt_game

  
  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: ttt_size
  end interface size

  public :: size


  
 
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  function ttt_size( ttt ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Return the order of this game.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                    :: n   ! order of this game. Board is NxN
    type(ttt_game), intent(in) :: ttt ! reference to a tic-tac-toe game.
    !/ -----------------------------------------------------------------------------------
    n = ttt%order
  end function ttt_size


  !/ =====================================================================================
  subroutine ttt_delete( self )
    !/ -----------------------------------------------------------------------------------
    !! Deallocate storage
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game), intent(inout) :: self ! reference to this tic-tac-toe game.
    !/ -----------------------------------------------------------------------------------
    if ( allocated( self%board ) ) then
       deallocate( self%board )
    end if
    self%order = 0
  end subroutine ttt_delete

  
  !/ =====================================================================================
  subroutine ttt_destroy( ttt )
    !/ -----------------------------------------------------------------------------------
    !! Final destroy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ttt_game), intent(inout) :: ttt ! reference to a tic-tac-toe game.
    !/ -----------------------------------------------------------------------------------
    call ttt%delete
  end subroutine ttt_destroy

  
  !/ =====================================================================================
  subroutine ttt_set_order( self, n, PLAY1, PLAY2, BLANK )
    !/ -----------------------------------------------------------------------------------
    !! Set the order of this game.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game),            intent(inout) :: self  ! reference to this tic-tac-toe game.
    integer,                    intent(in)    :: n     ! desired order for this game.
    character(len=1), optional, intent(in)    :: PLAY1 ! token for player 1
    character(len=1), optional, intent(in)    :: PLAY2 ! token for player 2
    character(len=1), optional, intent(in)    :: BLANK ! token empty cell
    !/ -----------------------------------------------------------------------------------
    if ( n.ne.self%order ) then
       call self%delete
       allocate( self%board(n,n) )
       self%order = n
    end if

    self%blank_token   = '.'
    self%player_token1 = 'O'
    self%player_token2 = 'X'

    if ( present( PLAY1 ) ) then
       self%player_token1 = PLAY1
    end if
    
    if ( present( PLAY2 ) ) then
       self%player_token2 = PLAY2
    end if
    
    if ( present( BLANK ) ) then
       self%blank_token = BLANK
    end if
    
    call self%reset
    
  end subroutine ttt_set_order

  
  !/ =====================================================================================
  subroutine ttt_reset( self )
    !/ -----------------------------------------------------------------------------------
    !! Reset the game. Clear the board.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game), intent(inout) :: self ! reference to this tic-tac-toe game.
    !/ -----------------------------------------------------------------------------------
    integer :: r, c
    !/ -----------------------------------------------------------------------------------
    
    do concurrent( r=1:self%order, c=1:self%order )
       self%board(r,c) = self%blank_token
    end do
  end subroutine ttt_reset

  
  !/ =====================================================================================
  function ttt_play( self, row, col, player ) result( res )
    !/ -----------------------------------------------------------------------------------
    !! Play a move.
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                         :: res  ! .true. == play was successful
    class(ttt_game),  intent(inout) :: self ! reference to this tic-tac-toe game.
    integer,          intent(in)    :: row
    integer,          intent(in)    :: col
    character(len=1), intent(in)    :: player
    !/ -----------------------------------------------------------------------------------
    res = .false.
    if ( self%blank_token.eq.self%board(row,col) ) then
       if ( player.eq.self%player_token1 ) then
          self%board(row,col) = self%player_token1
          res = .true.
       else
          if ( player.eq.self%player_token2 ) then
             self%board(row,col) = self%player_token2
             res = .true.
          else
             write(ERROR_UNIT,100) self%player_token1, self%player_token2
          end if
       end if
    end if

100 format( 'player must be ',A1,' or ',A1 )
    
  end function ttt_play


  !/ =====================================================================================
  function hasEmptyCell( board, blank ) result( ec )
    !/ -----------------------------------------------------------------------------------
    !/ Check for empty cell
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                         :: ec
    character(len=1), intent(inout) :: board(:,:)
    character(len=1), intent(in)    :: blank
    !/ -----------------------------------------------------------------------------------
    integer :: r,c,nr,nc
    nr = size(board, DIM=1)
    nc = size(board, DIM=2)
    ec = .false.
    do r=1,nr
       do c=1,nc
          if ( blank.eq.board(r,c) ) then
             ec = .true.
             goto 10
          end if
       end do
    end do
10  continue
  end function hasEmptyCell
    
    
  !/ =====================================================================================
  function rowWin( board, row, player ) result( ec )
    !/ -----------------------------------------------------------------------------------
    !/ Check for a row win
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                         :: ec
    character(len=1), intent(inout) :: board(:,:)
    integer,          intent(in)    :: row
    character(len=1), intent(in)    :: player
    !/ -----------------------------------------------------------------------------------
    integer :: c,nc
    nc = size(board, DIM=2)
    ec = .true.
    do c=1,nc
       if ( player.ne.board(row,c) ) then
          ec = .false.
          goto 10
       end if
    end do
10  continue
  end function rowWin

    
  !/ =====================================================================================
  function colWin( board, col, player ) result( ec )
    !/ -----------------------------------------------------------------------------------
    !/ Check for a column win
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                         :: ec
    character(len=1), intent(inout) :: board(:,:)
    integer,          intent(in)    :: col
    character(len=1), intent(in)    :: player
    !/ -----------------------------------------------------------------------------------
    integer :: r,nr
    nr = size(board, DIM=1)
    ec = .true.
    do r=1,nr
       if ( player.ne.board(r,col) ) then
          ec = .false.
          goto 10
       end if
    end do
10  continue
  end function colWin

  !/ =====================================================================================
  function rightDiagonalWin( board, player ) result( ec )
    !/ -----------------------------------------------------------------------------------
    !/ Check for a column win
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                         :: ec
    character(len=1), intent(inout) :: board(:,:)
    character(len=1), intent(in)    :: player
    !/ -----------------------------------------------------------------------------------
    integer :: i,n,nc
    n  = size(board, DIM=1)
    nc = size(board, DIM=2)
    if (nc.lt.n) n = nc
    
    ec = .true.
    
    do i=1,n
       if ( player.ne.board(i,i) ) then
          ec = .false.
          goto 10
       end if
    end do
10  continue
  end function rightDiagonalWin

    
  !/ =====================================================================================
  function leftDiagonalWin( board, player ) result( ec )
    !/ -----------------------------------------------------------------------------------
    !/ Check for a column win
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical                         :: ec
    character(len=1), intent(inout) :: board(:,:)
    character(len=1), intent(in)    :: player
    !/ -----------------------------------------------------------------------------------
    integer :: i,n,nc
    n  = size(board, DIM=1)
    nc = size(board, DIM=2)
    if (nc.lt.n) n = nc
    
    ec = .true.
    
    do i=1,n
       if ( player.ne.board(i,n+1-i) ) then
          ec = .false.
          goto 10
       end if
    end do
10  continue
  end function leftDiagonalWin

    
  !/ =====================================================================================
  function ttt_check_win( self ) result( win )
    !/ -----------------------------------------------------------------------------------
    !! Check for win
    !!
    !! |  res  !  description   |
    !! | :---: | :------------: |
    !! |   0   |  not finished  |
    !! |   1   |  player 1 win  |
    !! |   2   |  player 2 win  |
    !! |   3   |  stale mate    |
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                        :: win  ! results
    class(ttt_game), intent(inout) :: self ! reference to this tic-tac-toe game.
    !/ -----------------------------------------------------------------------------------
    integer :: c, r
    
    win = 3
    if ( hasEmptyCell( self%board, self%blank_token ) ) then

       !/ ----- check rows ---------------------------------------------------------------
       
       do r=1,self%order
          if ( rowWin( self%board, r, self%player_token1 ) ) then
             win = 1
             goto 999
          end if
          if ( rowWin( self%board, r, self%player_token2 ) ) then
             win = 2
             goto 999
          end if
       end do
       
       !/ ----- check columns ------------------------------------------------------------
       
       do c=1,self%order
          if ( colWin( self%board, c, self%player_token1 ) ) then
             win = 1
             goto 999
          end if
          if ( colWin( self%board, c, self%player_token2 ) ) then
             win = 2
             goto 999
          end if
       end do

       !/ ----- check right diagonal -----------------------------------------------------
       
       if ( rightDiagonalWin( self%board, self%player_token1 ) ) then
          win = 1
          goto 999
       end if

       if ( rightDiagonalWin( self%board, self%player_token2 ) ) then
          win = 2
          goto 999
       end if

       !/ ----- check left diagonal ------------------------------------------------------

       if ( leftDiagonalWin( self%board, self%player_token1 ) ) then
          win = 1
          goto 999
       end if

       if ( leftDiagonalWin( self%board, self%player_token2 ) ) then
          win = 2
          goto 999
       end if

       win = 0
    end if
999 continue
  end function ttt_check_win

  
  !/ =====================================================================================
  function ttt_get_cell( self, row, col ) result( val )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                        :: val  ! vale at (row,col)
    class(ttt_game), intent(inout) :: self ! reference to this tic-tac-toe game.
    integer,         intent(in)    :: row  ! row index.
    integer,         intent(in)    :: col  ! column index.
    !/ -----------------------------------------------------------------------------------
    character(len=1) :: test
    !/ -----------------------------------------------------------------------------------
    
    test = self%board(row,col)

    val = -1
    if ( self%blank_token.eq.test ) then
       val = 0
    else
       if ( self%player_token1.eq.test ) then
          val = 1
       else
          if ( self%player_token2.eq.test ) then
             val = 2
          else
             write(ERROR_UNIT,*) 'unknown token was loaded in cell'
          end if
       end if
    end if

  end function ttt_get_cell

    
  !/ =====================================================================================
  subroutine ttt_display( self, UNIT )
    !/ -----------------------------------------------------------------------------------
    !! Output the current state on a unit
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game),            intent(inout) :: self  ! reference to this tic-tac-toe game.
    integer,          optional, intent(in)    :: UNIT  ! output unit
    !/ -----------------------------------------------------------------------------------
    integer :: un, r, c

    un = OUTPUT_UNIT
    if ( present( UNIT ) ) then
       un = UNIT
    end if

    write(un,100) (c,c=1,self%order)
    do r=1,self%order
       write(un,110) r, (self%board(r,c),c=1,self%order)
    end do

100 format(' ',*(' ',I1))
110 format(I1,*(' ',A1))

  end subroutine ttt_display

  
  !/ =====================================================================================
  subroutine ttt_store_state_vector( self, state )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game),  intent(inout) :: self     ! reference to this tic-tac-toe game.
    character(len=1), intent(inout) :: state(:) ! state vector
    !/ -----------------------------------------------------------------------------------
    integer :: idx, r, c

    idx = 1
    do r=1,self%order
       do c=1,self%order
          state(idx) = self%board(r,c)
          idx = idx + 1
       end do
    end do
  end subroutine ttt_store_state_vector

  
  !/ =====================================================================================
  subroutine ttt_store_state_matrix( self, state )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game),  intent(inout) :: self       ! reference to this tic-tac-toe game.
    character(len=1), intent(inout) :: state(:,:) ! state matrix
    !/ -----------------------------------------------------------------------------------
    integer :: r, c

    do r=1,self%order
       do c=1,self%order
          state(r,c) = self%board(r,c)
       end do
    end do
  end subroutine ttt_store_state_matrix

  
  !/ =====================================================================================
  subroutine ttt_load_state_vector( self, state )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game),  intent(inout) :: self     ! reference to this tic-tac-toe game.
    character(len=1), intent(inout) :: state(:) ! state vector
    !/ -----------------------------------------------------------------------------------
    integer :: idx, r, c

    idx = 1
    do r=1,self%order
       do c=1,self%order
          self%board(r,c) = state(idx)
          idx = idx + 1
       end do
    end do
  end subroutine ttt_load_state_vector

  
  !/ =====================================================================================
  subroutine ttt_load_state_matrix( self, state )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ttt_game),  intent(inout) :: self       ! reference to this tic-tac-toe game.
    character(len=1), intent(inout) :: state(:,:) ! state matrix
    !/ -----------------------------------------------------------------------------------
    integer :: r, c

    do r=1,self%order
       do c=1,self%order
          self%board(r,c) = state(r,c)
       end do
    end do
  end subroutine ttt_load_state_matrix

  
end module tictactoe


!/ =======================================================================================
!/ **                              T T T _ M A T C H B O X                              **
!/ =========================================================================== END FILE ==
