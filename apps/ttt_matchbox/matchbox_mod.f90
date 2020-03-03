!/ ====================================================================== BEGIN FILE =====
!/ *                              M A T C H B O X _ M O D                              **
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
!> @brief   Teach MatchBoxes to play tic tac toe.
!!
!! @details Provides Console game play.
!!
!! @author  Stephen W. Soliday
!! @date    2018-12-28
!
!/ =======================================================================================
module matchbox_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  integer :: master_count = 1

  logical, public :: MB_DEBUG = .false.

  
  !/ =====================================================================================
  type, public :: MatchBox
     !/ -----------------------------------------------------------------------------------
     character(len=1), allocatable :: state(:,:)
     integer,          allocatable :: beads(:,:)
     integer                       :: pulled(2) =  (/0, 0/)
     integer                       :: order     =  0
     integer                       :: id        =  0
     integer                       :: move      =  0
     class(MatchBox), pointer      :: next      => null()

   contains

     procedure, public :: compare   => box_compare
     procedure, public :: choose    => box_choose
     procedure, public :: adjust    => box_adjust

     final :: box_destroy

  end type MatchBox

  
  !/ -------------------------------------------------------------------------------------
  interface createMatchBox
     !/ ----------------------------------------------------------------------------------
     module procedure :: box_alloc
  end interface createMatchBox

  
  !/ -------------------------------------------------------------------------------------
  interface display
     !/ ----------------------------------------------------------------------------------
     module procedure :: box_display
     module procedure :: pair_display
  end interface display
  


  !/ =====================================================================================
  type, public :: Player
     !/ -----------------------------------------------------------------------------------
     integer :: order = 0
     integer :: count = 0

   contains

     procedure, public :: win       => pl_win
     procedure, public :: stalemate => pl_stalemate
     procedure, public :: lose      => pl_lose
     
  end type Player

  
  !/ =====================================================================================
  type, extends(Player), public :: MatchBoxPlayer
     !/ -----------------------------------------------------------------------------------

     class(MatchBox),pointer :: head          => null()
     character(len=1)        :: blank_token   = '.'
     character(len=1)        :: player_token1 = 'O'
     character(len=1)        :: player_token2 = 'X'

   contains
     
     procedure, public :: init      => mb_init_player
     procedure, public :: add       => mb_add_box
     procedure, public :: find      => mb_find_box
     procedure, public :: play      => mb_play

     procedure, public :: adjust    => mb_adjust
     procedure, public :: win       => mb_win
     procedure, public :: stalemate => mb_stalemate
     procedure, public :: lose      => mb_lose

     final :: mb_destroy

  end type MatchBoxPlayer

  
  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: pl_size
  end interface size

  
  public :: size
  public :: display
  public :: createMatchBox
  

  

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  !/ =====================================================================================
  subroutine pl_win( self )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Player), intent(inout) :: self
  end subroutine pl_win


  !/ =====================================================================================
  subroutine pl_stalemate( self )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Player), intent(inout) :: self
  end subroutine pl_stalemate


  !/ =====================================================================================
  subroutine pl_lose( self )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Player), intent(inout) :: self
  end subroutine pl_lose


  !/ =====================================================================================
  function pl_size( plr, DIM ) result( n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                             :: n
    class(Player), intent(inout) :: plr
    integer, optional,   intent(in)    :: DIM
    !/ -----------------------------------------------------------------------------------

    if ( present( DIM ) ) then
       if ( 1.eq.DIM ) then
          n = plr%order
       else
          n = plr%count
       end if
    else
       n = plr%order
    end if

  end function pl_size
  

  

  !/ =====================================================================================
  subroutine pair_display( state, beads, UNIT )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=1), intent(inout) :: state(:,:)
    integer,          intent(inout) :: beads(:,:)
    integer, optional, intent(in) :: UNIT
    !/ -----------------------------------------------------------------------------------
    integer :: un, nr, nc, r, c
    !/ -----------------------------------------------------------------------------------

    if ( MB_DEBUG ) then
       
    un = OUTPUT_UNIT
    if ( present( UNIT ) ) then
       un = UNIT
    end if

    nr = size( state, DIM=1 )
    nc = size( state, DIM=2 )

    do r=1,nr
       write(un,110,ADVANCE='NO') r, (state(r,c),c=1,nc)
       write(un,120) (beads(r,c),c=1,nc)
    end do

 end if
    
110 format(I1,*(' ',A1))
120 format('   ',*(' ',I4))

  end subroutine pair_display
  
     
  !/ =====================================================================================
  subroutine box_display( box, UNIT )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(MatchBox), intent(inout) :: box !! reference to a MatchBox
    integer, optional, intent(in) :: UNIT
    !/ -----------------------------------------------------------------------------------

       call pair_display( box%state, box%beads, UNIT )
    
  end subroutine box_display

  
  !/ =====================================================================================
  function box_alloc( state, blank, move, NEXT ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !/ Constructor, allocate a new MatchBox.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBox), pointer                          :: ptr        !! reference to this Box
    character(len=1),                   intent(in)    :: state(:,:) !! character for a blank cell
    character(len=1),                   intent(in)    :: blank
    integer,                            intent(in)    :: move       !! move this state belongs to
    class(MatchBox), pointer, optional, intent(inout) :: NEXT       !! pointer to next Box
    !/ -----------------------------------------------------------------------------------
    integer :: r, c, n, init_beads
    !/ -----------------------------------------------------------------------------------
    
    n = size( state, DIM=1 )

    allocate( ptr )
    allocate( ptr%state(n,n) )
    allocate( ptr%beads(n,n) )

    ptr%id = master_count
    master_count = master_count + 1

    ptr%pulled = (/0, 0/)

    init_beads = n - move + 1
    if ( init_beads.lt.1 ) init_beads = 1

    do c=1,n
       do r=1,n
          ptr%state(r,c) = state(r,c)
          if ( blank.eq.ptr%state(r,c) ) then
             ptr%beads(r,c) = init_beads
          else
             ptr%beads(r,c) = 0
          end if
       end do
    end do

    ptr%order = n
    ptr%move  = move

    if ( present( NEXT ) ) then
       ptr%next => NEXT
    else
       ptr%next => null()
    end if

  end function box_alloc


  !/ =====================================================================================
  subroutine box_destroy( mb )
    !/ -----------------------------------------------------------------------------------
    !/ Deconstructor, free memory allocation.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(MatchBox), intent(inout) :: mb !! reference to this Box
    !/ -----------------------------------------------------------------------------------

    if ( allocated( mb%state ) ) deallocate( mb%state )
    if ( allocated( mb%beads ) ) deallocate( mb%beads )

    mb%order  = 0
    mb%pulled = (/ 0, 0 /)

    mb%next => null()

  end subroutine box_destroy


  !/ =====================================================================================
  function box_compare( self, state ) result( res )
    !/ -----------------------------------------------------------------------------------
    !/ Compare.
    !/ -----------------------------------------------------------------------------------
    implicit none
    logical :: res
    class(MatchBox),  intent(inout) :: self !! reference to this Box
    character(len=1), intent(inout) :: state(:,:)
    !/ -----------------------------------------------------------------------------------
    integer :: r,c
    !/ -----------------------------------------------------------------------------------

    res = .true.
    loop: do c=1,self%order
       do r=1,self%order
          if ( self%state(r,c).ne.state(r,c) ) then
             res = .false.
             exit loop
          end if
       end do
    end do loop
    
  end function box_compare


  !/ =====================================================================================
  subroutine box_choose( self, row, col )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBox), intent(inout) :: self !! reference to this Box
    integer, intent(out) :: row
    integer, intent(out) :: col
    !/ -----------------------------------------------------------------------------------
    integer  :: r, c, S, P
    real(dp) :: x
    !/ -----------------------------------------------------------------------------------
    
    P = 0
    do c=1,self%order
       do r=1,self%order
          P = P + self%beads(r,c)
       end do
    end do
    
    call RANDOM_NUMBER(x)
    
    S = int( x * real(P,dp) ) + 1
    P = 0
    loop: do c=1,self%order
       do r=1,self%order
          P = P + self%beads(r,c)
          if ( P.ge.S ) then
             self%pulled(1) = r
             self%pulled(2) = c
             exit loop
          end if
       end do
    end do loop

    row = self%pulled(1)
    col = self%pulled(2)

  end subroutine box_choose


  !/ =====================================================================================
  subroutine box_adjust( self, modify )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBox), intent(inout) :: self !! reference to this Box
    integer,         intent(in)    :: modify
    !/ -----------------------------------------------------------------------------------
    integer r, c, x
    !/ -----------------------------------------------------------------------------------
    r = self%pulled(1)
    c = self%pulled(2)

    x = self%beads(r,c) + modify

    if ( x.lt.0 )   x = 0
    if ( x.gt.999 ) x = 999

    self%beads(r,c) = x
    
  end subroutine box_adjust








  !/ =====================================================================================
  subroutine mb_init_player( self, order, PLAY1, PLAY2, BLANK )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self
    integer,               intent(in)    :: order
    character(len=1), optional, intent(in)    :: PLAY1 ! token for player 1
    character(len=1), optional, intent(in)    :: PLAY2 ! token for player 2
    character(len=1), optional, intent(in)    :: BLANK ! token empty cell
    !/ -----------------------------------------------------------------------------------
    call RANDOM_SEED
    self%head  => null()
    self%order =  order

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

  end subroutine mb_init_player


  !/ =====================================================================================
  subroutine mb_destroy( plr )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(MatchBoxPlayer), intent(inout) :: plr
    !/ -----------------------------------------------------------------------------------
    class(MatchBox), pointer :: ptr
    !/ -----------------------------------------------------------------------------------

10  continue
    ptr => plr%head
    if ( .not.associated( ptr ) ) goto 20
    plr%head => ptr%next
    deallocate( ptr )
    goto 10
20  continue

  end subroutine mb_destroy


  !/ =====================================================================================
  subroutine mb_add_box( self, state, move )
    !/ -----------------------------------------------------------------------------------
    !/ Add a new state
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self       !! reference to this player
    character(len=1),      intent(inout) :: state(:,:) !! 
    integer,               intent(in)    :: move
    !/ -----------------------------------------------------------------------------------
    class(MatchBox), pointer :: ptr

    self%count = self%count + 1

    ptr       => createMatchBox( state, self%blank_token, move, NEXT=self%head )
    self%head => ptr

  end subroutine mb_add_box
  

  !/ =====================================================================================
  function mb_find_box( self, state ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBox), pointer             :: ptr
    class(MatchBoxPlayer), intent(inout) :: self
    character(len=1),      intent(inout) :: state(:,:) 
    !/ -----------------------------------------------------------------------------------
    class(MatchBox), pointer :: temp
    !/ -----------------------------------------------------------------------------------

    ptr => null()
    temp => self%head

10  continue
    if ( .not.associated( temp ) ) goto 20
    if ( temp%compare( state ) ) then
       ptr => temp
       goto 20
    end if
    temp => temp%next
    goto 10
20  continue

  end function mb_find_box



  !/ =====================================================================================
  subroutine mb_play( self, row, col, state, move )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self
    integer,               intent(out)   :: row
    integer,               intent(out)   :: col
    character(len=1),      intent(inout) :: state(:,:)
    integer,               intent(in)    :: move
    !/ -----------------------------------------------------------------------------------
    class(MatchBox), pointer :: ptr

    ptr => self%find( state )

    if ( .not.associated( ptr ) ) then
       call self%add( state, move )
       ptr => self%head
       if ( MB_DEBUG ) write(OUTPUT_UNIT,100) ptr%id, self%count
    else
       if ( MB_DEBUG ) write(OUTPUT_UNIT,110) ptr%id, self%count
    end if
    
    call ptr%choose( row, col )
    
    if ( MB_DEBUG ) then
       call display( ptr )

       write(OUTPUT_UNIT,120) row, col
       write(OUTPUT_UNIT,*)
    end if

100 format('----- new(',I0,'/',I0,')----------')
110 format('----- old(',I0,'/',I0,')----------')
120 format('chose (',I0,',',I0,')')

  end subroutine mb_play





  !/ =====================================================================================
  subroutine mb_adjust( self, modify )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self
    integer, intent(in) :: modify
    !/ -----------------------------------------------------------------------------------
    class(MatchBox), pointer :: ptr
    !/ -----------------------------------------------------------------------------------

    ptr => self%head

10  continue
    if ( .not.associated( ptr ) ) goto 20
    call ptr%adjust( modify )
    ptr => ptr%next
    goto 10
20  continue

  end subroutine mb_adjust


  !/ =====================================================================================
  subroutine mb_win( self )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------
    write(*,*) 'MatchBoxPlayer::Win'
    call self%adjust(3)
  end subroutine mb_win


  !/ =====================================================================================
  subroutine mb_stalemate( self )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------
    call self%adjust(1)
  end subroutine mb_stalemate


  !/ =====================================================================================
  subroutine mb_lose( self )
    !/ -----------------------------------------------------------------------------------
    !/
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(MatchBoxPlayer), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------
    call self%adjust(-1)
  end subroutine mb_lose



  


end module matchbox_mod


!/ =======================================================================================
!/ **                              M A T C H B O X _ M O D                              **
!/ =========================================================================== END FILE ==
