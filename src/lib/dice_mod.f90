!/ ====================================================================== BEGIN FILE =====
!/ **                                  D I C E _ M O D                                  **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 1990, 2009, 2016, 2019, Stephen W. Soliday                         **
!/ **                stephen.soliday@trncmp.org                                         **
!/ **                http://research.trncmp.org                                         **
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
!! author:  Stephen W. Soliday
!! date:    2019-12-20
!! license: GPL
!!
!! ## Dice
!!
!! Dice, is a collection of utilities for generating various random distributions.
!! It derives its name from the first instantiation written in Basic in 1985.
!! That version produced die rolls for AD&D in the form of nDs+b
!! (example:    4d6+2 = sum 4 six sided dice rolls and add 2 for a pesudo normal
!! distribution with a mean of 16 a variance of 35/3 and bounded from 6 to 26.
!
!/ =======================================================================================
module dice_mod
  !/ -------------------------------------------------------------------------------------
  use entropy_mod
  use tlogger
  implicit none

  type :: Dice

     type(entropy_source) :: ent  !! entropy source
     real(dp), private    :: rand1
     real(dp), private    :: rand2
     logical,  private    :: have_spare = .false.
     
   contains
     procedure, private :: dd_index_int16
     procedure, private :: dd_index_int32
     procedure, private :: dd_seed_set_from_system
     procedure, private :: dd_seed_set_from_integer
     procedure, private :: dd_shuffle_real8
     procedure, private :: dd_shuffle_real4
     procedure, private :: dd_shuffle_int32
     procedure, private :: dd_shuffle_int16
     
     procedure :: uniform      => dd_uniform_real8
     procedure :: uniform_list => dd_uniform_real8_list

     procedure :: normal       => dd_normal_real8
     procedure :: normal_list  => dd_normal_real8_list

     procedure :: boolean      => dd_boolean
     procedure :: boolean_list => dd_boolean_list

     procedure :: seed_size    => dd_seed_size
     
     procedure :: roll         => dungeon_dice
     generic   :: index        => dd_index_int16, dd_index_int32
     generic   :: seed_set     => dd_seed_set_from_system, dd_seed_set_from_integer

     generic   :: shuffle      => dd_shuffle_real8, dd_shuffle_int32, &
          &                       dd_shuffle_real4, dd_shuffle_int16

  end type Dice



  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================






  !/ =====================================================================================
  subroutine dd_seed_set_from_system( dd )
    !/ -----------------------------------------------------------------------------------
    !! Seed the random number generator with data retrieved from the operating system.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(in) :: dd  !! reference this Dice object
    !/ -----------------------------------------------------------------------------------
    call dd%ent%seed_set
  end subroutine dd_seed_set_from_system
  

  !/ =====================================================================================
  subroutine dd_seed_set_from_integer( dd, matter )
    !/ -----------------------------------------------------------------------------------
    !! Seed the random number generator from an integer array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(in) :: dd         !! reference this Dice object
    integer,     intent(in) :: matter(:)  !! integer source array
    !/ -----------------------------------------------------------------------------------
    call dd%ent%seed_set( matter )
  end subroutine dd_seed_set_from_integer
  

  !/ =====================================================================================
  function dd_seed_size( dd ) result( s )
    !/ -----------------------------------------------------------------------------------
    !! Get the seed size. This is the number of 32 bit intgeers required to uniqully
    !! seed this random number generator.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(in) :: dd !! reference this Dice object
    integer                 :: s    !! number of integers.
    !/ -----------------------------------------------------------------------------------
    s = dd%ent%seed_size()
  end function dd_seed_size
  


  

  
  !/ =====================================================================================
  function dungeon_dice( dd, NUM, SIDE, BONUS ) result( r )
    !/ -----------------------------------------------------------------------------------
    !! Dungeon Dice
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),       intent(inout) :: dd    !! reference to this dice object
    integer, optional, intent(in)    :: NUM   !! number of dice to sum over  (default: 1)
    integer, optional, intent(in)    :: SIDE  !! number of sides on the dice (default: 6)
    integer, optional, intent(in)    :: BONUS !! bonus or penalty            (default: 0)
    integer                          :: r     !! complete roll.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, s
    !/ -----------------------------------------------------------------------------------
    n = 1
    s = 6
    r = 0

    if ( present( SIDE ) ) then
       s = SIDE
       if ( 1.gt.s ) then
          call log_error( 'DungeonDice: SIDE must be a whole number' )
          goto 999
       end if
    end if

    if ( present( NUM ) ) then
       n = NUM
    end if

    if ( 0.lt.n ) then
       if ( 1.eq.n ) then
          r = dd%ent%I32(s)
          goto 999
       end if
       r = 0
       do i=1,n
          r = r + dd%ent%I32(s)
       end do
    else
       call log_error( 'DungeonDice: NUM must be a whole number' )
       goto 999
    end if

    if ( present( BONUS ) ) then
       r = r + BONUS
    end if

999 continue

  end function dungeon_dice




  !/ =====================================================================================
  function dd_index_int16( dd, mx ) result( r )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: dd  !! reference to this dice object
    integer(int16), intent(in)    :: mx  !! maximum index value
    integer(int16)                :: r   !! random integer [1,mx]
    !/ -----------------------------------------------------------------------------------
    r = dd%ent%I16( mx )
  end function dd_index_int16


  !/ =====================================================================================
  function dd_index_int32( dd, mx ) result( r )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: dd    !! reference to this dice object
    integer(int32), intent(in)    :: mx
    integer(int32)                :: r
    !/ -----------------------------------------------------------------------------------
    r = dd%ent%I32( mx )
  end function dd_index_int32


  !/ =====================================================================================
  function dd_uniform_real8( dd ) result( r )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd !! reference to this dice object
    real(dp)                   :: r
     !/ -----------------------------------------------------------------------------------
   r = dd%ent%R64()
 end function dd_uniform_real8

  
  !/ =====================================================================================
  subroutine dd_uniform_real8_list( dd, list )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd    !! reference to this dice object
    real(dp),    intent(out)   :: list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    do i=1,n
       list(i) = dd%ent%R64()
    end do
  end subroutine dd_uniform_real8_list
  

  !/ =====================================================================================
  function dd_normal_real8( dd ) result( r )
    !/ -----------------------------------------------------------------------------------
 !!  Use Box-Muller algorithm to generate numbers with a normal distribution.
 !!  The mean is 0.0, the standard deviation is 1.0 and limits +/- 6.15 * StdDev,
!!  based on experimental results of rolling 1 trillion values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd !! reference to this dice object
    real(dp)                   :: r
     !/ -----------------------------------------------------------------------------------
    if ( dd%have_spare ) then
       dd%have_spare = .false.
       r = sqrt(dd%rand1) * sin(dd%rand2)
    else
       dd%have_spare = .true.
       dd%rand1 = dd%ent%R64()
       if (dd%rand1.lt.1.0e-100) then
          dd%rand1 = 1.0e-100
       end if
       dd%rand1 = -D_TWO * log(dd%rand1)
       dd%rand2 = D_2PI*dd%ent%R64()
       r = sqrt(dd%rand1) * cos(dd%rand2)
    end if
 end function dd_normal_real8

  
  !/ =====================================================================================
  function dd_boolean( dd, THRESHOLD ) result( r )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd !! reference to this dice object
    real(dp), optional, intent(in) :: THRESHOLD
    logical                    :: r
    !/ -----------------------------------------------------------------------------------
    real(dp) :: t, x
    !/ -----------------------------------------------------------------------------------
    if ( present( THRESHOLD ) ) then
       t = THRESHOLD
    else
       t = D_HALF
    end if
    
    x = dd%ent%R64()
    r = x.lt.t
    
  end function dd_boolean


  !/ =====================================================================================
  subroutine dd_normal_real8_list( dd, list )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd    !! reference to this dice object
    real(dp),    intent(out)   :: list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    do i=1,n
       list(i) = dd%ent%R64()
    end do
  end subroutine dd_normal_real8_list
  
  !/ =====================================================================================
  subroutine dd_boolean_list( dd, list, THRESHOLD )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd !! reference to this dice object
    logical, intent(out) :: list(:)
    real(dp), optional, intent(in) :: THRESHOLD
    !/ -----------------------------------------------------------------------------------
    real(dp) :: t, x
    integer  :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(list)

    if ( present( THRESHOLD ) ) then
       t = THRESHOLD
    else
       t = D_HALF
    end if

    do i=1,n
       x = dd%ent%R64()
       list(i) = x.lt.t
    end do
    
  end subroutine dd_boolean_list




  !/ =====================================================================================
  subroutine dd_shuffle_real8( dd, list )
    !/ -----------------------------------------------------------------------------------
    !!  Using the Knuth Shuffle, page 145 Volume 2
    !!  See also Persi Diaconis (3/2) Log_2(N)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd      !! reference to this dice object
    real(dp),    intent(inout) :: list(:) !! array
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, n
    real(dp) :: temp
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    do i=n,1,-1
       j = dd%index(i)
       temp    = list(i)
       list(i) = list(j)
       list(j) = temp
    end do
  end subroutine dd_shuffle_real8


  !/ =====================================================================================
  subroutine dd_shuffle_real4( dd, list )
    !/ -----------------------------------------------------------------------------------
    !!  Using the Knuth Shuffle, page 145 Volume 2
    !!  See also Persi Diaconis (3/2) Log_2(N)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: dd      !! reference to this dice object
    real(sp),    intent(inout) :: list(:) !! array
    !/ -----------------------------------------------------------------------------------
    integer  :: i, j, n
    real(sp) :: temp
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    do i=n,1,-1
       j = dd%index(i)
       temp    = list(i)
       list(i) = list(j)
       list(j) = temp
    end do
  end subroutine dd_shuffle_real4


  !/ =====================================================================================
  subroutine dd_shuffle_int32( dd, list )
    !/ -----------------------------------------------------------------------------------
    !!  Using the Knuth Shuffle, page 145 Volume 2
    !!  See also Persi Diaconis (3/2) Log_2(N)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: dd      !! reference to this dice object
    integer(int32), intent(inout) :: list(:) !! array
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n
    integer(int32) :: temp
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    do i=n,1,-1
       j = dd%index(i)
       temp    = list(i)
       list(i) = list(j)
       list(j) = temp
    end do
  end subroutine dd_shuffle_int32

  
  !/ =====================================================================================
  subroutine dd_shuffle_int16( dd, list )
    !/ -----------------------------------------------------------------------------------
    !!  Using the Knuth Shuffle, page 145 Volume 2
    !!  See also Persi Diaconis (3/2) Log_2(N)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: dd      !! reference to this dice object
    integer(int16), intent(inout) :: list(:) !! array
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n
    integer(int16) :: temp
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    do i=n,1,-1
       j = dd%index(i)
       temp    = list(i)
       list(i) = list(j)
       list(j) = temp
    end do
  end subroutine dd_shuffle_int16

  
end module dice_mod
