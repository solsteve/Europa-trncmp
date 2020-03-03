!/ ====================================================================== BEGIN FILE =====
!/ **                               E N T R O P Y _ M O D                               **
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
!/ ----- Modification History ------------------------------------------------------------
!! author:  Stephen W. Soliday
!! date:    2019-11-19
!! license: GPL
!!
!!##Entropy.
!!
!! .
!
!/ =======================================================================================
module entropy_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env

  
  type :: entropy_source
   contains
     procedure, private :: set_seed_system_source
     procedure, private :: set_seed_integer_source

     procedure :: seed_size => get_seed_size

     procedure :: R64       => uniform_64_bit_real
     procedure :: R32       => uniform_32_bit_real

     procedure :: I32       => uniform_32_bit_integer
     procedure :: I16       => uniform_16_bit_integer

     generic   :: seed_set  => set_seed_system_source, set_seed_integer_source
  end type entropy_source

  !/ -------------------------------------------------------------------------------------
  interface seed_map
     !/ ----------------------------------------------------------------------------------
     module procedure :: seed_map_integer
  end interface seed_map




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  


  !/ =====================================================================================
  subroutine system_entropy_source( buffer )
    !/ -----------------------------------------------------------------------------------
    !! Retrieve entropy from /dev/urandom
   !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(inout) :: buffer(:)
   !/ -----------------------------------------------------------------------------------
    call URANDOM( buffer )
  end subroutine system_entropy_source


  !/ =====================================================================================
  subroutine seed_map_integer( dst, src, NUM )
    !/ -----------------------------------------------------------------------------------
    !! Map seed material to random number generators seed.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,               intent(out) :: dst(:) !! destination buffer
    integer,               intent(in)  :: src(:) !! source buffer
    integer, optional,     intent(in)  :: NUM    !! use less than the src size.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n_dst, n_src
    !/ -----------------------------------------------------------------------------------
    n_src = size(src)
    if ( present( NUM ) ) then
       if ( NUM.lt.n_src ) then
          n_src = NUM
       end if
    end if

    n_dst = size(dst)

    if ( n_dst.eq.n_src ) then       !/ ----- buffers are equal size ---------------------
       do i=1,n_dst
          dst(i) = src(i)
       end do
    else if ( n_dst.lt.n_src ) then  !/ ----- destination is shorter than the source -----
       do i=1,n_dst
          dst(i) = src(i)
       end do
       do i=n_dst,n_src
          j=mod(i,n_dst)
          dst(j) = xor( dst(j), src(i) )
       end do
    else                             !/ ----- desination is longer than the source -------
       do i=1,n_dst
          j=mod(i,n_src)
          dst(i) = src(j)
       end do
    end if

  end subroutine seed_map_integer


  !/ =====================================================================================
  subroutine set_seed_system_source( dts )
    !/ -----------------------------------------------------------------------------------
    !! Seed the random number generator with data retrieved from the operating system.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts  !! reference this entropy_source
    !/ -----------------------------------------------------------------------------------
    integer :: n
    integer, allocatable :: seed(:)
    !/ -----------------------------------------------------------------------------------
    call random_seed( SIZE=n )
    allocate( seed( n ) )
    call system_entropy_source( seed )
    call random_seed( PUT=seed )
    deallocate( seed )
  end subroutine set_seed_system_source


  !/ =====================================================================================
  subroutine set_seed_integer_source( dts, matter )
    !/ -----------------------------------------------------------------------------------
    !! Seed the random number generator from an integer array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts        !! reference this entropy_source
    integer,               intent(in) :: matter(:)  !! integer source array
    !/ -----------------------------------------------------------------------------------
    integer :: ss, ms
    integer, allocatable :: seed(:)
    !/ -----------------------------------------------------------------------------------
    ms = size(matter)
    ss = dts%seed_size()

    if ( ms.eq.ss ) then
       call random_seed( PUT=matter )
    else
       allocate( seed(ss) )
       call seed_map( seed, matter )
       call random_seed( PUT=seed )
       deallocate( seed )
    end if

  end subroutine set_seed_integer_source


  !/ =====================================================================================
  function get_seed_size( dts ) result( s )
    !/ -----------------------------------------------------------------------------------
    !! Get the seed size. This is the number of 32 bit intgeers required to uniqully
    !! seed this random number generator.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts  !! reference this entropy_source.
    integer                           :: s    !! number of integers.
    !/ -----------------------------------------------------------------------------------
    call random_seed(size=s)
  end function get_seed_size


  !/ =====================================================================================
  function uniform_64_bit_real( dts ) result( r )
    !/ -----------------------------------------------------------------------------------
    !! Get random number 64 bit real. 0.le.r.lt.1
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts  !! reference this entropy_source.
    real(dp)                          :: r    !! 64 bit real random number.
    !/ -----------------------------------------------------------------------------------
    call random_number(r)
  end function uniform_64_bit_real


  !/ =====================================================================================
  function uniform_32_bit_real( dts ) result( r )
    !/ -----------------------------------------------------------------------------------
    !! Get random number 32 bit real. 0.le.r.lt.1
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts  !! reference this entropy_source.
    real(sp)                          :: r    !! 32 bit real random number.
    !/ -----------------------------------------------------------------------------------
    call random_number(r)
  end function uniform_32_bit_real


  !/ =====================================================================================
  function uniform_32_bit_integer( dts, maxv ) result( r )
    !/ -----------------------------------------------------------------------------------
    !! Get random number 32 bit integer. 1.le.r.le.max
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts  !! reference this entropy_source.
    integer(int32),        intent(in) :: maxv !! maximum value.
    integer(int32)                    :: r    !! 32 bit real random number.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: temp
    !/ -----------------------------------------------------------------------------------
    call random_number(temp)
    r = floor( temp * real(maxv,dp), kind=int32 ) + 1
    if ( r.lt.1 )   r = 1
    if ( r.gt.maxv) r = maxv
  end function uniform_32_bit_integer


  !/ =====================================================================================
  function uniform_16_bit_integer( dts, maxv ) result( r )
    !/ -----------------------------------------------------------------------------------
    !! Get random number 16 bit integer. 1.le.r.le.max
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(entropy_source), intent(in) :: dts  !! reference this entropy_source
    integer(int16),        intent(in) :: maxv !! maximum value.
    integer(int16)                    :: r    !! 32 bit real random number.
    !/ -----------------------------------------------------------------------------------
    real(sp) :: temp
    !/ -----------------------------------------------------------------------------------
    call random_number(temp)
    r = floor( temp * real(maxv,sp), kind=int16 ) + 1_int16
    if ( r.lt.1_int16 )   r = 1_int16
    if ( r.gt.maxv) r = maxv
  end function uniform_16_bit_integer


end module entropy_mod


!/ =======================================================================================
!/ **                               E N T R O P Y _ M O D                               **
!/ =========================================================================== END FILE ==
