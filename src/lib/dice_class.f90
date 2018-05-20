!/ ====================================================================== BEGIN FILE =====
!/ **                                D I C E _ C L A S S                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 1990-2018, Stephen W. Soliday                                      **
!/ **                           stephen.soliday@trncmp.org                              **
!/ **                           http://research.trncmp.org                              **
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
module dice_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2015-10-22
  !! license: GPL
  !!
  !!####Revised
  !! * 1990-02-17 Original release.
  !! * 2009-02-07 Separated out entropy source.
  !! * 2016-10-11 Major revision.
  !! * 2018-04-03 Port to the Europa Fortran library.
  !!
  !! Provides the interface and procedures for random number utilities.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use entropy_class
  use tlogger
  implicit none

  
  public :: dice_getInstance
  public :: dice_getUniqueInstance


  !/ =====================================================================================
  type, public :: Dice
     !/ ----------------------------------------------------------------------------------
     class(Entropy), private, pointer :: engine     => null()
     logical                          :: have_spare = .false.
     real(dp)                         :: rand1      =  N_ZERO
     real(dp)                         :: rand2      =  N_ZERO

   contains

     procedure, public :: seedSize => dice_seed_size
     procedure, public :: uniformf => dice_uniform_R32
     procedure, public :: normal   => dice_normal_R64
     procedure, public :: boolean  => dice_boolean

     procedure :: dice_uniform_I16
     procedure :: dice_uniform_I32
     procedure :: dice_uniform_I64
     procedure :: dice_uniform_R64

     procedure :: dice_set_seed_void
     procedure :: dice_set_seed_I08
     procedure :: dice_set_seed_I16
     procedure :: dice_set_seed_I32
     
     procedure :: dice_uniform_R64_1d_array
     procedure :: dice_uniform_R64_2d_array
     procedure :: dice_normal_R64_1d_array
     procedure :: dice_normal_R64_2d_array

     generic, public :: seedSet     => dice_set_seed_void, &
          &                            dice_set_seed_I08,  &
          &                            dice_set_seed_I16,  &
          &                            dice_set_seed_I32

     generic, public :: uniform     => dice_uniform_I16, &
          &                            dice_uniform_I32, &
          &                            dice_uniform_I64, &
          &                            dice_uniform_R64

     generic, public :: set_uniform => dice_uniform_R64_1d_array,  &
          &                            dice_uniform_R64_2d_array

     generic, public :: set_normal  => dice_normal_R64_1d_array,   &
          &                            dice_normal_R64_2d_array

     final :: dice_destroy

  end type Dice

  class(Dice), pointer :: dice_singleton_instance => null()




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  function dice_getInstance() result( dd )
    !/ -----------------------------------------------------------------------------------
    !! Singleton Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), pointer :: dd !! pointer to a singlton Dice class.
    !/ -----------------------------------------------------------------------------------

    if ( .not.associated( dice_singleton_instance ) ) then
       allocate( dice_singleton_instance )
       call log_info( 'Create Common Dice' )
    else
       call log_info( 'Use Existing Dice' )
    end if

    dd => dice_singleton_instance

  end function dice_getInstance


  !/ =====================================================================================
  function dice_getUniqueInstance() result( dd )
    !/ -----------------------------------------------------------------------------------
    !! Unique Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), pointer :: dd !! pointer to a unique Dice class.
    !/ -----------------------------------------------------------------------------------
    allocate( dd )
    call log_info( 'Create Unique Dice' )
  end function dice_getUniqueInstance


  !/ =====================================================================================
  subroutine dice_destroy( dd )
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Dice), intent(inout) :: dd !! reference to a Dice object.
    !/ -----------------------------------------------------------------------------------

  end subroutine dice_destroy








  !/ =====================================================================================
  function dice_seed_size( self ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return the number of integers required to establish a unique state.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: self !! reference to this class.
    integer                    :: x    !! number of seed elements.
    !/ -----------------------------------------------------------------------------------

    x = self%engine%seedSize()

  end function dice_seed_size


  !/ =====================================================================================
  subroutine dice_set_seed_void( self )
    !/ -----------------------------------------------------------------------------------
    !! Set the seed from system entropy.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: self !! reference to this class.
    !/ -----------------------------------------------------------------------------------

    call self%engine%seedSet

  end subroutine dice_set_seed_void


  !/ =====================================================================================
  subroutine dice_set_seed_I08( self, seed )
    !/ -----------------------------------------------------------------------------------
    !! Set the entropy state using the seed source.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),                 intent(inout) :: self !! reference to this class.
    integer(int8), dimension(:), intent(in)    :: seed !! seed source.
    !/ -----------------------------------------------------------------------------------

    call self%engine%seedSet( seed )

  end subroutine dice_set_seed_I08


  !/ =====================================================================================
  subroutine dice_set_seed_I16( self, seed )
    !/ -----------------------------------------------------------------------------------
    !! Set the entropy state using the seed source.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),                  intent(inout) :: self !! reference to this class.
    integer(int16), dimension(:), intent(in)    :: seed !! seed source.
    !/ -----------------------------------------------------------------------------------

    call self%engine%seedSet( seed )

  end subroutine dice_set_seed_I16


  !/ =====================================================================================
  subroutine dice_set_seed_I32( self, seed )
    !/ -----------------------------------------------------------------------------------
    !! Set the entropy state using the seed source.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),                  intent(inout) :: self !! reference to this class.
    integer(int32), dimension(:), intent(in)    :: seed !! seed source.
    !/ -----------------------------------------------------------------------------------

    call self%engine%seedSet( seed )

  end subroutine dice_set_seed_I32








  !/ =====================================================================================
  function dice_boolean( self, threshold ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a true/false based on the threshold. By default it is 1/2 providing just as
    !! many trues and falses. If threshold is set to 0.3 then 3 times out of 10 this will
    !! return a true and 7 times out of 10 a false will be returned. If the threshold is
    !! 0.75 then 3 times out of 4 this will return a true and 1 time out of 4 a false.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),        intent(inout) :: self      !! reference to this class.
    real(dp), optional, intent(in)    :: threshold !! divide between true/false.
    logical                           :: x         !! true if uniform random is less than
    !!                                                the threshold, otherwise return false.
    !!                                                true = [0,threshold), and
    !!                                                false = [threshold,1)
    !/ -----------------------------------------------------------------------------------

    x = ( self%engine%getReal().lt.threshold )

  end function dice_boolean








  !/ =====================================================================================
  function dice_uniform_I16( self, exclusive_max ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a uniformly distributed integer with the range [0,exclusive_max)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: self          !! reference to this class.
    integer(int16), intent(in)    :: exclusive_max !!  exclusive_max maximum value.
    integer(int16)                :: x             !! uniformly distributed integer.
    !/ -----------------------------------------------------------------------------------

    x = int( mod( self%engine%getInteger(), int( exclusive_max, int32 ) ), int16 )

  end function dice_uniform_I16


  !/ =====================================================================================
  function dice_uniform_I32( self, exclusive_max ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a uniformly distributed integer with the range [0,exclusive_max)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: self          !! reference to this class.
    integer(int32), intent(in)    :: exclusive_max !! maximum value.
    integer(int32)                :: x             !! a uniformly distributed integer.
    !/ -----------------------------------------------------------------------------------

    x = mod( self%engine%getInteger(), exclusive_max )

  end function dice_uniform_I32


  !/ =====================================================================================
  function dice_uniform_I64( self, exclusive_max ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a uniformly distributed integer with the range [0,exclusive_max)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),    intent(inout) :: self          !! reference to this class.
    integer(int64), intent(in)    :: exclusive_max !! max maximum value.
    integer(int64)                :: x             !! uniformly distributed integer.
    !/ -----------------------------------------------------------------------------------

    call log_critical( "No I64 dice yet" )

    x = mod( int( self%engine%getInteger(), int64 ), exclusive_max )

  end function dice_uniform_I64








  !/ =====================================================================================
  function dice_uniform_R32( self ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a uniformly distributed real with the range [0,1)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: self !! reference to this class.
    real(sp)                   :: x    !! uniformly distributed real.
    !/ -----------------------------------------------------------------------------------

    x = real( self%engine%getReal(), sp )

  end function dice_uniform_R32


  !/ =====================================================================================
  function dice_uniform_R64( self ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Return a uniformly distributed real with the range [0,1)
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: self !! reference to this class.
    real(dp)                   :: x    !! uniformly distributed real.
    !/ -----------------------------------------------------------------------------------

    x = self%engine%getReal()

  end function dice_uniform_R64


  !/ =====================================================================================
  subroutine dice_uniform_R64_1d_array( self, buffer )
    !/ -----------------------------------------------------------------------------------
    !! Load an array with uniformally distributed samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),            intent(inout) :: self   !! reference to this class.
    real(dp), dimension(:), intent(inout) :: buffer !! pointer to a receiving buffer.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size( buffer, DIM=1 )

    do i=1,n
       buffer(i) = self%engine%getReal()
    end do

  end subroutine dice_uniform_R64_1d_array


  !/ =====================================================================================
  subroutine dice_uniform_R64_2d_array( self, buffer )
    !/ -----------------------------------------------------------------------------------
    !! Load a two dimensional array with uniformally distributed samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),              intent(inout) :: self   !! reference to this class.
    real(dp), dimension(:,:), intent(inout) :: buffer !! pointer to a receiving buffer.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n, m
    !/ -----------------------------------------------------------------------------------
    n = size( buffer, DIM=1 )    
    m = size( buffer, DIM=2 )    

    do j=1,m
       do i=1,n
          buffer(i,j) = self%engine%getReal()
       end do
    end do

  end subroutine dice_uniform_R64_2d_array








  !/ =====================================================================================
  function dice_normal_R64( self ) result( x )
    !/ -----------------------------------------------------------------------------------
    !! Use Box-Muller algorithm to generate numbers with a normal distribution.
    !! The mean is 0.0, the standard deviation is 1.0 and limits +/- 6.15 * StdDev,
    !! based on experimental results of rolling 1 trillion values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice), intent(inout) :: self !! reference to this class.
    real(dp)                   :: x    !! number with a normal distribution.
    !/ -----------------------------------------------------------------------------------

    if ( self%have_spare ) then
       self%have_spare = .false.
       x = sqrt(self%rand1) * sin(self%rand2)
    else

       self%have_spare = .true.

       self%rand1 = self%engine%getReal()

       if ( self%rand1.lt.1d-100 ) then
          self%rand1 = 1d-100
       end if

       self%rand1 = -N_TWO * log(self%rand1)
       self%rand2 =  N_2PI * self%engine%getReal()

       x = sqrt(self%rand1) * cos(self%rand2)

    end if

  end function dice_normal_R64


  !/ =====================================================================================
  subroutine dice_normal_R64_1d_array( self, buffer )
    !/ -----------------------------------------------------------------------------------
    !! Load an array with normally distributed samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),            intent(inout) :: self   !! reference to this class.
    real(dp), dimension(:), intent(inout) :: buffer !! pointer to a receiving buffer.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size( buffer, DIM=1 )    

    do i=1,n
       buffer(i) = self%engine%getReal()
    end do

  end subroutine dice_normal_R64_1d_array


  !/ =====================================================================================
  subroutine dice_normal_R64_2d_array( self, buffer )
    !/ -----------------------------------------------------------------------------------
    !! Load a two dimensional array with normally distributed samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Dice),              intent(inout) :: self   !! reference to this class.
    real(dp), dimension(:,:), intent(inout) :: buffer !! pointer to a receiving buffer.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n, m
    !/ -----------------------------------------------------------------------------------
    n = size( buffer, DIM=1 )    
    m = size( buffer, DIM=2 )    

    do j=1,m
       do i=1,n
          buffer(i,j) = self%engine%getReal()
       end do
    end do

  end subroutine dice_normal_R64_2d_array


end module dice_class


!/ =======================================================================================
!/ **                                D I C E _ C L A S S                                **
!/ =========================================================================== END FILE ==
