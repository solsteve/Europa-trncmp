!/ ====================================================================== BEGIN FILE =====
!/ **                       E N T R O P Y _ S E E D _ S O U R C E                       **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015, Stephen W. Soliday                                           **
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
module entropy_seed_source
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2015-10-22
  !! license: GPL
  !!
  !! This module fills an integer array with a sequence of pseudo random numbers.
  !! This module attempts to access the UNIX standard entropy source /dev/urandom.
  !! If that fails it utilizes the F95 intrinsic procedures for system clock and
  !! for date and time. If all three of these fail the module will exit to the
  !! operating system with an error.
  !!
  !! @note
  !! This module is intended to be used to uniquely seed a proper PRNG, and is not
  !! sufficient to be used as a PRNG itself.
  !!
  !! @todo
  !! Add more entropy sources, and fail with a fixed seed on the LCG.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  
  public :: urandom
  public :: mix_seed_matter
  public :: bit_transfer

  
  integer, parameter :: seed_kind = int32

  
  !/ -------------------------------------------------------------------------------------
  interface urandom
     !/ ----------------------------------------------------------------------------------
     module procedure :: urandom_I08
     module procedure :: urandom_I16
     module procedure :: urandom_I32
     module procedure :: urandom_I64
  end interface urandom


  !/ -------------------------------------------------------------------------------------
  interface mix_seed_matter
     !/ ----------------------------------------------------------------------------------
     module procedure :: mix_seed_matter_I08
     module procedure :: mix_seed_matter_I16
     module procedure :: mix_seed_matter_I32
     module procedure :: mix_seed_matter_I64
  end interface mix_seed_matter

  
  !/ -------------------------------------------------------------------------------------
  interface bit_transfer
     !/ ----------------------------------------------------------------------------------
     module procedure :: bit_transfer_8to32
     module procedure :: bit_transfer_16to32
  end interface bit_transfer




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine bit_transfer_8to32( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Tranfer an 8 bit array into a 32 bit array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int32), dimension(:), intent(inout) :: dst !! reference to a 32 bit destination array.
    integer(int8),  dimension(:), intent(inout) :: src !! reference to an 8 bit source array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n
    !/ -----------------------------------------------------------------------------------

    n = size(dst)
    
    if ( n*4 .eq. size(src) ) then
       j = 1
       do i=1,n
          dst(i) = (src(j)*16777216) + (src(j+1)*65536) + (src(j+2)*256) + src(j+3)
          j = j + 4
       end do
    else
       write(ERROR_UNIT,*) 'bit_transfer_8to32 size mismatch'
    end if
  end subroutine bit_transfer_8to32


  !/ =====================================================================================
  subroutine bit_transfer_16to32( dst, src )
    !/ -----------------------------------------------------------------------------------
    !! Transfer a 16 bit array into a 32 bit array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int32), dimension(:), intent(inout) :: dst !! reference to a 32 bit destination array.
    integer(int16), dimension(:), intent(inout) :: src !! reference to a 16 bit source array.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, n
    !/ -----------------------------------------------------------------------------------

    n = size(dst)
    
    if ( n*2 .eq. size(src) ) then
       j = 1
       do i=1,n
          dst(i) = (src(j)*65536) + src(j+1)
          j = j + 2
       end do
    else
       write(ERROR_UNIT,*) 'bit_transfer_16to32 size mismatch'
    end if
  end subroutine bit_transfer_16to32




  !/ =====================================================================================
  function urandom_file( ier, ERR ) result( un )
    !/ -----------------------------------------------------------------------------------
    !! URandom File.
    !! @note
    !! It is the responsibility of the programmer to close this unit.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer,           intent(out) :: ier !! error reporting.
    integer, optional, intent(out) :: ERR !! optional copy.
    integer                        :: un  !! unit for the /dev/urandom file.
    !/ -----------------------------------------------------------------------------------
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    open(  newunit=un, file="/dev/urandom", access="stream", form="unformatted", &
         & action="read", status="old", iostat=ier )

    if ( 0.ne.ier ) then
       if ( report ) then
          write( ERROR_UNIT, * ) 'cannot open/find /dev/urandom'
       end if
    end if

    if ( present( ERR ) ) ERR = ier

  end function urandom_file


  !/ =====================================================================================
  subroutine urandom_I08( array, ERR )
    !/ -----------------------------------------------------------------------------------
    !! Load the destination array with entropy from /dev/urandom.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int8), dimension(:), intent(inout) :: array !! reference to a destination array.
    integer, optional,           intent(out)   :: ERR   !! optional error reporting.
    !/ -----------------------------------------------------------------------------------
    integer i, ier, inf, n
    !/ -----------------------------------------------------------------------------------

    inf = urandom_file( ier, ERR )

    if ( 0.eq.ier ) then

       n = size( array, DIM=1 )

       read( inf ) (array(i),i=1,n)

       close( inf )

    end if

  end subroutine urandom_I08


  !/ =====================================================================================
  subroutine urandom_I16( array, ERR )
    !/ -----------------------------------------------------------------------------------
    !! Load the destination array with entropy from /dev/urandom.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int16), dimension(:), intent(inout) :: array !! reference to a destination array.
    integer, optional,            intent(out)   :: ERR   !! optional error reporting.
    !/ -----------------------------------------------------------------------------------
    integer i, ier, inf, n
    !/ -----------------------------------------------------------------------------------

    inf = urandom_file( ier, ERR )

    if ( 0.eq.ier ) then

       n = size( array, DIM=1 )

       read( inf ) (array(i),i=1,n)

       close( inf )

    end if

  end subroutine urandom_I16


  !/ =====================================================================================
  subroutine urandom_I32( array, ERR )
    !/ -----------------------------------------------------------------------------------
    !! Load the destination array with entropy from /dev/urandom.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int32), dimension(:), intent(inout) :: array !! reference to a destination array.
    integer, optional,            intent(out)   :: ERR   !! optional error reporting.
    !/ -----------------------------------------------------------------------------------
    integer i, ier, inf, n
    !/ -----------------------------------------------------------------------------------

    inf = urandom_file( ier, ERR )

    if ( 0.eq.ier ) then

       n = size( array, DIM=1 )

       read( inf ) (array(i),i=1,n)

       close( inf )

    end if

  end subroutine urandom_I32


  !/ =====================================================================================
  subroutine urandom_I64( array, ERR )
    !/ -----------------------------------------------------------------------------------
    !!! Load the destination array with entropy from /dev/urandom.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int64), dimension(:), intent(inout) :: array !! reference to a destination array.
    integer, optional,            intent(out)   :: ERR   !! optional error reporting.
    !/ -----------------------------------------------------------------------------------
    integer i, ier, inf, n
    !/ -----------------------------------------------------------------------------------

    inf = urandom_file( ier, ERR )

    if ( 0.eq.ier ) then

       n = size( array, DIM=1 )

       read( inf ) (array(i),i=1,n)

       close( inf )

    end if

  end subroutine urandom_I64




  !/ =====================================================================================
  subroutine mix_seed_matter_I08( dst, src )
    !/ ----------------------------------------------------------------------------------
    !! Mix seed matter.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int8), dimension(:), intent(inout) :: dst !! reference to a destination array.
    integer(int8), dimension(:), intent(in)    :: src !! reference to a source array.
    !/ ----------------------------------------------------------------------------------
    integer :: i, j, nd, ns
    !/ ----------------------------------------------------------------------------------

    nd = size( dst, DIM=1 )
    ns = size( src, DIM=1 )

    if ( nd.gt.ns ) then                       ! source has too few bytes.
       do i=1,nd
          j = mod(i, ns) + 1
          dst(i) = src(j)
       end do
    else
       if ( ns.gt.nd ) then                    ! source has too many bytes.
          do i=1,nd
             dst(i) = src(i)
          end do
          do i=nd+1,ns
             j = mod(i, nd) + 1
             dst(j) = xor( dst(j), src(i) )
          end do
       else                                    ! source is the same size as the destination.
          do i=1,nd
             dst(i) = src(i)
          end do
       end if
    end if

  end subroutine mix_seed_matter_I08


  !/ =====================================================================================
  subroutine mix_seed_matter_I16( dst, src )
    !/ ----------------------------------------------------------------------------------
    !! Mix seed matter.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int16), dimension(:), intent(inout) :: dst !! reference to a destination array.
    integer(int16), dimension(:), intent(in)    :: src !! reference to a source array.
    !/ ----------------------------------------------------------------------------------
    integer :: i, j, nd, ns
    !/ ----------------------------------------------------------------------------------

    nd = size( dst, DIM=1 )
    ns = size( src, DIM=1 )

    if ( nd.gt.ns ) then                       ! source has too few bytes.
       do i=1,nd
          j = mod(i, ns) + 1
          dst(i) = src(j)
       end do
    else
       if ( ns.gt.nd ) then                    ! source has too many bytes.
          do i=1,nd
             dst(i) = src(i)
          end do
          do i=nd+1,ns
             j = mod(i, nd) + 1
             dst(j) = xor( dst(j), src(i) )
          end do
       else                                    ! source is the same size as the destination.
          do i=1,nd
             dst(i) = src(i)
          end do
       end if
    end if

  end subroutine mix_seed_matter_I16


  !/ =====================================================================================
  !/ -------------------------------------------------------------------------------------
  subroutine mix_seed_matter_I32( dst, src )
    !/ ----------------------------------------------------------------------------------
    !! Mix seed matter.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer(int32), dimension(:), intent(inout) :: dst !! reference to a destination array.
    integer(int32), dimension(:), intent(in)    :: src !! reference to a source array.
    !/ ----------------------------------------------------------------------------------
    integer :: i, j, nd, ns
    !/ ----------------------------------------------------------------------------------

    nd = size( dst, DIM=1 )
    ns = size( src, DIM=1 )

    if ( nd.gt.ns ) then                       ! source has too few bytes.
       do i=1,nd
          j = mod(i, ns) + 1
          dst(i) = src(j)
       end do
    else
       if ( ns.gt.nd ) then                    ! source has too many bytes.
          do i=1,nd
             dst(i) = src(i)
          end do
          do i=nd+1,ns
             j = mod(i, nd) + 1
             dst(j) = xor( dst(j), src(i) )
          end do
       else                                    ! source is the same size as the destination.
          do i=1,nd
             dst(i) = src(i)
          end do
       end if
    end if

  end subroutine mix_seed_matter_I32


  !/ =====================================================================================
  subroutine mix_seed_matter_I64( dst, src )
    !/ ----------------------------------------------------------------------------------
    implicit none
    integer(int64), dimension(:), intent(inout) :: dst !! reference to a destination array.
    integer(int64), dimension(:), intent(in)    :: src !! reference to a source array.
    !/ ----------------------------------------------------------------------------------
    !! Mix seed matter.
    !/ -----------------------------------------------------------------------------------
    integer :: i, j, nd, ns
    !/ ----------------------------------------------------------------------------------

    nd = size( dst, DIM=1 )
    ns = size( src, DIM=1 )

    if ( nd.gt.ns ) then                       ! source has too few bytes.
       do i=1,nd
          j = mod(i, ns) + 1
          dst(i) = src(j)
       end do
    else
       if ( ns.gt.nd ) then                    ! source has too many bytes.
          do i=1,nd
             dst(i) = src(i)
          end do
          do i=nd+1,ns
             j = mod(i, nd) + 1
             dst(j) = xor( dst(j), src(i) )
          end do
       else                                    ! source is the same size as the destination.
          do i=1,nd
             dst(i) = src(i)
          end do
       end if
    end if

  end subroutine mix_seed_matter_I64


end module entropy_seed_source


!/ =======================================================================================
!/ **                       E N T R O P Y _ S E E D _ S O U R C E                       **
!/ =========================================================================== END FILE ==
