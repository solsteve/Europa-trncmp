!/ ====================================================================== BEGIN FILE =====
!/ **                           F U Z Z Y _ G R O U P _ M O D                           **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2015-2020 Stephen W. Soliday                                       **
!/ **                          stephen.soliday@trncmp.org                               **
!/ **                          http://research.trncmp.org                               **
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
module fuzzy_group_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a collection of Fuzzy Partitions
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-01-05
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use fuzzy_partition_mod
  use string_tools
  use tlogger
  implicit none

  !/ =====================================================================================
  type :: FuzzyGroup
     !/ ----------------------------------------------------------------------------------

     integer                               :: num_part = 0 !! number of partitions.
     type(FuzzyPartition_ptr), allocatable :: fpart(:)     !! array of fuzzy partitions.

   contains

     procedure, private :: fg_init_default
     procedure, private :: fg_init_bound
     procedure, private :: fg_init_parts
     procedure, private :: fg_init_params
     procedure, private :: fg_resize

     procedure :: part      => fg_get_part
     procedure :: nIn       => fg_get_number_inputs
     procedure :: nOut      => fg_get_number_outputs
     procedure :: size      => fg_get_storage_size

     procedure :: fuzzify   => fg_fuzzify
     procedure :: defuzzify => fg_defuzzify

     procedure :: load      => fg_load
     procedure :: store     => fg_store
     procedure :: read      => fg_read
     procedure :: write     => fg_write

     procedure :: destroy   => fg_destroy

     final :: fg_final

     generic :: init => fg_init_default, fg_init_bound, fg_init_parts, fg_init_params

  end type FuzzyGroup




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine fg_init_default( dts, n )
    !/ -----------------------------------------------------------------------------------
    !! Create a FuzzyGroup composed of n empty FuzzyPartitions.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts  !! reference to this FuzzyGroup.
    integer,           intent(in)    :: n    !! desired number of partitions.
    !/ -----------------------------------------------------------------------------------

    if ( n.gt.0 ) then
       call dts%fg_resize( n )
    else
       call log_error( 'FuzzyPartition%init must be 0 .lt. n:', I4=n )
    end if

  end subroutine fg_init_default


  !/ =====================================================================================
  subroutine fg_init_bound( dts, num_set, mnc, mxc )
    !/ -----------------------------------------------------------------------------------
    !! Create a FuzzyGroup composed of n empty FuzzyPartitions.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts        !! reference to this FuzzyGroup.
    integer,           intent(in)    :: num_set(:) !! desired number of set per partition.
    real(dp),          intent(in)    :: mnc(:)     !! minimum center for each partition.
    real(dp),          intent(in)    :: mxc(:)     !! minimum center for each partition.
    !/ -----------------------------------------------------------------------------------
    integer                        :: i, np
    class(FuzzyPartition), pointer :: ptemp
    !/ -----------------------------------------------------------------------------------

    np = size(num_set)

    if ( 1.gt.np ) then
       call log_error( 'FuzzyPartition%init must be 0 .lt. n:', I4=np )
    end if

    if ( np.gt.size(mnc) ) then
       call log_error( 'FuzzyGroup%init: too few min centers' )
       goto 999
    end if

    if ( np.gt.size(mxc) ) then
       call log_error( 'FuzzyGroup%init: too few max centers' )
       goto 999
    end if

    call dts%fg_resize(np)

    do i=1,np
       if ( associated( dts%fpart(i)%ptr ) ) then
          call dts%fpart(i)%ptr%init( num_set(i), mnc(i), mxc(i) )
       else
          dts%fpart(i)%ptr => FuzzyPartition( num_set(i), mnc(i), mxc(i) )
       end if
    end do

999 continue

  end subroutine fg_init_bound


  !/ =====================================================================================
  subroutine fg_init_parts( dts, list )
    !/ -----------------------------------------------------------------------------------
    !! Create a FuzzyGroup by cloning FuzzyPartitions from a list.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup),        intent(inout) :: dts     !! reference to this FuzzyGroup.
    type(FuzzyPartition_ptr), intent(in)    :: list(:) !! List of FuzzyPartitions.
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------
    n = size(list)
    call dts%fg_resize(n)
    do i=1,n
       if ( associated( dts%fpart(i)%ptr ) ) then
          call dts%fpart(i)%ptr%copy( list(i)%ptr )
       else
          dts%fpart(i)%ptr => list(i)%ptr%clone()
       end if
    end do

  end subroutine fg_init_parts


  !/ =====================================================================================
  subroutine fg_init_params( dts, num_set, buffer )
    !/ -----------------------------------------------------------------------------------
    !! Create a FuzzyGroup from a list of partition lengths and a buffer of centers.
    !
    !  If you wanted a group of three partiotions with the following layout
    !  __      __    __            __    __         __
    !    \ /\ /        \ /\ /\ /\ /        \ /\ /\ /
    !     X  X          X  X  X  X          X  X  X
    !  __/ \/ \__    __/ \/ \/ \/ \__    __/ \/ \/ \__
    !   -1 0  +1      2  3  4  5  6       -5 0  3  7
    ! 
    ! N(3)  = [3,5,4]
    ! B(12) = [-1.0, 0.0, 1.0, 2.0, 3.0, 4.0, 5.0, 6.0, -5.0, 0.0, 3.0, 7.0 ]
    !
    ! call P%init( N, B )
    !
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts        !! reference to this FuzzyGroup.
    integer,           intent(in)    :: num_set(:) !! number of sets in each FuzzyPartition
    real(dp),          intent(in)    :: buffer(:)  !! Overall list of centers
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, m
    integer :: i1, i2
    !/ -----------------------------------------------------------------------------------

    n = size(num_set)

    if ( 1.gt.n ) then
       call log_error( 'FuzzyGroup%init: no partitions specified' )
       goto 999
    end if

    m = 0
    do i=1,n
       m = m + num_set(i)
    end do

    if ( size(buffer).lt.m ) then
       call log_error( 'FuzzyGroup%init: buffer does not contain enough centers' )
       goto 999
    end if

    call dts%fg_resize( n )

    i1 = 1
    do i=1,n
       if ( 1.gt.num_set(i) ) then
          call log_error('FuzzyGroup%init: partiton has no sets. part:', I4=i)
          goto 999
       end if
       i2 = i1 + num_set(i) - 1
       if ( associated( dts%fpart(i)%ptr ) ) then
          call dts%fpart(i)%ptr%init( buffer(i1:i2) )
       else
          dts%fpart(i)%ptr => FuzzyPartition( buffer(i1:i2) )
       endif
       i1 = i2 + 1
    end do

999 continue

  end subroutine fg_init_params


  !/ =====================================================================================
  subroutine fg_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destroy the allocation for this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts !! reference to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( allocated( dts%fpart ) ) then
       do i=1,dts%num_part
          if ( associated( dts%fpart(i)%ptr ) ) then
             deallocate( dts%fpart(i)%ptr )
          end if
          nullify( dts%fpart(i)%ptr )
       end do
       deallocate( dts%fpart )
    end if
    dts%num_part = 0

  end subroutine fg_destroy


  !/ =====================================================================================
  subroutine fg_final( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FuzzyGroup), intent(inout) :: dts !! reference to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    call dts%destroy
  end subroutine fg_final


  !/ =====================================================================================
  subroutine fg_resize( dts, n, CHANGED )
    !/ -----------------------------------------------------------------------------------
    !! Alter the allocation for this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts     !! reference to this FuzzyGroup.
    integer,           intent(in)    :: n       !! desired number of FuzzyPartitions.
    logical, optional, intent(out)   :: CHANGED !! true if the number of sets changed.
    !/ -----------------------------------------------------------------------------------
    logical :: chg
    integer :: i
    !/ -----------------------------------------------------------------------------------

    chg = .false.

    if ( 1.gt.n ) then
       call log_error( 'FuzzyPartition%resize: There needs to be at least one function only got', I4=n )
       goto 999
    end if

    if ( n.eq.dts%num_part ) then
       call log_debug( 'FuzzyPartition%resize: Resize called with no change' )
       goto 999
    end if

    call dts%destroy

    allocate( dts%fpart(n) )

    chg = .true.

    dts%num_part = n

    do i=1,n
       dts%fpart(i)%ptr => null()
    end do

999 continue
    if ( present( CHANGED ) ) CHANGED = chg

  end subroutine fg_resize


  !/ =====================================================================================
  function fg_get_part( dts, idx ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Return a pointer to the indexed FuzzyPartition in this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup),     intent(inout) :: dts !! reference to this FuzzyGroup.
    integer,               intent(in)    :: idx !! partition index.
    class(FuzzyPartition), pointer       :: ptr !! pointer to the indexed FuzzyPartition.
    !/ -----------------------------------------------------------------------------------
    ptr => dts%fpart(idx)%ptr
  end function fg_get_part


  !/ =====================================================================================
  function fg_get_number_inputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of crisp inputs to this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts !! reference to this FuzzyGroup.
    integer                          :: n   !! number of inputs.
    !/ -----------------------------------------------------------------------------------
    n = dts%num_part
  end function fg_get_number_inputs


  !/ =====================================================================================
  function fg_get_number_outputs( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get the number of fuzzy membership outputs from this FuzzyGroup.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts !! reference to this FuzzyGroup.
    integer                          :: n   !! number of outputs.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    n = 0
    do i=1,dts%num_part
       if ( associated( dts%fpart(i)%ptr ) ) then
          n = n + dts%fpart(i)%ptr%nOut()
       end if
    end do
  end function fg_get_number_outputs


  !/ =====================================================================================
  function fg_get_storage_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts !! reference to this FuzzyGroup.
    integer                          :: n   !! number of parameters.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    n = 0
    do i=1,dts%num_part
       if ( associated( dts%fpart(i)%ptr ) ) then
          n = n + dts%fpart(i)%ptr%size()
       end if
    end do
  end function fg_get_storage_size


  !/ =====================================================================================
  subroutine fg_fuzzify( dts, mu, x )
    !/ -----------------------------------------------------------------------------------
    !! Compute the degree of membership for each FuzzySet based on a crisp value x.
    !! The domain is all real numbers. The range is 0 to 1 inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts   !! reference to this FuzzyGroup.
    real(dp),          intent(out)   :: mu(:) !! degree of membership list.
    real(dp),          intent(in)    :: x(:)  !! crisp value list.
    !/ -----------------------------------------------------------------------------------
    integer                        :: i, ms, me
    class(FuzzyPartition), pointer :: P
    !/ -----------------------------------------------------------------------------------
    ms = 1
    do i=1,dts%num_part
       P => dts%fpart(i)%ptr
       me = ms + P%nOut() - 1
       call P%mu( mu(ms:me), x(i) )
       ms = me + 1
    end do
  end subroutine fg_fuzzify


  !/ =====================================================================================
  subroutine fg_defuzzify( dts, x, mu )
    !/ -----------------------------------------------------------------------------------
    !! Compute the center of area based on the degrees of membership in this FuzzyPartition. 
    !! The domain is 0 to 1 inclusive. The range is (left) to (right) inclusive.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts    !! reference to this FuzzyGroup.
    real(dp),          intent(out)   :: x(:)   !! center of area list.
    real(dp),          intent(in)    :: mu(:)  !! degree of membership list.
    !/ -----------------------------------------------------------------------------------
    integer                        :: i, ms, me
    class(FuzzyPartition), pointer :: P
    !/ -----------------------------------------------------------------------------------
    ms = 1
    do i=1,dts%num_part
       P => dts%fpart(i)%ptr
       me = ms + P%nOut() - 1
       x(i) = P%coa( mu(ms:me) )
       ms = me + 1
    end do
  end subroutine fg_defuzzify


  !/ =====================================================================================
  function fg_load( dts, buffer, INDEX ) result( post_index )
    !/ -----------------------------------------------------------------------------------
    !! Load the parameters for this FuzzyGroup from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts        !! reference to this FuzzyGroup.
    real(dp),          intent(in)    :: buffer(:)  !! buffer for the parameters.
    integer, optional, intent(in)    :: INDEX      !! index of starting parameter.
    integer                          :: post_index !! next available index.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    post_index = 1
    if ( present( INDEX ) ) post_index = INDEX

    do i=1,dts%num_part
       post_index = dts%fpart(i)%ptr%load(buffer,post_index)
    end do

  end function fg_load


  !/ =====================================================================================
  function fg_store( dts, buffer, INDEX ) result( post_index )
    !/ -----------------------------------------------------------------------------------
    !! Store the parameters for this FuzzyGroup from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts        !! reference to this FuzzyGroup.
    real(dp),          intent(out)   :: buffer(:)  !! buffer for the parameters.
    integer, optional, intent(in)    :: INDEX      !! index of starting parameter.
    integer                          :: post_index !! next available index.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    post_index = 1
    if ( present( INDEX ) ) post_index = INDEX

    do i=1,dts%num_part
       post_index = dts%fpart(i)%ptr%store(buffer,post_index)
    end do

  end function fg_store


  !/ =====================================================================================
  subroutine fg_read( dts, un, IOSTAT, MAXINDEX )
    !/ -----------------------------------------------------------------------------------
    !! Read the parameters for this FuzzyGroup from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup), intent(inout) :: dts      !! reference to this FuzzyGroup.
    integer,           intent(in)    :: un       !! file unit.
    integer, optional, intent(out)   :: IOSTAT   !! error return status.
    integer, optional, intent(in)    :: MAXINDEX !! maximum parameters that can be read.
    !/ -----------------------------------------------------------------------------------
    integer                        :: i, n
    class(FuzzyPartition), pointer :: temp
    !/ -----------------------------------------------------------------------------------

    read( un, * ) n
    call dts%fg_init_default(n)
    do i=1,n
       allocate( temp )
       call temp%read( un, IOSTAT, MAXINDEX )
       if ( associated( dts%fpart(i)%ptr ) ) then
          call dts%fpart(i)%ptr%copy( temp )
          deallocate( temp )
       else
          dts%fpart(i)%ptr => temp
       end if
       temp => null()
    end do

  end subroutine fg_read


  !/ =====================================================================================
  subroutine fg_write( dts, un, FMT, IOSTAT )
    !/ -----------------------------------------------------------------------------------
    !! Write the parameters for this FuzzyGroup from a source array.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FuzzyGroup),      intent(inout) :: dts    !! reference to this FuzzyGroup.
    integer,                intent(in)    :: un     !! file unit.
    character(*), optional, intent(in)    :: FMT    !! edit descriptor.
    integer,      optional, intent(out)   :: IOSTAT !! error return status.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    write(un,'(I0)') dts%num_part
    do i=1,dts%num_part
       call dts%fpart(i)%ptr%write(un,FMT,IOSTAT)
    end do

  end subroutine fg_write


end module fuzzy_group_mod


!/ =======================================================================================
!/ **                           F U Z Z Y _ G R O U P _ M O D                           **
!/ =========================================================================== END FILE ==
