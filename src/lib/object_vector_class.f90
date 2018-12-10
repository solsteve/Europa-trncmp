!/ ====================================================================== BEGIN FILE =====
!/ **                      C O N F I G _ S E C T I O N _ C L A S S                      **
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
!/ =======================================================================================
module object_vector_class
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-11-24
  !! license: GPL
  !!
  !! Provides a Variable size array
  !!
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none
  private

  integer, parameter :: INIT_OARR_SIZE  = 16
  integer, parameter :: INCR_BLOCK_SIZE = 16

  
  !/ =====================================================================================
  type, public :: object_vector_t
     !/ ----------------------------------------------------------------------------------

     class(object_pointer), pointer :: vector(:)     => null() !! array of object pointers.
     integer                        :: current_index = 1       !! first availble empty index.
     integer                        :: max_index     = 1       !! maximum availble empty indices.

   contains

     procedure :: resize => oarr_resize

     procedure, public  :: clear  => oarr_clear
     procedure, public  :: get    => oarr_get_object
     procedure, public  :: set    => oarr_set_object
     procedure, public  :: append => oarr_append_object

     final :: oarr_destroy
     
  end type object_vector_t

  
  !/ -------------------------------------------------------------------------------------
  interface create
     !/ ----------------------------------------------------------------------------------
     module procedure :: oarr_create
  end interface

  
  !/ -------------------------------------------------------------------------------------
  interface ObjectVector
     !/ ----------------------------------------------------------------------------------
     module procedure :: oarr_allocate
  end interface

  
  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: oarr_get_size
  end interface


public :: create
public :: ObjectVector
public :: size
  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  subroutine oarr_create( ar, INIT )
    !/ -----------------------------------------------------------------------------------
    !! Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(object_vector_t), intent(inout) :: ar
    integer, optional,     intent(in)    :: INIT
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = INIT_OARR_SIZE

    if ( present(INIT) ) then
       n = INIT
    end if
    
    allocate( ar%vector( n ) )
    ar%current_index = 0
    ar%max_index     = n

    do i=1,n
       ar%vector(i)%ptr => null()
    end do
    
  end subroutine oarr_create

  
  !/ =====================================================================================
  function oarr_allocate( INIT ) result( ptr )
    !/ -----------------------------------------------------------------------------------
    !! Constructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), pointer :: ptr
    integer, optional,  intent(in) :: INIT
    !/ -----------------------------------------------------------------------------------

    allocate( ptr )
    call create( ptr, INIT )
    
  end function oarr_allocate


  !/ =====================================================================================
  subroutine oarr_destroy( ar)
    !/ -----------------------------------------------------------------------------------
    !! Destroy allocations.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(object_vector_t), intent(inout) :: ar !! reference to an object vector.
    !/ -----------------------------------------------------------------------------------

    call ar%clear
    deallocate( ar%vector )
    ar%current_index = 0
    ar%max_index     = 0

  end subroutine oarr_destroy


  !/ =====================================================================================
  function oarr_get_size( ar ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Size.
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                          :: n  !! number of elements stored in this vector.
    type(object_vector_t), intent(in) :: ar !! reference to an object vector.
    !/ -----------------------------------------------------------------------------------

    n = ar%current_index
    
  end function oarr_get_size

  
  !/ =====================================================================================
  subroutine oarr_resize( self, new_max )
    !/ -----------------------------------------------------------------------------------
    !! Resize this vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), intent(inout) :: self    !! reference to this object vector.
    integer,               intent(in)    :: new_max !! new maximum vector size.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    class(object_pointer), pointer :: temp(:) => null()
    !/ -----------------------------------------------------------------------------------
    
    if ( new_max.gt.self%max_index ) then
       temp       => self%vector
       self%vector => null()
       allocate( self%vector(new_max) )
       do i=1,new_max
          if ( i.gt.self%max_index ) then
             self%vector(i)%ptr => null()
          else
             self%vector(i)%ptr => temp(i)%ptr
             temp(i)%ptr       => null()
          end if
       end do
       self%max_index = new_max
    end if
    
  end subroutine oarr_resize

  
  !/ =====================================================================================
  subroutine oarr_clear( self )
    !/ -----------------------------------------------------------------------------------
    !! Remove all elements.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), intent(inout) :: self !! reference to this object vector.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    
    do i=1,self%max_index
       if ( associated( self%vector(i)%ptr ) ) then
          deallocate( self%vector(i)%ptr )
          nullify( self%vector(i)%ptr )
       end if
    end do

    self%current_index = 0
    
  end subroutine oarr_clear

  
  !/ =====================================================================================
  function oarr_get_object( self, idx ) result( obj )
    !/ -----------------------------------------------------------------------------------
    !! Retrive an object.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), intent(inout) :: self !! reference to this object vector.
    integer,               intent(in)    :: idx  !! index for storage retrieval.
    class(*), pointer                    :: obj  !! pointer to the indexed object.
    !/ -----------------------------------------------------------------------------------

    obj => null()

    if ( idx.le.self%max_index ) then
       obj => self%vector(idx)%ptr
    end if
    
  end function oarr_get_object

  
  !/ =====================================================================================
  subroutine oarr_set_object( self, idx, obj )
    !/ -----------------------------------------------------------------------------------
    !! Insert an object in this vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), intent(inout) :: self !! reference to this object vector.
    integer,               intent(in)    :: idx  !! index for storage.
    class(*), pointer,     intent(in)    :: obj  !! pointer to an object for storage.
    !/ -----------------------------------------------------------------------------------

    if ( idx.gt.self%max_index ) then
       call self%resize( idx+INCR_BLOCK_SIZE )
    end if

    if ( idx.gt.self%current_index ) then
       self%current_index = idx
    end if

    self%vector( idx )%ptr => obj

  end subroutine oarr_set_object

  
  !/ =====================================================================================
  subroutine oarr_append_object( self, obj )
    !/ -----------------------------------------------------------------------------------
    !! Append an object to the next available location in this object vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), intent(inout) :: self !! reference to this object vector.
    class(*), pointer,     intent(in)    :: obj  !! pointer to an object for storage.
    !/ -----------------------------------------------------------------------------------

    self%current_index = self%current_index + 1

    if ( self%current_index.gt.self%max_index ) then
       call self%resize( 2*self%max_index )
    end if

    self%vector( self%current_index )%ptr => obj
    
  end subroutine oarr_append_object


end module object_vector_class

!/ =======================================================================================
!/ **                      C O N F I G _ S E C T I O N _ C L A S S                      **
!/ =========================================================================== END FILE ==
