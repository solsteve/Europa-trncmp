!/ ====================================================================== BEGIN FILE =====
!/ **                          F T E S T _ F I N A L _ T E S T                          **
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
module ftest_final_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-01-22
  !! license: GPL
  !!
  !!##Test of .
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private
 public :: ftest, size, init

  !/ =====================================================================================
  type :: ftest
     !/ ----------------------------------------------------------------------------------

     integer                            :: num = 0
     integer, allocatable, dimension(:) :: data

   contains

     procedure, public :: get  => ftest_get
     procedure, public :: set  => ftest_set

     final :: ftest_destroy

  end type ftest

  interface init
     module procedure :: ftest_init
  end interface init

  interface size
     module procedure :: ftest_size
  end interface size

  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine ftest_destroy( ft )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ftest), intent(inout) :: ft
    !/ -----------------------------------------------------------------------------------
    if ( allocated( ft%data ) ) then
       write (*,*) '  final: deallocate'
       deallocate( ft%data )
    end if
    ft%num = 0    
  end subroutine ftest_destroy

  
  !/ =====================================================================================
  subroutine ftest_init( ft, n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ftest), intent(inout) :: ft
    integer,     intent(in)    :: n
    !/ -----------------------------------------------------------------------------------
    if ( allocated( ft%data ) ) then
       write (*,*) '  init: deallocate'
       deallocate( ft%data )
    end if
    write (*,*) '  init: allocate'
    allocate( ft%data(n) )
    ft%num = n    
  end subroutine ftest_init


  !/ =====================================================================================
  function ftest_size( ft ) result( n )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer                    :: n
    type(ftest), intent(inout) :: ft
    !/ -----------------------------------------------------------------------------------
    n = ft%num
  end function ftest_size


  !/ =====================================================================================
  function ftest_get( self, i ) result( v )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer             :: v
    class(ftest), intent(inout) :: self
    integer,      intent(in)    :: i
    !/ -----------------------------------------------------------------------------------
    v = self%data(i)
  end function ftest_get


  !/ =====================================================================================
  subroutine ftest_set( self, i, v )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ftest), intent(inout) :: self
    integer,      intent(in)    :: i
    integer,      intent(in)    :: v
    !/ -----------------------------------------------------------------------------------
    self%data(i) = v
  end subroutine ftest_set


end module ftest_final_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_final_test
  implicit none

  type(ftest), pointer :: ft => null()

  write(*,*) 'main: allocate'
  allocate( ft )

  write(*,*) 'main: init'
  call init( ft, 100 )

  write(*,*) size(ft)

  write(*,*) 'main: deallocate'
  deallocate( ft )

  write(*,*) 'main: nullify'
  nullify( ft )

  
end program main

!/ =======================================================================================
!/ **                          F T E S T _ F I N A L _ T E S T                          **
!/ =========================================================================== END FILE ==
