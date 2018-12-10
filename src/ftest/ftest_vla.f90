!/ ====================================================================== BEGIN FILE =====
!/ **                                 F T E S T _ V L A                                 **
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
module ftest_vla_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-12-02
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use object_vector_class
  implicit none


  type :: test_obj
     integer  :: idn = 0
     real(dp) :: val = 1.0d0
  end type test_obj
  


  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  function create_tobj( i, v ) result( obj )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(test_obj), pointer :: obj
    integer,  intent(in) :: i
    real(dp), intent(in) :: v
    !/ -----------------------------------------------------------------------------------

    allocate( obj )
    obj%idn = i
    obj%val = v

  end function create_tobj


  !/ =====================================================================================
  subroutine test02
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(object_vector_t), pointer :: OA
    class(*),              pointer :: temp 
    integer :: i, n, m, k
    !/ -----------------------------------------------------------------------------------

    n = 3
    m = 7

    OA => ObjectVector(INIT=n)

    write(*,*) size(OA)

    do i=1,n-1
       temp => create_tobj( i, real(i*37,dp) )
       call OA%set(i, temp);
    end do

    k = size(OA)
    
    write(*,*) size(OA)

  end subroutine test02
  
  !/ =====================================================================================
  subroutine test01
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, allocatable :: A(:)
    integer, allocatable :: B(:)
    integer :: i, n, m
    !/ -----------------------------------------------------------------------------------

    n = 3
    m = 7
    
    allocate( A(n) )

    write(*,*) A

    do i=1,n
       A(i) = i
    end do

    write(*,*) A

    allocate( B(m) )

    write(*,*) B

    B(1:n) = A

    write(*,*) B

  end subroutine test01

end module ftest_vla_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_vla_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test01



end program main

!/ =======================================================================================
!/ **                                 F T E S T _ V L A                                 **
!/ =========================================================================== END FILE ==
