!/ ====================================================================== BEGIN FILE =====
!/ **                                T R N C M P _ E N V                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2017, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
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
module trncmp_env
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2017-03-30
  !! license: GPL
  !!
  !!## Tran-Comp Environment.
  !!
  !! Collection of definitions for common developmental environment
  !
  !/ -------------------------------------------------------------------------------------
  use constants_env
  use math_aux
  use copy_mod
  use zero_mod
  use summation_mod
  use compare_types_mod
  use poly_cast_mod
  implicit none


  !/ =====================================================================================
  type :: string_array_t
     !/ ----------------------------------------------------------------------------------
     character(:), allocatable :: str
  end type string_array_t


  !/ =====================================================================================
  type, public :: object_pointer
     !/ ----------------------------------------------------------------------------------
     class(*), pointer :: ptr => null()
  end type object_pointer


  integer, public, parameter :: MAX_PATH = 128 !! Maximum charaters in file path


  !/ -------------------------------------------------------------------------------------
  interface LEQ
     !/ ----------------------------------------------------------------------------------
     module procedure :: lexical_equals
  end interface LEQ

  !/ -------------------------------------------------------------------------------------
  interface strcmp
     !/ ----------------------------------------------------------------------------------
     module procedure :: string_compare
  end interface strcmp


  !/ -------------------------------------------------------------------------------------
  interface standard_error
     !/ ----------------------------------------------------------------------------------
     module procedure :: display_standard_error
  end interface standard_error


  !/ -------------------------------------------------------------------------------------
  interface print_array
     !/ ----------------------------------------------------------------------------------
     module procedure :: display_vector
     module procedure :: display_matrix
  end interface print_array




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  function lexical_equals( lhs, rhs ) result( cmp )
    !/ -----------------------------------------------------------------------------------
    !! Lexical equal to.
    !!
    !! Determines whether one string is lexically equal to another string, where the two
    !! strings are interpreted as containing ASCII character codes. 
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: lhs !! left  hand side string
    character(*), intent(in) :: rhs !! right hand side string
    logical                  :: cmp

    cmp = .true.
    if ( llt( lhs, rhs ) ) then
       cmp = .false.
    else
       if ( lgt( lhs, rhs ) ) then
          cmp = .false.
       end if
    end if

  end function lexical_equals


  !/ =====================================================================================
  function string_compare( lhs, rhs ) result( cmp )
    !/ -----------------------------------------------------------------------------------
    !! Compare two strings
    !!
    !! | cmp  | condition  |
    !! |:----:|------------|
    !! |  -1  |lhs .lt. rhs|
    !! |   0  |lhs .eq. rhs|
    !! |  +1  |lhs .gt. rhs|
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: lhs !! left  hand side string
    character(*), intent(in) :: rhs !! right hand side string
    integer                  :: cmp !! result
    !/ -----------------------------------------------------------------------------------
    cmp = 0
    if ( llt( lhs, rhs ) ) then
       cmp = -1
    else
       if ( lgt( lhs, rhs ) ) then
          cmp = 1
       end if
    end if

  end function string_compare




  !/ =====================================================================================
  subroutine display_standard_error( message, num, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*),                        intent(in)  :: message !! supplied error message
    integer,                             intent(in)  :: num     !! supplied error number
    integer,      optional,              intent(out) :: IERR    !! returned error number
    character(:), optional, allocatable, intent(out) :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer :: cde
    !/ -----------------------------------------------------------------------------------
    cde = 0
    if ( present( IERR ) ) cde = cde + 1
    if ( present( EMSG ) ) cde = cde + 2

    if ( 0.eq.cde ) then
       write( ERROR_UNIT, '(A)' ) message
    else
       if ( present( IERR ) ) IERR = num
       if ( present( EMSG ) ) EMSG = message
    end if
  end subroutine display_standard_error




  !/ =====================================================================================
  subroutine display_vector( V, UNIT, FMT )
    !/ -----------------------------------------------------------------------------------
    !! Print the contents of a vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(in) :: V(:)  !! vector
    integer,      optional, intent(in) :: UNIT  !! file unit
    character(*), optional, intent(in) :: FMT   !! edit descriptor (default: ES13.6)

    !/ -----------------------------------------------------------------------------------
    integer :: i, n, un
    character(:), allocatable :: vfmt, sfmt

    n = size(V)

    if ( present( UNIT ) ) then
       un = UNIT
    else
       un = OUTPUT_UNIT
    end if

    if ( present( FMT ) ) then
       vfmt = trim(adjustl(FMT))
    else
       vfmt = 'ES13.6'
    end if

    sfmt = '(' // vfmt // ',*(1X,' // vfmt // '))'

    write(un, sfmt) V(1), (V(i),i=2,n)

  end subroutine display_vector


  !/ =====================================================================================
  subroutine display_matrix( M, UNIT, FMT )
    !/ -----------------------------------------------------------------------------------
    !! Print the contents of a vector.
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(in) :: M(:,:)  !! matrix
    integer,      optional, intent(in) :: UNIT    !! file unit
    character(*), optional, intent(in) :: FMT     !! edit descriptor (default: ES13.6)

    !/ -----------------------------------------------------------------------------------
    integer :: i, j, nr, nc, un
    character(:), allocatable :: vfmt, sfmt

    nr = size(M,DIM=1)
    nc = size(M,DIM=2)

    if ( present( UNIT ) ) then
       un = UNIT
    else
       un = OUTPUT_UNIT
    end if

    if ( present( FMT ) ) then
       vfmt = trim(adjustl(FMT))
    else
       vfmt = 'ES13.6'
    end if

    sfmt = '(' // vfmt // ',*(1X,' // vfmt // '))'

    do i=1,nr
       write(un, sfmt) M(i,1), (M(i,j),j=2,nc)
    end do

  end subroutine display_matrix


end module trncmp_env


!/ =======================================================================================
!/ **                                T R N C M P _ E N V                                **
!/ =========================================================================== END FILE ==
