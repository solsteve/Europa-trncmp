!/ ====================================================================== BEGIN FILE =====
!/ **                           F T E S T _ C O N S T R U C T                           **
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
module ftest_construct
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none
  private

  type, public :: record
     private
     real(dp)               :: age_value = 20.0
     real(dp)               :: gpa_value = 3.5
     character, allocatable :: name_value
     integer                :: sat_score = 1000
   contains
     private

     procedure, public :: print => print_record
     procedure, public :: set   => set_record
     procedure, public :: get   => get_record
  end type record

contains

  subroutine print_record( self )
    implicit none
    class(record), intent(in) :: self
    write(*,*) 'Name', self%name_value
    write(*,*) 'Age ', self%age_value
    write(*,*) 'GPA ', self%gpa_value
    write(*,*) 'SAT ', self%sat_score
  end subroutine print_record

  subroutine set_record( self, age, gpa, name, sat )
    implicit none
    class(record),                       intent(inout) :: self
    real(dp),     optional,              intent(in)    :: age
    real(dp),     optional,              intent(in)    :: gpa
    character(:), optional, allocatable, intent(in)    :: name
    integer,      optional,              intent(in)    :: sat
    if ( present(age)  ) self%age_value  = age
    if ( present(gpa)  ) self%gpa_value  = gpa
    if ( present(name) ) self%name_value = name
    if ( present(sat)  ) self%sat_score  = sat
  end subroutine set_record

  subroutine get_record( self, age, gpa, name, sat )
    implicit none
    class(record),                       intent(inout) :: self
    real(dp),     optional,              intent(inout) :: age
    real(dp),     optional,              intent(inout) :: gpa
    character(:), optional, allocatable, intent(inout) :: name
    integer,      optional,              intent(inout) :: sat
    if ( present(age)  ) age  = self%age_value
    if ( present(gpa)  ) gpa  = self%gpa_value
    if ( present(name) ) name = self%name_value
    if ( present(sat)  ) sat  = self%sat_score
  end subroutine get_record

end module ftest_construct



!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use ftest_construct
  implicit none

  type(record) :: A
  character(:), allocatable :: my_name

  my_name='Steve'

  call A%set( name=my_name, age=53.0d0 )
  call A%set( sat=1280 )

  call A%print

end program main

!/ =======================================================================================
!/ **                           F T E S T _ C O N S T R U C T                           **
!/ =========================================================================== END FILE ==
