!/ ====================================================================== BEGIN FILE =====
!/ **                                M E T R I C _ M O D                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 1994-2020, Stephen W. Soliday                                      **
!/ **                           stephen.soliday@trncmp.org                              **
!/ **                           http://research.trncmp.org                              **
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
module metric_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  
  !/ =====================================================================================
  type :: Metric
     !/ ----------------------------------------------------------------------------------
     
     integer               :: num
     real(dp), allocatable :: value

   contains

     procedure :: build => met_build
     procedure :: size  => met_get_num

     final :: met_destroy
     
  end type Metric

  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: size_of_metric
  end interface size




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine met_destroy( met )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Metric), intent(inout) :: met !! reference to a Metric.
    !/ -----------------------------------------------------------------------------------

    if ( 0.lt.met%num ) then
       deallocate( met%value )
    end if
    met%num = 0

  end subroutine met_destroy

  
  !/ =====================================================================================
  subroutine met_build( dts, n )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Metric), intent(inout) :: dts !! reference to this Metric.
    integer,       intent(in)    :: n   !! number of metric parameters.
    !/ -----------------------------------------------------------------------------------

    call met_destroy(dts)
    allocate( dts%value(n), source=D_ZERO )
    dts%num = n
    
  end subroutine met_build
    

end module metric_mod


!/ =======================================================================================
!/ **                                M E T R I C _ M O D                                **
!/ ======================================================================== END FILE =====
