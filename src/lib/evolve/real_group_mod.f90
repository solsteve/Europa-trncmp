!/ ====================================================================== BEGIN FILE =====
!/ **                            R E A L _ G R O U P _ M O D                            **
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
module real_group_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  implicit none

  
  !/ =====================================================================================
  type :: RealGroup
     !/ ----------------------------------------------------------------------------------

     integer :: n_member = 0
     integer :: n_param  = 0
     
     real(dp), allocatable :: param(:,:,:)     !! parameters          (nvar,nmemb,2)
     real(dp), allocatable :: metric(:,:)      !! metrics             (nmet,nmemb)
     integer,  allocatable :: parent_group(:)  !! parent group  index      (nmemb)
     integer,  allocatable :: parent_member(:) !! parent member index      (nmemb)
     
   contains

     procedure :: initialize => rg_initialize_parameters
     
     final :: rg_destroy
     
  end type RealGroup

  
  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: rg_size_of_parameters
  end interface size




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine rg_destroy( grp )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(RealGroup), intent(inout) :: grp !! reference to a RealGroup.
    !/ -----------------------------------------------------------------------------------

  end subroutine rg_destroy


  !/ =====================================================================================
  subroutine rg_initialize_parameters( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts !! reference to this RealGroup.
    !/ -----------------------------------------------------------------------------------

  end subroutine rg_initialize_parameters


  !/ =====================================================================================
  function rg_size_of_parameters( dts, DIM ) result( n )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup),  intent(inout) :: dts !! reference to this RealGroup.
    integer, optional, intent(in)    :: DIM !! DIM=1 number of parameters, DIM=2 number of members.
    integer :: n
    !/ -----------------------------------------------------------------------------------

    if ( present( DIM ) ) then
       if ( 1.eq.DIM ) then
          n = dts%n_param
       else
          n = dts%n_member
       end if
    else
       n = dts%n_param
    end if

  end function rg_size_of_parameters
    

end module real_group_mod


!/ =======================================================================================
!/ **                            R E A L _ G R O U P _ M O D                            **
!/ ======================================================================== END FILE =====
