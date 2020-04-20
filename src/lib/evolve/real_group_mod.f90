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
  use real_model_mod
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

     integer               :: bests_index
     integer               :: worst_index

   contains

     procedure :: build         => rg_build_parameters
     procedure :: initialize    => rg_initialize_parameters
     procedure :: crossover     => rg_crossover_parameters
     procedure :: mutate        => rg_mutate_parameters
     procedure :: score         => rg_score_parameters
     procedure :: findBest      => rg_find_best_member

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
    n_member = 0
    n_param  = 0

    deallocate( dts%param )
    deallocate( dts%metric )
    deallocate( dts%parent_group )
    deallocate( dts%parent_member )

  end subroutine rg_destroy


  !/ =====================================================================================
  subroutine rg_build_parameters( dts, nvar, nmemb, nmet )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts   !! reference to this RealGroup.
    integer,          intent(in)    :: nvar  !! number of parameters.
    integer,          intent(in)    :: nmemb !! number of members.
    integer,          intent(in)    :: nmet  !! number of metrics.
    !/ -----------------------------------------------------------------------------------

    if ( 0.eq.nmemb ) then
       call rg_destroy( dts )
    end if

    allocate( dts%param( nvar, nmemb, 2 ) )
    allocate( dts%metric( nmet, nmemb ) )
    allocate( dts%parent_group( nmemb ) )
    allocate( dts%parent_member( nmemb ) )

  end subroutine rg_build_parameters


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


function rg_find_best_member( dts, best,  ) % 
  


  !/ =====================================================================================
  subroutine rg_initialize_parameters( dts, pn )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts !! reference to this RealGroup.
    integer,          intent(in)    :: pn  !! population number
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    do i=1,n
       call initialize_parameters( dts%param(:,i,pn) )
    end do

  end subroutine rg_initialize_parameters


  !/ =====================================================================================
  subroutine rg_crossover_parameters( dts, cp, pp, groups, pcross )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts       !! reference to this RealGroup.
    integer,          intent(in)    :: cp        !! child  population number
    integer,          intent(in)    :: pp        !! parent population number
    type(RealGroup),  intent(inout) :: groups(:) !! reference to this RealGroup.
    real(dp),         intent(in)    :: pcross    !! probability of cross vs. clone
   !/ -----------------------------------------------------------------------------------
    integer :: c1, c2, p1, p2, g1, g2, n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    do c1=1,n,2
       c2 = c1 + 1
       g1 = dts%parent_group(c1)
       g2 = dts%parent_group(c2)
       p1 = dts%parent_member(c1)
       p2 = dts%parent_member(c2)

       call crossover(  dts%param(:,c1,cp), dts%param(:,c2,cp), &
            &           groups(g1)%param(:,p1,pp), groups(g2)%param(:,p2,pp), &
            &           pcross )
    end do

  end subroutine rg_crossover_parameters


  !/ =====================================================================================
  subroutine rg_mutate_parameters( dts, dpn, spn, pmutate, sigma )
    !/ -----------------------------------------------------------------------------------
    !! Atempt mutation to each member of this group. 
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts     !! reference to this RealGroup.
    integer,          intent(in)    :: dpn     !! destination population number
    integer,          intent(in)    :: spn     !! source population number
    real(dp),         intent(in)    :: pmutate !! probability that a single allele mutates.
    real(dp),         intent(in)    :: sigma   !! standard deviation of the noise.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    do i=1,n
       call mutate( dts%param(:,i,dpn), dts%param(:,i,spn) )
    end do

  end subroutine rg_mutate_parameters


  !/ =====================================================================================
  subroutine rg_score_parameters( dts, pn, model )
    !/ -----------------------------------------------------------------------------------
    !! Apply a model to each member of this group.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts   !! reference to this RealGroup.
    integer,          intent(in)    :: pn    !! population number
    type(RealModel),  intent(inout) :: model !! reference toi a model.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    do i=1,n
       call model%execute( dts%metric(:,i), dts%param(:,i,pn) )
    end do

  end subroutine rg_score_parameters


  !/ =====================================================================================
  subroutine rg_find_best_member( dts, pn, model )
    !/ -----------------------------------------------------------------------------------
    !! Apply a model to each member of this group.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts   !! reference to this RealGroup.
    integer,          intent(in)    :: pn    !! population number
    type(RealModel),  intent(inout) :: model !! reference toi a model.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    best_index  = 1
    worst_index = 1

    do i=2,n
       if ( isLeftBetter( dts%metric(:,i), dts%metric(:,best_index) ) ) then
          best_index = i
       end if
       if ( isLeftBetter( dts%metric(:,worst_index), dts%metric(:,i) ) ) then
          worst_index = i
       end if
    end do

  end subroutine rg_find_best_member



end module real_group_mod


!/ =======================================================================================
!/ **                            R E A L _ G R O U P _ M O D                            **
!/ ======================================================================== END FILE =====
