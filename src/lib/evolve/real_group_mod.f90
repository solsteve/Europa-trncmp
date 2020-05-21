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
  use real_toolbox_mod
  use evo_entropy_mod
  implicit none


  !/ =====================================================================================
  type :: RealGroup
     !/ ----------------------------------------------------------------------------------

     integer :: id       = 0
     integer :: n_member = 0
     integer :: n_param  = 0

     real(dp), allocatable :: param(:,:,:)     !! parameters          (npar,nmemb,2)
     real(dp), allocatable :: metric(:,:)      !! metrics             (nmet,nmemb)
     integer,  allocatable :: parent_group(:)  !! parent group  index      (nmemb)
     integer,  allocatable :: parent_member(:) !! parent member index      (nmemb)

     integer               :: best_index  = 0
     integer               :: worst_index = 0
     logical               :: new_best    = .false.
     logical               :: new_worst   = .false.

     type(Entropy) :: dd
     class(RealModel), pointer :: model => null()

   contains

     procedure :: build         => rg_build_parameters
     procedure :: initialize    => rg_initialize_parameters
     procedure :: zero          => rg_zero_parameters
     procedure :: crossover     => rg_crossover_parameters
     procedure :: mutate        => rg_mutate_parameters
     procedure :: score         => rg_score_parameters
     procedure :: findBest      => rg_find_best_member
     procedure :: toString      => rg_member_to_string

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
    grp%n_member = 0
    grp%n_param  = 0

    if ( 0.lt.grp%n_member ) then
       deallocate( grp%param )
       deallocate( grp%metric )
       deallocate( grp%parent_group )
       deallocate( grp%parent_member )
    end if
    
    grp%best_index  = 0
    grp%worst_index = 0
    grp%new_best    = .false.
    grp%new_worst   = .false.

  end subroutine rg_destroy


  !/ =====================================================================================
  subroutine rg_build_parameters( dts, nmemb, model )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts   !! reference to this RealGroup.
    integer,          intent(in)    :: nmemb !! number of members.
    class(RealModel), target, intent(inout) :: model !! reference to the model.
    !/ -----------------------------------------------------------------------------------
    integer :: npar, nmet
    npar = model%nPar()
    nmet = model%nMet()

    call dts%dd%seed

    dts%model => model

    dts%n_member = nmemb
    dts%n_param  = npar

    if ( 0.eq.nmemb ) then
       call rg_destroy( dts )
    end if

    allocate( dts%param( npar, nmemb, 2 ) )
    allocate( dts%metric( nmet, nmemb ) )
    allocate( dts%parent_group( nmemb ) )
    allocate( dts%parent_member( nmemb ) )

    dts%best_index  = 0
    dts%worst_index = 0
    dts%new_best    = .false.
    dts%new_worst   = .false.

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
       call initialize_parameters( dts%dd, dts%param(:,i,pn) )
    end do

  end subroutine rg_initialize_parameters


   !/ =====================================================================================
  subroutine rg_zero_parameters( dts, pn )
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
       call zero_parameters( dts%param(:,i,pn) )
    end do

  end subroutine rg_zero_parameters


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

       call crossover(  dts%dd, dts%param(:,c1,cp), dts%param(:,c2,cp), &
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
       call mutate( dts%dd, dts%param(:,i,dpn), dts%param(:,i,spn), pmutate, sigma )
    end do

  end subroutine rg_mutate_parameters


  !/ =====================================================================================
  subroutine rg_score_parameters( dts, pn )
    !/ -----------------------------------------------------------------------------------
    !! Apply a model to each member of this group.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts   !! reference to this RealGroup.
    integer,          intent(in)    :: pn    !! population number
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    do i=1,n
       call dts%model%evaluate( dts%metric(:,i), dts%param(:,i,pn) )
    end do

  end subroutine rg_score_parameters


  !/ =====================================================================================
  subroutine rg_find_best_member( dts )
    !/ -----------------------------------------------------------------------------------
    !! Apply a model to each member of this group.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup), intent(inout) :: dts   !! reference to this RealGroup.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, best_index, worst_index
    !/ -----------------------------------------------------------------------------------

    n = dts%n_member

    best_index  = 1
    worst_index = 1

    do i=2,n
       if ( dts%model%isLeftBetter( dts%metric(:,i), dts%metric(:,best_index) ) ) then
          best_index = i
       end if
       if ( dts%model%isLeftBetter( dts%metric(:,worst_index), dts%metric(:,i) ) ) then
          worst_index = i
       end if
    end do

    if ( dts%best_index.ne.best_index ) then
       dts%new_best   = .true.
       dts%best_index = best_index
    else
       dts%new_best   = .false.
    end if

    if ( dts%worst_index.ne.worst_index ) then
       dts%new_worst   = .true.
       dts%worst_index = worst_index
    else
       dts%new_worst   = .false.
    end if

  end subroutine rg_find_best_member

  !/ =====================================================================================
  function rg_member_to_string( dts, pn, member, FMT, MFMT, LONG ) result( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(RealGroup),       intent(inout) :: dts    !! reference to this RealGroup.
    integer,                intent(in)    :: pn     !! population number.
    integer,                intent(in)    :: member !! member number to format.
    character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: MFMT
    logical,      optional, intent(in)    :: LONG
    character(:), allocatable             :: str
    !/ -----------------------------------------------------------------------------------

    str = dts%model%toString( dts%metric(:,member),    &
         &                    dts%param(:,member,pn),  &
         &                    FMT=FMT, MFMT=MFMT, LONG=LONG )

  end function rg_member_to_string


  
end module real_group_mod


!/ =======================================================================================
!/ **                            R E A L _ G R O U P _ M O D                            **
!/ ======================================================================== END FILE =====
