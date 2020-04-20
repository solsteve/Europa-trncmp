!/ ====================================================================== BEGIN FILE =====
!/ **                                   F G A _ M O D                                   **
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
module fga_mod
  !/ -------------------------------------------------------------------------------------
  use real_group_mod
  implicit none


  !/ =====================================================================================
  type :: FGA
     !/ ----------------------------------------------------------------------------------

     type(RealGroup), allocatable :: group(:)

   contains

     procedure, private :: scorePop        => fga_score_population
     procedure, private :: markSelect      => fga_mark_parents_for_selection
     procedure, private :: doCrossover     => fga_perform_crossover
     procedure, private :: doMutate        => fga_perform_mutation
     procedure, private :: findBestByGroup => fga_find_best_member_in_group
     procedure, private :: findBest        => fga_find_best_member_overall

     procedure :: build  => fga_build
     procedure :: init   => fga_initialize
     procedure :: evolve => fga_evolve

     final :: fga_destroy

  end type FGA




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  !/ =====================================================================================
  subroutine fga_destroy( ga )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FGA), intent(inout) :: ga !! reference to a FGA.


  end subroutine fga_destroy

  !/ =====================================================================================
  subroutine fga_build( ga, model, nPop )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA),              intent(inout) :: ga    !! reference to this FGA.
    type(RealModel), target, intent(inout) :: model !!
    integer,                 intent(in)    :: nPop  !! number of variables
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( 0.eq.ga%n_group) then
       call fga_destroy( ga )
    end if

    ga%n_group  = omp_get_max_threads()
    ga%n_member = nPop / ga%nProc
    ga%n_param  = model%nPar
    ga%n_metric = model%nMet
    
    allocate( ga%group( ga%n_group ) )

    do i=1,ga%n_group
       call ga%group(i)%build( ga%n_param, ga%n_member, dts%metric )
    end do

  end subroutine fga_build


  !/ =====================================================================================
  subroutine fga_initialize( ga, pn )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    integer,    intent(in)    :: pn !! population number
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------

    n = ga%n_group

    do i=1,n
       call ga%group(i)%initialize( pn )
    end do

  end subroutine fga_initialize


  !/ =====================================================================================
  subroutine fga_score_population( ga, pn, model )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    integer,   intent(in)    :: pn !! population number
    type(RealModel), intent(inout) :: model
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------
    
    n = ga%n_group

    do i=1,n
       call ga%group(i)%score( pn, model )
    end do

  end subroutine fga_perform_crossover


  !/ =====================================================================================
  subroutine fga_mark_parents_for_selection( ga, pn )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.

  end subroutine fga_mark_parents_for_selection


  !/ =====================================================================================
  subroutine fga_perform_crossover( ga, cn, pn, pcross )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga     !! reference to this FGA.
    integer,    intent(in)    :: cn     !! child  population number
    integer,    intent(in)    :: pn     !! parent population number
    real(dp),   intent(in)    :: pcross !! probability of cross vs. clone
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------
    
    n = ga%n_group

    do i=1,n
       call ga%group(i)%crossover( cn, pn, ga%group, pcross )
    end do

  end subroutine fga_perform_crossover


  !/ =====================================================================================
  subroutine fga_perform_mutation( ga, dpn, spn, pmutate, sigma )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga      !! reference to this FGA.
    integer,    intent(in)    :: dpn     !! destination population number
    integer,    intent(in)    :: spn     !! source population number
    real(dp),   intent(in)    :: pmutate !! probability that a single allele mutates.
    real(dp),   intent(in)    :: sigma   !! standard deviation of the noise.
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------
    
    n = ga%n_group

    do i=1,n
       call ga%group(i)%mutate( dpn, spn, pmutate, sigma )
    end do

  end subroutine fga_perform_mutation


  !/ =====================================================================================
  subroutine fga_find_best_member_in_group( ga )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------
    
    n = ga%n_group

    do i=1,n
       call ga%group(i)%findBest()
    end do

  end subroutine fga_find_best_member_in_group


  !/ =====================================================================================
  subroutine fga_find_best_member_overall( ga )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.

  end subroutine fga_find_best_member_overall


  !/ =====================================================================================
  subroutine fga_evolve( ga, max_gen, REPORT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA),        intent(inout) :: ga !! reference to this FGA.
    integer,           intent(in)    :: max_gen
    integer, optional, intent(in)    :: REPORT
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------

    if ( present( REPORT ) ) then
       nrep = REPORT
    else
       nrep = 10
    end if

    !/ ----- initialize population A -------------------

    call ga%init(1)

    !/ ----- evaluate population A ------------------------

    call ga%score( 1, ga%model )

    call ga%findBestByGroup()

    call ga%findBest()

    !/ v==== begin main evolutionary loop ===============================================v
    count = 0
    do gen=1,max_gen

       call ga%markSelect()

       call ga%doCrossover( 2, 1, pCross )

       call ga%doMutate( 1, 2, pMutate, sigma )

       call ga%score( 1, ga%model )

       call ga%findBestByGroup()

       call ga%findBest()

       count = count + 1
       if ( count.ge.nrep ) then
          print *, gen,  best_metric
          count = 0
       end if
          
    end do
    !/ ^==== end main evolutionary loop =================================================^

    ! PRINT FINAL
    
  end subroutine fga_evolve



end module fga_mod


!/ =======================================================================================
!/ **                                   F G A _ M O D                                   **
!/ ======================================================================== END FILE =====
