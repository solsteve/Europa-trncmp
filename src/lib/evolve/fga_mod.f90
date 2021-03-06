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
  use omp_lib
  use real_group_mod
  use dropoff_mod
  use dice_mod
  implicit none


  !/ =====================================================================================
  type :: FGA
     !/ ----------------------------------------------------------------------------------

     integer :: n_group      = 0       !! number of groups
     integer :: n_member     = 0       !! number of members per group
     integer :: n_tour       = 0       !! tournament size

     integer :: best_group   = 0       !! generation's best  group  number.
     integer :: best_member  = 0       !! generation's best  member number.
     integer :: worst_group  = 0       !! generation's worst group  number.
     integer :: worst_member = 0       !! generation's worst member number.

     logical :: new_best     = .false. !! did generation produce a new best  metric?
     logical :: new_worst    = .false. !! did generation produce a new worst metric?

     real(dp) :: p_cross(2)  = [ 0.90D0, 0.20D0 ]
     real(dp) :: p_mutate(2) = [ 0.10D0, 0.05D0 ]
     real(dp) :: sigma(2)    = [ 0.30D0, 0.01D0 ]

     class(RealModel), pointer :: model => null()
     type(Dice)                :: dd

   contains

     procedure, private :: fga_find_best_member_in_group
     procedure, private :: fga_show_best
     procedure, private :: fga_show_worst

     procedure, private :: scorePop    => fga_score_population
     procedure, private :: markSelect  => fga_mark_parents_for_selection
     procedure, private :: doCrossover => fga_perform_crossover
     procedure, private :: doMutate    => fga_perform_mutation
     procedure, private :: findBest    => fga_find_best_member_overall

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
    !! Free allocations.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FGA), intent(inout) :: ga !! reference to a FGA.
    !/ -----------------------------------------------------------------------------------

    ga%n_group      = 0
    ga%n_member     = 0
    ga%n_tour       = 0

    ga%best_group   = 0
    ga%best_member  = 0
    ga%worst_group  = 0
    ga%worst_member = 0

    ga%new_best     = .false.
    ga%new_worst    = .false.

  end subroutine fga_destroy

  !/ =====================================================================================
  subroutine fga_build( ga, model, nPop, TOUR, PCROSS, PMUTATE, SIGMA )
    !/ -----------------------------------------------------------------------------------
    !! Construct the GA.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA),               intent(inout) :: ga    !! reference to this FGA.
    class(RealModel), target, intent(inout) :: model !! reference to the model.
    integer,                  intent(in)    :: nPop  !! population size.
    integer,  optional,       intent(in)    :: TOUR  !! tournament size.
    real(dp), optional,       intent(in)    :: PCROSS(2)
    real(dp), optional,       intent(in)    :: PMUTATE(2)
    real(dp), optional,       intent(in)    :: SIGMA(2)
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------

    if ( 0.eq.ga%n_group) then
       call fga_destroy( ga )
    end if

    if ( present( PCROSS ) )  ga%p_cross  = PCROSS
    if ( present( PMUTATE ) ) ga%p_mutate = PMUTATE
    if ( present( SIGMA ) )   ga%sigma    = SIGMA

    ga%model => model

    ga%n_group  = omp_get_max_threads()
    ga%n_member = nPop / ga%n_group

    if ( present( TOUR ) ) then
       ga%n_tour = TOUR
    else
       ga%n_tour = 7
    end if

    ga%best_group   = 0
    ga%best_member  = 0
    ga%worst_group  = 0
    ga%worst_member = 0

    ga%new_best     = .false.
    ga%new_worst    = .false.

    call ga%dd%seed_set()

  end subroutine fga_build


  !/ =====================================================================================
  subroutine fga_initialize( ga, group, pn )
    !/ -----------------------------------------------------------------------------------
    !! Initialize the population into groups. There will be one goroup for each
    !! available processor core.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    integer,    intent(in)    :: pn !! population number
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------

    n = ga%n_group

    do i=1,n
       call group(i)%initialize( pn )
    end do

  end subroutine fga_initialize

  
  !/ =====================================================================================
  subroutine fga_score_population( ga, group, pn )
    !/ -----------------------------------------------------------------------------------
    !! Evaluate the model with each population member.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    integer,    intent(in)    :: pn !! population number
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    
    n = ga%n_group
    
    do i=1,n
       call group(i)%score( pn )
    end do

  end subroutine fga_score_population


  !/ =====================================================================================
  subroutine fga_mark_parents_for_selection( ga, group )
    !/ -----------------------------------------------------------------------------------
    !! mark each member for selection
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    !/ -----------------------------------------------------------------------------------
    integer :: g, m, t, ng, nm, nt, b_group, b_member, t_group, t_member
    !/ -----------------------------------------------------------------------------------

    ng = ga%n_group
    nm = ga%n_member
    nt = ga%n_tour

    do g=1,ng
       do m=1,nm
          b_group  = ga%dd%index( ng )
          b_member = ga%dd%index( nm )
          do t=2,nt
             t_group  = ga%dd%index( ng )
             t_member = ga%dd%index( nm )
             if ( ga%model%isLeftBetter( group(t_group)%metric(:,t_member),  &
                  &             group(b_group)%metric(:,b_member) ) ) then
                b_group  = t_group
                b_member = t_member
             end if
          end do
          group(g)%parent_group(m)  = b_group
          group(g)%parent_member(m) = b_member
       end do
    end do


  end subroutine fga_mark_parents_for_selection


  !/ =====================================================================================
  subroutine fga_perform_crossover( ga, group, cn, pn, pcross )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga     !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    integer,    intent(in)    :: cn     !! child  population number
    integer,    intent(in)    :: pn     !! parent population number
    real(dp),   intent(in)    :: pcross !! probability of cross vs. clone
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------

    n = ga%n_group

    do i=1,n
       call group(i)%crossover( cn, pn, group, pcross )
    end do

  end subroutine fga_perform_crossover


  !/ =====================================================================================
  subroutine fga_perform_mutation( ga, group, dpn, spn, pmutate, sigma )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga      !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    integer,    intent(in)    :: dpn     !! destination population number
    integer,    intent(in)    :: spn     !! source population number
    real(dp),   intent(in)    :: pmutate !! probability that a single allele mutates.
    real(dp),   intent(in)    :: sigma   !! standard deviation of the noise.
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------

    n = ga%n_group

    do i=1,n
       call group(i)%mutate( dpn, spn, pmutate, sigma )
    end do

  end subroutine fga_perform_mutation


  !/ =====================================================================================
  subroutine fga_find_best_member_in_group( ga, group )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    !/ -----------------------------------------------------------------------------------
    integer i, n
    !/ -----------------------------------------------------------------------------------

    n = ga%n_group

    do i=1,n
       call group(i)%findBest
    end do

  end subroutine fga_find_best_member_in_group


  !/ =====================================================================================
  subroutine fga_find_best_member_overall( ga, group )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA), intent(inout) :: ga !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    !/ -----------------------------------------------------------------------------------
    integer :: g, idx, best_group, worst_group, best_member, worst_member
    !/ -----------------------------------------------------------------------------------

    call ga%fga_find_best_member_in_group( group )

    best_group   = 1
    worst_group  = 1
    best_member  = group(best_group)%best_index
    worst_member = group(best_group)%worst_index

    do g=1,ga%n_group
       idx = group(g)%best_index
       if ( ga%model%isLeftBetter( group(g)%metric(:,idx), &
            &                      group(best_group)%metric(:,best_member) ) ) then
          best_group = g
          best_member  = group(best_group)%best_index
       end if

       idx = group(g)%worst_index
       if ( ga%model%isLeftBetter( group(worst_group)%metric(:,worst_member), &
            &                      group(g)%metric(:,idx) ) ) then
          worst_group = g
          worst_member = group(best_group)%worst_index
       end if
    end do

    if ( ( ga%best_group.ne.best_group ).or.( ga%best_member.ne.best_member ) ) then
       ga%best_group  = best_group
       ga%best_member = best_member
       ga%new_best    = .true.
    else
       ga%new_best    = .false.
    end if

    if ( ( ga%worst_group.ne.worst_group ).or.( ga%worst_member.ne.worst_member ) ) then
       ga%worst_group  = worst_group
       ga%worst_member = worst_member
       ga%new_worst    = .true.
    else
       ga%new_worst    = .false.
    end if

  end subroutine fga_find_best_member_overall


  !/ =====================================================================================
  function fga_show_best( ga, group, FMT, MFMT, LONG ) result( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA),             intent(inout) :: ga !! reference to this FGA.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: MFMT
    logical,      optional, intent(in)    :: LONG
    character(:), allocatable             :: str
    !/ -----------------------------------------------------------------------------------
    character(32) :: buffer
    !/ -----------------------------------------------------------------------------------
    write( buffer, 100 ) ga%best_group, ga%best_member

    str = trim(adjustl(buffer)) // group(ga%best_group)%toString( 1, ga%best_member, &
         &                               FMT=FMT, MFMT=MFMT, LONG=LONG )

    100 format( '{',I0,',',I0,'}: ' )
  end function fga_show_best


  !/ =====================================================================================
  function fga_show_worst( ga, group, FMT, MFMT, LONG ) result( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA),             intent(inout) :: ga !! reference to this FGA.
     type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
   character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: MFMT
    logical,      optional, intent(in)    :: LONG
    character(:), allocatable             :: str
    !/ -----------------------------------------------------------------------------------
    character(32) :: buffer
    !/ -----------------------------------------------------------------------------------
    write( buffer, 100 ) ga%worst_group, ga%worst_member

    str = trim(adjustl(buffer)) // group(ga%worst_group)%toString( 1, ga%worst_member, &
         &                               FMT=FMT, MFMT=MFMT, LONG=LONG )
    100 format( '{',I0,',',I0,'}: ' )
  end function fga_show_worst


  !/ =====================================================================================
  subroutine fga_evolve( ga, max_gen, REPORT, FMT, MFMT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA),             intent(inout) :: ga !! reference to this FGA.
    integer,                intent(in)    :: max_gen
    integer,      optional, intent(in)    :: REPORT
    character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: MFMT
    !/ -----------------------------------------------------------------------------------
    integer, parameter :: POPA = 1
    integer, parameter :: POPB = 2

    integer       :: nrep, test, gen, i
    type(DropOff) :: pcross, pmutate, sigma
    real(dp)      :: pc, pm, sg, t1, t2

    type(RealGroup), allocatable :: group(:)
    
    !/ -----------------------------------------------------------------------------------

    allocate( group( ga%n_group ) )

    do i=1,ga%n_group
       call group(i)%build( ga%n_member, ga%model )
    end do
   
    !/ -----------------------------------------------------------------------------------

    call pCross%build(  ga%p_cross(1),  ga%p_cross(2),  max_gen, 'Gaussian' )
    call pMutate%build( ga%p_mutate(1), ga%p_mutate(2), max_gen, 'Gaussian' )
    call sigma%build(   ga%sigma(1),    ga%sigma(2),    max_gen, 'Gaussian' )

    if ( present( REPORT ) ) then
       nrep = REPORT
    else
       nrep = 0
    end if

    !/ ----- initialize population A -------------------

    call ga%init( group, POPA )

    !/ ----- evaluate population A ------------------------

    call ga%scorePop( group, POPA )

    call ga%findBest( group )

    !/ v==== begin main evolutionary loop ===============================================v

    t1 = omp_get_wtime()
    
    do gen=1,max_gen

       pc = pcross%next()
       pm = pmutate%next()
       sg = sigma%next()

       call ga%markSelect( group )

       call ga%doCrossover( group, POPB, POPA, pc )

       call ga%doMutate( group, POPA, POPB, pm, sg )

       call ga%scorePop( group, POPA )

       call ga%findBest( group )

       if ( 0.lt.nrep ) then
          test = modulo( gen, nrep )
          if ( gen.eq.0 )    test = 0
          if ( gen.eq.max_gen) test = 0
          if ( 0.eq.test ) then
             write(*,100) gen, ga%fga_show_best(  group, FMT=FMT, MFMT=MFMT )
             write(*,200)      ga%fga_show_worst( group, FMT=FMT, MFMT=MFMT )
             write(*,*)
          end if
       end if
       
    end do

    t2 = omp_get_wtime()
    
    !/ ^==== end main evolutionary loop =================================================^

    write(*,300) ga%fga_show_best(  group, FMT=FMT, MFMT=MFMT, LONG=.true. )
    write(*,310) ga%fga_show_worst( group, FMT=FMT, MFMT=MFMT, LONG=.true. )

    print *,'Elapsed time ', t2-t1, ' seconds.'

    deallocate( group )

100 format( I6,' B: ',A)
200 format( 6X,' W: ',A)
300 format( 'Final Best; ',A)
    310 format( '      Worst ',A)

  end subroutine fga_evolve



end module fga_mod


!/ =======================================================================================
!/ **                                   F G A _ M O D                                   **
!/ ======================================================================== END FILE =====
