!/ ====================================================================== BEGIN FILE =====
!/ **                               F G A _ O M P _ M O D                               **
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
module fga_omp_mod
  !/ -------------------------------------------------------------------------------------
  use omp_lib
  use real_group_mod
  use dropoff_mod
  use evo_entropy_mod
  implicit none

  !/ =====================================================================================
  type :: FGA_OMP
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

   contains

     procedure, private :: fo_show_best
     procedure, private :: fo_show_worst

     procedure, private :: markSelect  => fo_mark_parents_for_selection
     procedure, private :: findBest    => fo_find_best_member_overall

     procedure :: build  => fo_build
     procedure :: evolve => fo_evolve

     final :: fo_destroy

  end type FGA_OMP




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine fo_destroy( ga )
    !/ -----------------------------------------------------------------------------------
    !! Free allocations.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(FGA_OMP), intent(inout) :: ga !! reference to a FGA_OMP.
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

  end subroutine fo_destroy

  !/ =====================================================================================
  subroutine fo_build( ga, model, nPop, TOUR, PCROSS, PMUTATE, SIGMA )
    !/ -----------------------------------------------------------------------------------
    !! Construct the GA.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA_OMP),           intent(inout) :: ga    !! reference to this FGA_OMP.
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
       call fo_destroy( ga )
    end if

    if ( present( PCROSS ) )  ga%p_cross  = PCROSS
    if ( present( PMUTATE ) ) ga%p_mutate = PMUTATE
    if ( present( SIGMA ) )   ga%sigma    = SIGMA

    ga%model => model

    ga%n_group  = omp_get_max_threads()
    ga%n_member = nPop / ga%n_group

    print *, 'Using', ga%n_group, 'groups'

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

  end subroutine fo_build


  !/ =====================================================================================
  subroutine fo_mark_parents_for_selection( ga, dd, group )
    !/ -----------------------------------------------------------------------------------
    !! mark each member for selection
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA_OMP), intent(inout) :: ga !! reference to this FGA_OMP.
    type(Entropy),      intent(inout) :: dd     !! reference to an entropy source.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    !/ -----------------------------------------------------------------------------------
    integer :: g, m, t, ng, nm, nt, b_group, b_member, t_group, t_member
    !/ -----------------------------------------------------------------------------------

    ng = ga%n_group
    nm = ga%n_member
    nt = ga%n_tour

    do g=1,ng
       do m=1,nm
          b_group  = dd%index( ng )
          b_member = dd%index( nm )
          do t=2,nt
             t_group  = dd%index( ng )
             t_member = dd%index( nm )
             if ( ga%model%isLeftBetter( group(t_group)%metric(:,t_member),  &
                  &                      group(b_group)%metric(:,b_member) ) ) then
                b_group  = t_group
                b_member = t_member
             end if
          end do
          group(g)%parent_group(m)  = b_group
          group(g)%parent_member(m) = b_member
       end do
    end do


  end subroutine fo_mark_parents_for_selection



  !/ =====================================================================================
  subroutine fo_find_best_member_overall( ga, group )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA_OMP), intent(inout) :: ga !! reference to this FGA_OMP.
    type(RealGroup), intent(inout) :: group(:)  !! reference to the group list
    !/ -----------------------------------------------------------------------------------
    integer :: g, idx, best_group, worst_group, best_member, worst_member
    !/ -----------------------------------------------------------------------------------

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

    !print *,best_group,best_member,worst_group,worst_member

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

  end subroutine fo_find_best_member_overall


  !/ =====================================================================================
  function fo_show_best( ga, group, FMT, MFMT, LONG ) result( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA_OMP),             intent(inout) :: ga !! reference to this FGA_OMP.
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
  end function fo_show_best


  !/ =====================================================================================
  function fo_show_worst( ga, group, FMT, MFMT, LONG ) result( str )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA_OMP), intent(inout) :: ga !! reference to this FGA_OMP.
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
         &                                FMT=FMT, MFMT=MFMT, LONG=LONG )
100 format( '{',I0,',',I0,'}: ' )
  end function fo_show_worst


  !/ =====================================================================================
  subroutine fo_evolve( ga, max_gen, REPORT, FMT, MFMT )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(FGA_OMP),         intent(inout) :: ga !! reference to this FGA_OMP.
    integer,                intent(in)    :: max_gen
    integer,      optional, intent(in)    :: REPORT
    character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: MFMT
    !/ -----------------------------------------------------------------------------------
    integer, parameter :: POPA = 1
    integer, parameter :: POPB = 2

    integer       :: nrep, test, gen, tid, i, j
    type(DropOff) :: pcross, pmutate, sigma
    real(dp)      :: pc, pm, sg, t1, t2

    type(RealGroup), allocatable :: group(:)
    type(Entropy) :: dd

    call dd%seed

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

    do i=1,ga%n_group

       !/ ----- initialize population A ----------------------
       call group(i)%initialize( POPA )
       call group(i)%zero( POPB )

       !/ ----- evaluate population A ------------------------
       call group(i)%score( POPA )

       call group(i)%findBest
    end do

    call ga%findBest( group )

    !/ v==== begin main evolutionary loop ===============================================v

    t1 = omp_get_wtime()

    do gen=1,max_gen

       pc = pcross%next()
       pm = pmutate%next()
       sg = sigma%next()

       call ga%markSelect( dd, group )

       do j=1,ga%n_group
          call group(j)%crossover( POPB, POPA, group, pc )
       end do
     
        !/ ---------------------------------------------------------
       !$OMP PARALLEL SHARED(group) PRIVATE(tid,pc,pm,sg)
       tid = omp_get_thread_num() + 1

       group(tid)%id = tid

       !$OMP FLUSH

       !call group(tid)%crossover( POPB, POPA, group, pc )
       !$OMP BARRIER

       call group(tid)%mutate( POPA, POPB, pm, sg )
       !$OMP BARRIER

       call group(tid)%score( POPA )
       !$OMP BARRIER

       call group(tid)%findBest

       !$OMP END PARALLEL
       !/ ---------------------------------------------------------

       call ga%findBest( group )

       if ( 0.lt.nrep ) then
          test = modulo( gen, nrep )
          if ( gen.eq.0 )    test = 0
          if ( gen.eq.max_gen) test = 0
          if ( 0.eq.test ) then
             write(*,100) gen, ga%fo_show_best(  group, FMT=FMT, MFMT=MFMT )
             write(*,200)      ga%fo_show_worst( group, FMT=FMT, MFMT=MFMT )
             write(*,*)
          end if
       end if

    end do

    t2 = omp_get_wtime()

    !/ ^==== end main evolutionary loop =================================================^

    write(*,300) ga%fo_show_best( group, FMT=FMT, MFMT=MFMT, LONG=.true. )
    write(*,310) ga%fo_show_worst( group, FMT=FMT, MFMT=MFMT, LONG=.true. )

    print *,'Elapsed time ', t2-t1, ' seconds.'

    !do i=1,ga%n_group
    !   print *, group(i)%id, ' = ', group(i)%best_index, group(i)%worst_index
    !end do

    deallocate( group )

100 format( I6,' B: ',A)
200 format( 6X,' W: ',A)
300 format( 'Final Best  ',A)
310 format( '      Worst ',A)

  end subroutine fo_evolve



end module fga_omp_mod


!/ =======================================================================================
!/ **                               F G A _ O M P _ M O D                               **
!/ ======================================================================== END FILE =====
