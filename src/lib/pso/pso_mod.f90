!/ ====================================================================== BEGIN FILE =====
!/ **                                   P S O _ M O D                                   **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module pso_mod
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use dice_mod
  use pso_model_mod
  implicit none

  !/ =====================================================================================
  type :: PSO
     !/ ----------------------------------------------------------------------------------

     integer :: num_part   !!  number of particles
     integer :: num_param  !!  position dimension
     integer :: num_metric !!  metric dimension

     real(dp), allocatable :: pos(:,:)   !! position in the parameter space.
     real(dp), allocatable :: vel(:,:)   !! velocity in the parameter space.
     real(dp), allocatable :: met(:,:)   !! cost evaluation meterics.

     real(dp), allocatable :: ppos(:,:)  !! personal best position in the parameter space.
     real(dp), allocatable :: pmet(:,:)  !! personal best cost evaluation meterics.

     real(dp), allocatable :: gpos(:)    !! global best position in the parameter space.
     real(dp), allocatable :: gmet(:)    !! global best cost evaluation meterics.

     real(dp), private, allocatable :: r1(:)    !! random buffer.
     real(dp), private, allocatable :: r2(:)    !! another random buffer.

     class(PSO_Model), private, pointer :: model => null()

     type(Dice), private :: dd

   contains

     procedure, private :: pso_personal_copy
     procedure, private :: pso_global_copy
     procedure, private :: pso_check_personal_best
     procedure, private :: pso_check_global_best
     procedure, private :: pso_evaluate
     procedure, private :: pso_initialize_pos_vel
     procedure, private :: pso_update_pos_vel
     procedure, private :: pso_display_pop

     final     ::             pso_destroy

     procedure :: build => pso_build
     procedure :: fit   => pso_fit

  end type PSO




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  !/ =====================================================================================
  subroutine pso_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Free allocations for the PSO.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(PSO), intent(inout) :: dts  !! reference to this Particle.
    !/ -----------------------------------------------------------------------------------

    if ( 0.lt.dts%num_part ) then
       deallocate( dts%pos  )
       deallocate( dts%vel  )
       deallocate( dts%met  )
       deallocate( dts%ppos )
       deallocate( dts%pmet )
       deallocate( dts%gpos )
       deallocate( dts%gmet )

       deallocate( dts%r1 )
       deallocate( dts%r2 )      
    end if

    dts%num_part   = 0
    dts%num_param  = 0
    dts%num_metric = 0
    dts%model     => null()

  end subroutine pso_destroy


  !/ =====================================================================================
  subroutine pso_personal_copy( dts, idx )
    !/ -----------------------------------------------------------------------------------
    !! Copy a particles current position to it's local best.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO),  intent(inout) :: dts  !! reference to this Particle.
    integer,     intent(in)    :: idx  !! particle index
    !/ -----------------------------------------------------------------------------------
    integer :: j,p,m
    !/ -----------------------------------------------------------------------------------
    p = dts%num_param
    m = dts%num_metric
    do j=1,p
       dts%ppos(j,idx) = dts%pos(j,idx)
    end do
    do j=1,m
       dts%pmet(j,idx) = dts%met(j,idx)
    end do
  end subroutine pso_personal_copy


  !/ =====================================================================================
  subroutine pso_global_copy( dts, idx )
    !/ -----------------------------------------------------------------------------------
    !! Copy a particles current position to the global best.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO),  intent(inout) :: dts  !! reference to this Particle.
    integer,     intent(in)    :: idx  !! particle index
    !/ -----------------------------------------------------------------------------------
    integer :: j,p,m
    !/ -----------------------------------------------------------------------------------
    p = dts%num_param
    m = dts%num_metric
    do j=1,p
       dts%gpos(j) = dts%pos(j,idx)
    end do
    do j=1,m
       dts%gmet(j) = dts%met(j,idx)
    end do
  end subroutine pso_global_copy


  !/ =====================================================================================
  subroutine pso_evaluate( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO),  intent(inout) :: dts  !! reference to this Particle.
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_part

    do i=1,n
       call dts%model%evaluate( dts%met(:,i), dts%pos(:,i ) )
    end do

  end subroutine pso_evaluate


  !/ =====================================================================================
  subroutine pso_check_personal_best( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO),  intent(inout) :: dts  !! reference to this Particle.
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_part

    do i=1,n
       if ( dts%model%isLeftBetter( dts%met(:,i), dts%pmet(:,i) ) ) then
          call dts%pso_personal_copy( i )
       end if
    end do

  end subroutine pso_check_personal_best


  !/ =====================================================================================
  subroutine pso_check_global_best( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO),  intent(inout) :: dts  !! reference to this Particle.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------

    n = dts%num_part

    do i=1,n
       if ( dts%model%isLeftBetter( dts%met(:,i), dts%gmet(:) ) ) then
          call dts%pso_global_copy( i )
       end if
    end do

  end subroutine pso_check_global_best


  !/ =====================================================================================
  subroutine pso_initialize_pos_vel( dts )
    !/ -----------------------------------------------------------------------------------
    !! Initialize each particle's position and velocity.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO), intent(inout) :: dts !! reference to this PSO object.
    !/ -----------------------------------------------------------------------------------
    integer  :: i,j,n,p,m
    !/ -----------------------------------------------------------------------------------

    n = dts%num_part
    p = dts%num_param
    m = dts%num_metric

    !/ ----- initialize the population ---------------------------------------------------
    do j=1,n
       do i=1,p
          dts%pos(i,j)  = D_TWO*dts%dd%uniform() - D_ONE
          dts%vel(i,j)  = D_ZERO
       end do
    end do

    !/ ----- evaluate the entire population ----------------------------------------------
    call dts%pso_evaluate

    !/ ----- set each personal best to it self ------------------------------------------- 
    do j=1,n
       call dts%pso_personal_copy( j )
    end do

    !/ ----- set the global best to one and then step through the rest of the population
    call dts%pso_global_copy( 1 )
    call dts%pso_check_global_best

  end subroutine pso_initialize_pos_vel


  !/ =====================================================================================
  subroutine pso_update_pos_vel( dts, w, c1, c2 )
    !/ -----------------------------------------------------------------------------------
    !! Update each particle's position and velocity.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO), intent(inout) :: dts !! reference to this PSO object.
    real(dp),   intent(in)    :: w   !! inertia constant.
    real(dp),   intent(in)    :: c1  !! cognitive/personal acceleration constant.
    real(dp),   intent(in)    :: c2  !! social/swarm acceleration constant.
    !/ -----------------------------------------------------------------------------------
    integer  :: i,j,n,p
    real(dp) :: x, v
    !/ -----------------------------------------------------------------------------------

    n = dts%num_part
    p = dts%num_param

    do j=1,n
       call dts%dd%uniform_list( dts%r1 )
       call dts%dd%uniform_list( dts%r2 )
       do i=1,p
          v = w*dts%vel(i,j) + &
               &  c1*dts%r1(i)*( dts%ppos(i,j) - dts%pos(i,j) ) + &
               &  c2*dts%r2(i)*( dts%gpos(i)   - dts%pos(i,j) )

          if ( -D_TWO.gt.v ) v = -D_TWO
          if (  D_TWO.lt.v ) v =  D_TWO
          dts%vel(i,j) = v
       end do
    end do

    do j=1,n
       do i=1,p
          x = dts%pos(i,j) + dts%vel(i,j)
          if ( -D_ONE.gt.x ) x = -D_ONE
          if (  D_ONE.lt.x ) x =  D_ONE
          dts%pos(i,j) = x
       end do
    end do

  end subroutine pso_update_pos_vel


  !/ =====================================================================================
  subroutine pso_build( dts, pop, MODEL )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO),       intent(inout) :: dts    !! reference to this PSO object.
    integer,          intent(in)    :: pop    !! number of particles in this PSO.
    class(PSO_Model), target, intent(inout) :: MODEL  !! model to be optimized.
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------

    dts%model      => MODEL
    dts%num_part   = pop
    dts%num_param  = dts%model%nVar()
    dts%num_metric = dts%model%nMet()

    allocate( dts%pos(  dts%num_param,  dts%num_part ) )
    allocate( dts%vel(  dts%num_param,  dts%num_part ) )
    allocate( dts%met(  dts%num_metric, dts%num_part ) )
    allocate( dts%ppos( dts%num_param,  dts%num_part ) )
    allocate( dts%pmet( dts%num_metric, dts%num_part ) )
    allocate( dts%gpos( dts%num_param  ) )
    allocate( dts%gmet( dts%num_metric ) )

    allocate( dts%r1( dts%num_param  ) )
    allocate( dts%r2( dts%num_param  ) )

    call dts%dd%seed_set()

  end subroutine pso_build

  !/ =====================================================================================
  subroutine pso_display_pop( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO), intent(inout) :: dts !! reference to this PSO object.
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------
    n = dts%num_part

    print *, '========================================='
    do i=1,n
       print *,dts%model%toString( dts%met(:,i), dts%pos(:,i), PFMT='F9.4', MFMT='ES10.4' )
    end do
    print *, '-----------------------------------------'
    print *, ''


  end subroutine pso_display_pop


  !/ =====================================================================================
  subroutine pso_fit( dts, maxit, HISTORY, REPORT, MFMT, PFMT, W, W_FINAL, C1, C2 )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(PSO), intent(inout) :: dts    !! reference to this PSO object.
    integer,    intent(in)    :: maxit  !! number of iteration to perform.

    real(dp), allocatable, optional, intent(inout) :: HISTORY(:,:)  !! list  of cost values by iteration.
    integer,      optional, intent(in) :: REPORT  !! report interval.
    character(*), optional, intent(in) :: PFMT
    character(*), optional, intent(in) :: MFMT

    real(dp),     optional, intent(in) :: W       !! 
    real(dp),     optional, intent(in) :: W_FINAL !! 
    real(dp),     optional, intent(in) :: C1
    real(dp),     optional, intent(in) :: C2
    !/ -----------------------------------------------------------------------------------
    real(dp) :: w_con, c1_accel, c2_accel, decay
    integer  :: rep, it, test, j
    !/ -----------------------------------------------------------------------------------

    rep = 0
    if ( present( REPORT ) ) rep = REPORT

    w_con    = D_ONE
    c1_accel = D_TWO
    c2_accel = D_TWO
    decay    = D_ONE

    if ( present( W  ) ) w_con    = W
    if ( present( C1 ) ) c1_accel = C1
    if ( present( C2 ) ) c2_accel = C2

    if ( present( W_FINAL ) ) then
       decay = exp((log(W_FINAL) - log(w_con))/real(maxit-1,dp))
    end if

    !/ -----------------------------------------------------------------------------------

    if ( present( HISTORY ) ) then
       allocate( HISTORY( dts%num_metric, maxit ) )
    end if

    call dts%pso_initialize_pos_vel

    do it=1,maxit

       call dts%pso_update_pos_vel( w_con, c1_accel, c2_accel )
       w_con = w_con * decay

       call dts%pso_evaluate

       call dts%pso_check_personal_best

       call dts%pso_check_global_best

       if ( present( HISTORY ) ) then
          do j=1,dts%num_metric
             HISTORY(j,it) = dts%gmet(j)
          end do
       end if

       if ( 0.lt.rep ) then
          test = modulo( it, report )
          if ( it.eq.0 )    test = 0
          if ( it.eq.maxit) test = 0
          if ( 0.eq.test ) then
             write(*,100) it, dts%model%toString( dts%gmet, dts%gpos, MFMT=MFMT, PFMT=PFMT )
          end if
       end if

    end do

100 format( I0, ': ', A )

  end subroutine pso_fit

end module pso_mod


!/ =======================================================================================
!/ **                                   P S O _ M O D                                   **
!/ ======================================================================== END FILE =====
