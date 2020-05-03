!/ ====================================================================== BEGIN FILE =====
!/ **                           I N T E R P O L A T E _ M O D                           **
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
module interpolate_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-23
  !! license: GPL
  !!
  !!## Interpolation.
  !!
  !! Provides 3 and 5 entry divided difference interpolation.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none

  
  !/ =====================================================================================
  type :: Bug3
     !/ ----------------------------------------------------------------------------------
     real(dp)          :: x1, x2, x3             !! independent table entries
     real(dp)          :: y1, y2, y3             !! dependent table entries
     real(dp)          :: x, delta, next         !! current X, dX and nextX values NeXT
     integer           :: pos        =  0        !! central table position
     integer           :: last       =  0        !! last valid central position
     integer           :: tab_len    =  0        !! number of table entries
     real(dp), pointer :: x_tab(:)   => null()   !! pointer to the independent table
     real(dp), pointer :: y_tab(:)   => null()   !! pointer to the dependent table
     logical           :: owns_tables = .false.  !! flag to indicate ownership of the allocation

   contains

     final ::  b3_destroy_bug

     procedure :: build   => b3_build_bug
     procedure :: advance => b3_advance_bug
     procedure :: set     => b3_set_starting_point
     procedure :: get     => b3_get_next_value
     procedure :: show   =>  b3_show_details

  end type Bug3




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine b3_destroy_bug( bug )
    !/ -----------------------------------------------------------------------------------
    !! Release allocation if this object owns it.
    !/ -----------------------------------------------------------------------------------
    type(Bug3), intent(inout) :: bug  !! reference to a Bug3 object.
    !/ -----------------------------------------------------------------------------------

    if ( bug%owns_tables ) then
       if ( 0.lt.bug%tab_len ) then
          deallocate( bug%x_tab )
          deallocate( bug%y_tab )
       end if
    end if

    bug%owns_tables = .false.
    bug%pos     = 0
    bug%tab_len = 0
    bug%last    = 0
    bug%x       = D_ZERO
    bug%delta   = D_ZERO
    bug%next    = D_ZERO
    bug%x1      = D_ZERO
    bug%x2      = D_ZERO
    bug%x3      = D_ZERO
    bug%y1      = D_ZERO
    bug%y2      = D_ZERO
    bug%y3      = D_ZERO

    bug%x_tab   => null()
    bug%y_tab   => null()

  end subroutine b3_destroy_bug


  !/ =====================================================================================
  subroutine b3_build_bug( bug, REFX, REFY, COPYX, COPYY, IERR, DELTA, X1, POS )
    !/ -----------------------------------------------------------------------------------
    !! Build the Bug. Either point to a source table or allocate memory and copy it.
    !! Optionally set the inital table position directlly or by setting an initial X value.
    !/ -----------------------------------------------------------------------------------
    class(Bug3),                 intent(inout) :: bug      !! reference to this Bug3 object.
    real(dp), optional, target, intent(inout)  :: REFX(:)  !! reference to an independent table.
    real(dp), optional, target, intent(inout)  :: REFY(:)  !! reference to a dependent table.
    real(dp), optional,          intent(in)    :: COPYX(:) !! source to copy an independent table from.
    real(dp), optional,          intent(in)    :: COPYY(:) !! source to copy a dependent table from
    integer,  optional,          intent(out)   :: IERR     !! return error code.
    real(dp), optional,          intent(in)    :: DELTA    !! step change in the independent variable.
    real(dp), optional,          intent(in)    :: X1       !! initial independent variable.
    integer,  optional,          intent(in)    :: POS      !! initial central table position.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, ie, flag
    logical :: report
    !/ -----------------------------------------------------------------------------------

    flag = 0
    ie   = 0
    report = .true.
    if ( present( IERR ) ) report = .false.

    if ( .not.present( DELTA ) ) then
       ie=1
       if ( report ) then
          call log_error( 'DELTA= is a required argument' )
       end if
       goto 999
    end if

    if ( present( REFX ) )  flag = flag + 1
    if ( present( REFY ) )  flag = flag + 2
    if ( present( COPYX ) ) flag = flag + 4
    if ( present( COPYY ) ) flag = flag + 8

    if ( 3.eq.flag ) then
       goto 100

    else if ( 12.eq.flag ) then
       goto 200

    else if ( 0.eq.flag ) then
       ie = 2
       if ( report ) then
          call log_error( 'You must call REFX & REFY or COPYX & COPYY' )
       end if
    else if ( 1.eq.flag ) then
       ie = 3
       if ( report ) then
          call log_error( 'REFY must be used with REFX' )
       end if
    else if ( 2.eq.flag ) then
       ie = 4
       if ( report ) then
          call log_error( 'REFX must be used with REFY' )
       end if
    else if ( 4.eq.flag ) then
       ie = 5
       if ( report ) then
          call log_error( 'COPYY must be used with COPYX' )
       end if
    else if ( 8.eq.flag ) then
       ie = 6
       if ( report ) then
          call log_error( 'COPYX must be used with COPYY' )
       end if
    else
       ie = 7
       if ( report ) then
          call log_error( 'COPY and REF may not be mixed' )
       end if
    end if

    goto 999

    !/ -----------------------------------------------------------------------------------
100 continue  ! Reference the tables
    n = min( size( REFX  ), size( REFY ) )
    bug%x_tab => REFX
    bug%y_tab => REFY
    bug%owns_tables = .false.
    goto 300

    !/ -----------------------------------------------------------------------------------
200 continue  ! create an allocation and copy the tables
    n = min( size( COPYX  ), size( COPYY ) )
    call b3_destroy_bug( bug )
    allocate( bug%x_tab(n) )
    allocate( bug%y_tab(n) )
    do concurrent (i=1:n)
       bug%x_tab(i) = COPYX(i)
       bug%y_tab(i) = COPYY(i)
    end do
    bug%owns_tables = .true.

    !/ -----------------------------------------------------------------------------------
300 continue  ! common build section

    bug%tab_len = n
    bug%last    = n - 1

    call bug%set( DELTA, X1=X1, POS=POS, IERR=ie )

    !/ -----------------------------------------------------------------------------------
999 continue

    if ( present( IERR ) ) IERR = ie


  end subroutine b3_build_bug


  !/ =====================================================================================
  subroutine b3_set_starting_point( bug, delta, X1, POS, IERR )
    !/ -----------------------------------------------------------------------------------
    !! note: assume that the data is sorted by the independent table and that
    !!       the deltas are uniform.
    !/ -----------------------------------------------------------------------------------
    class(Bug3),        intent(inout) :: bug   !! reference to this Bug3 object.
    real(dp),           intent(in)    :: delta !! step change in the independent variable.
    real(dp), optional, intent(in)    :: X1    !! initial independent variable.
    integer,  optional, intent(in)    :: POS   !! initial central table position.
    integer,  optional, intent(out)   :: IERR  !! return error code.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff, test
    integer  :: first, flag, ie
    logical  :: report
    !/ -----------------------------------------------------------------------------------

    bug%delta = delta

    report = .true.
    if ( present( IERR ) ) report = .false.

    first = 0
    flag  = 0
    ie    = 0
    if ( present( X1 ) )  flag = flag + 1
    if ( present( POS ) ) flag = flag + 2

    if ( 1.eq.flag ) then
       goto 100
    else if ( 2.eq.flag ) then
       first = POS
       goto 200
    else if ( 0.eq.flag ) then
       first = 2
       goto 200
    else
       ie = 8
       if ( report ) then
          call log_error( 'use only X1= or POS=' )
       end if
    end if

    goto 999

    !/ -----------------------------------------------------------------------------------
100 continue  ! find position in table
    if ( X1.lt.bug%x_tab(1) ) then
       ie = 9
       if ( report ) then
          call log_error( 'X1 is below the table', R8=X1 )
       end if
       goto 999
    end if

    if ( X1.gt.bug%x_tab(bug%tab_len) ) then
       ie = 10
       if ( report ) then
          call log_error( 'X1 is above the table', R8=X1 )
       end if
       goto 999
    end if

    diff  = bug%x_tab(2) - bug%x_tab(1)
    test  = D_ONE + (X1 - bug%x_tab(1)) / diff

    first = floor(test)
    test = test - real(first,dp)
    if ( test.gt.D_HALF ) then
       first = first + 1
    end if

    !/ -----------------------------------------------------------------------------------
200 continue  ! common section

    if ( 1.lt.first ) then
       if ( first.lt.bug%tab_len ) then
          !/ -----------------------------------------------------------------------------

          bug%pos = first

          bug%x1 = bug%x_tab( bug%pos-1 )
          bug%x2 = bug%x_tab( bug%pos   )
          bug%x3 = bug%x_tab( bug%pos+1 )

          bug%y1 = bug%y_tab( bug%pos-1 )
          bug%y2 = bug%y_tab( bug%pos   )
          bug%y3 = bug%y_tab( bug%pos+1 )

          if ( present( X1 ) ) then
             bug%x = X1
          else
             bug%x = D_HALF*(bug%x1 + bug%x2)
          end if

          bug%next = D_HALF*(bug%x2 + bug%x3)

          !/ -----------------------------------------------------------------------------
       else
          ie = 11
          if ( report ) then
             call log_error('Position is too large', I4=first)
          end if
       end if
    else
       ie = 12
       if ( report ) then
          call log_error('Position is too small', I4=first)
       end if
    end if

    !/ -----------------------------------------------------------------------------------
999 continue

    if ( present( IERR ) ) IERR = ie

  end subroutine b3_set_starting_point


  !/ =====================================================================================
  subroutine b3_advance_bug( bug, ATEND )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    class(Bug3),       intent(inout) :: bug   !! reference to this Bug3 object.
    logical, optional, intent(out)   :: ATEND !! return flag to mark the end of the table.
    !/ -----------------------------------------------------------------------------------
    logical  :: is_at_end
    real(dp) :: test
    !/ -----------------------------------------------------------------------------------

    is_at_end = .false.

    bug%pos = bug%pos + 1
    if ( bug%pos.gt.bug%last ) then
       is_at_end = .true.
       goto 999
    end if

    bug%x1 = bug%x_tab( bug%pos-1 )
    bug%x2 = bug%x_tab( bug%pos   )
    bug%x3 = bug%x_tab( bug%pos+1 )

    bug%y1 = bug%y_tab( bug%pos-1 )
    bug%y2 = bug%y_tab( bug%pos   )
    bug%y3 = bug%y_tab( bug%pos+1 )

    test     = D_HALF*(bug%x1 + bug%x2)
    bug%next = D_HALF*(bug%x2 + bug%x3)

    if ( ( bug%x.gt.bug%next ).or.( bug%x.lt.test ) ) then
       bug%x = D_HALF*(bug%x1 + bug%x2)
       call log_warn( 'X is out of sync. Re-sync, new value', R8=bug%x )
    end if

999 continue
    if ( present( ATEND ) ) ATEND = is_at_end

  end subroutine b3_advance_bug


  !/ =====================================================================================
  function b3_get_next_value( bug, ATEND ) result( y )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    class(Bug3),       intent(inout) :: bug    !! reference to this Bug3 object.
    logical, optional, intent(out)   :: ATEND  !! return flage to mark the end of the table.
    real(dp)                         :: y      !! interpolated value.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: n
    !/ -----------------------------------------------------------------------------------

    if ( present( ATEND ) ) ATEND = .false.

    n = bug%x - bug%x2

    y = D_HALF*(n*(bug%y1 - D_TWO*bug%y2 + bug%y3) - bug%y1 + bug%y3)*n + bug%y2

    bug%x = bug%x + bug%delta
    if ( bug%x.gt.bug%next ) then
       call bug%advance( ATEND=ATEND )
    end if

  end function b3_get_next_value


  !/ =====================================================================================
  subroutine b3_show_details( bug )
    !/ -----------------------------------------------------------------------------------
    !! Display a representation of the divided difference table.
    !/ -----------------------------------------------------------------------------------
    class(Bug3), intent(inout) :: bug   !! reference to this Bug3 object.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: A, B, C
    !/ -----------------------------------------------------------------------------------

    A = bug%y2 - bug%y1
    B = bug%y3 - bug%y2
    C = B - A

    write(*,*)
    write(*,'(A,1X,G0)') 'delta', bug%delta
    write(*,'(A,1X,I0)') 'pos  ', bug%pos
    write(*,'(A,1X,G0)') 'x    ', bug%x
    write(*,'(A,1X,G0)') 'next ', bug%next
    write(*,'(A,1X,I0)') 'end  ', bug%last
    write(*,'(A,1X,I0)') 'len  ', bug%tab_len
    write(*,*)
    write(*, 50)
    write(*,100) bug%pos-1, bug%x1, bug%y1
    write(*,200)                 A
    write(*,300) bug%pos,   bug%x2, bug%y2,    C
    write(*,200)                 B
    write(*,100) bug%pos+1, bug%x3, bug%y3
    write(*,*)

50  format( '   I         X            Y           1st          2nd' )
100 format( I6,2X,ES11.4,2X,ES11.4 )
200 format( 34X,ES11.4 )
300 format( I6,2X,ES11.4,2X,ES11.4,15X,ES11.4 )

  end subroutine b3_show_details


end module interpolate_mod


!/ =======================================================================================
!/ **                           I N T E R P O L A T E _ M O D                           **
!/ =========================================================================== END FILE ==
