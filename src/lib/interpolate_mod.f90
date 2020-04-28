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
  !!## Object.
  !!
  !! Object.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  implicit none

  !/ =====================================================================================
  type :: Bug3
     !/ ----------------------------------------------------------------------------------
     real(dp)          :: x1, x2, x3
     real(dp)          :: y1, y2, y3
     real(dp)          :: x, delta, next
     integer           :: pos        =  0
     integer           :: last       =  0
     integer           :: tab_len    =  0
     real(dp), pointer :: x_tab(:)   => null()
     real(dp), pointer :: y_tab(:)   => null()
     logical           :: owns_tables = .false.

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
  subroutine b3_destroy_bug( dts )
    !/ -----------------------------------------------------------------------------------
    !! Release allocation if this object owns it.
    !/ -----------------------------------------------------------------------------------
    type(Bug3), intent(inout) :: dts
    !/ -----------------------------------------------------------------------------------

    if ( dts%owns_tables ) then
       if ( 0.lt.dts%tab_len ) then
          deallocate( dts%x_tab )
          deallocate( dts%y_tab )
       end if
    end if

    dts%owns_tables = .false.
    dts%pos     = 0
    dts%tab_len = 0
    dts%last    = 0
    dts%x       = D_ZERO
    dts%delta   = D_ZERO
    dts%next    = D_ZERO
    dts%x1      = D_ZERO
    dts%x2      = D_ZERO
    dts%x3      = D_ZERO
    dts%y1      = D_ZERO
    dts%y2      = D_ZERO
    dts%y3      = D_ZERO

    dts%x_tab   => null()
    dts%y_tab   => null()

  end subroutine b3_destroy_bug


  !/ =====================================================================================
  subroutine b3_build_bug( dts, REFX, REFY, COPYX, COPYY, IERR, DELTA, X1, POS )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    class(Bug3),                 intent(inout) :: dts
    real(dp), optional, target, intent(inout) :: REFX(:)
    real(dp), optional, target, intent(inout) :: REFY(:)
    real(dp), optional,          intent(in)    :: COPYX(:)
    real(dp), optional,          intent(in)    :: COPYY(:)
    integer,  optional,          intent(out)   :: IERR
    real(dp), optional,          intent(in)    :: DELTA
    real(dp), optional,          intent(in)    :: X1
    integer,  optional,          intent(in)    :: POS
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
    dts%x_tab => REFX
    dts%y_tab => REFY
    dts%owns_tables = .false.
    goto 300

    !/ -----------------------------------------------------------------------------------
200 continue  ! create an allocation and copy the tables
    n = min( size( COPYX  ), size( COPYY ) )
    call b3_destroy_bug( dts )
    allocate( dts%x_tab(n) )
    allocate( dts%y_tab(n) )
    do concurrent (i=1:n)
       dts%x_tab(i) = COPYX(i)
       dts%y_tab(i) = COPYY(i)
    end do
    dts%owns_tables = .true.

    !/ -----------------------------------------------------------------------------------
300 continue  ! common build section

    dts%tab_len = n
    dts%last    = n - 1

    call dts%set( DELTA, X1=X1, POS=POS, IERR=ie )

    !/ -----------------------------------------------------------------------------------
999 continue

    if ( present( IERR ) ) IERR = ie


  end subroutine b3_build_bug


  !/ =====================================================================================
  subroutine b3_set_starting_point( dts, delta, X1, POS, IERR )
    !/ -----------------------------------------------------------------------------------
    !! note: assume that the data is sorted by the independent table and that
    !!       the deltas are uniform.
    !/ -----------------------------------------------------------------------------------
    class(Bug3),        intent(inout) :: dts
    real(dp),           intent(in)    :: delta
    real(dp), optional, intent(in)    :: X1
    integer,  optional, intent(in)    :: POS
    integer,  optional, intent(out)   :: IERR
    !/ -----------------------------------------------------------------------------------
    real(dp) :: diff, test
    integer  :: first, flag, ie
    logical  :: report
    !/ -----------------------------------------------------------------------------------

    dts%delta = delta

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
    if ( X1.lt.dts%x_tab(1) ) then
       ie = 9
       if ( report ) then
          call log_error( 'X1 is below the table', R8=X1 )
       end if
       goto 999
    end if

    if ( X1.gt.dts%x_tab(dts%tab_len) ) then
       ie = 10
       if ( report ) then
          call log_error( 'X1 is above the table', R8=X1 )
       end if
       goto 999
    end if

    diff  = dts%x_tab(2) - dts%x_tab(1)
    test  = D_ONE + (X1 - dts%x_tab(1)) / diff

    first = floor(test)
    test = test - real(first,dp)
    if ( test.gt.D_HALF ) then
       first = first + 1
    end if

    !/ -----------------------------------------------------------------------------------
200 continue  ! common section

    if ( 1.lt.first ) then
       if ( first.lt.dts%tab_len ) then
          !/ -----------------------------------------------------------------------------

          dts%pos = first

          dts%x1 = dts%x_tab( dts%pos-1 )
          dts%x2 = dts%x_tab( dts%pos   )
          dts%x3 = dts%x_tab( dts%pos+1 )

          dts%y1 = dts%y_tab( dts%pos-1 )
          dts%y2 = dts%y_tab( dts%pos   )
          dts%y3 = dts%y_tab( dts%pos+1 )

          if ( present( X1 ) ) then
             dts%x = X1
          else
             dts%x = D_HALF*(dts%x1 + dts%x2)
          end if

          dts%next = D_HALF*(dts%x2 + dts%x3)

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
  subroutine b3_advance_bug( dts, ATEND )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    class(Bug3),       intent(inout) :: dts
    logical, optional, intent(out)   :: ATEND
    !/ -----------------------------------------------------------------------------------
    logical  :: is_at_end
    real(dp) :: test
    !/ -----------------------------------------------------------------------------------

    is_at_end = .false.

    dts%pos = dts%pos + 1
    if ( dts%pos.gt.dts%last ) then
       is_at_end = .true.
       goto 999
    end if

    dts%x1 = dts%x_tab( dts%pos-1 )
    dts%x2 = dts%x_tab( dts%pos   )
    dts%x3 = dts%x_tab( dts%pos+1 )

    dts%y1 = dts%y_tab( dts%pos-1 )
    dts%y2 = dts%y_tab( dts%pos   )
    dts%y3 = dts%y_tab( dts%pos+1 )

    test     = D_HALF*(dts%x1 + dts%x2)
    dts%next = D_HALF*(dts%x2 + dts%x3)

    if ( ( dts%x.gt.dts%next ).or.( dts%x.lt.test ) ) then
       dts%x = D_HALF*(dts%x1 + dts%x2)
       call log_warn( 'X is out of sync. Re-sync, new value', R8=dts%x )
    end if

999 continue
    if ( present( ATEND ) ) ATEND = is_at_end

  end subroutine b3_advance_bug


  !/ =====================================================================================
  function b3_get_next_value( dts, ATEND ) result( y )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    class(Bug3),       intent(inout) :: dts
    logical, optional, intent(out)   :: ATEND
    real(dp)                         :: y  !! interpolated value
    !/ -----------------------------------------------------------------------------------
    real(dp) :: n
    logical  :: is_at_end
    !/ -----------------------------------------------------------------------------------

    if ( present( ATEND ) ) ATEND = .false.

    n = dts%x - dts%x2

    y = D_HALF*(n*(dts%y1 - D_TWO*dts%y2 + dts%y3) - dts%y1 + dts%y3)*n + dts%y2

    dts%x = dts%x + dts%delta
    if ( dts%x.gt.dts%next ) then
       call dts%advance( ATEND=ATEND )
    end if

  end function b3_get_next_value


  !/ =====================================================================================
  subroutine b3_show_details( dts )
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------
    class(Bug3), intent(inout) :: dts
    !/ -----------------------------------------------------------------------------------
    real(dp) :: A, B, C
    !/ -----------------------------------------------------------------------------------

    A = dts%y2 - dts%y1
    B = dts%y3 - dts%y2
    C = B - A

    write(*,*)
    write(*,'(A,1X,G0)') 'delta', dts%delta
    write(*,'(A,1X,I0)') 'pos  ', dts%pos
    write(*,'(A,1X,G0)') 'x    ', dts%x
    write(*,'(A,1X,G0)') 'next ', dts%next
    write(*,'(A,1X,I0)') 'end  ', dts%last
    write(*,'(A,1X,I0)') 'len  ', dts%tab_len
    write(*,*)
    write(*, 50)
    write(*,100) dts%pos-1, dts%x1, dts%y1
    write(*,200)                 A
    write(*,300) dts%pos,   dts%x2, dts%y2,    C
    write(*,200)                 B
    write(*,100) dts%pos+1, dts%x3, dts%y3
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
