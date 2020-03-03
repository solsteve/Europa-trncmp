!/ ====================================================================== BEGIN FILE =====
!/ **                             C O N F U S I O N _ M O D                             **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
module confusion_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a collection of tools for confusion matrix and associated metrics.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2019-12-13
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use iso_varying_string
  implicit none


  !/ =====================================================================================
  type :: confusion_matrix
     !/ ----------------------------------------------------------------------------------

     type(varying_string)              :: title             !! Graph title
     type(varying_string), allocatable :: short_name(:)     !! Three letter class labels
     type(varying_string), allocatable :: long_name(:)      !! Seven letter class labels
     integer,              allocatable :: mat(:,:)          !! NxN error matrix
     integer                           :: non_class_actual  !! Total number of unlabled actuals
     integer                           :: non_class_predict !! Total number of unlabled predictions
     
     integer,              allocatable :: act_total(:)      !! column totals
     integer,              allocatable :: prd_total(:)      !! row totals
     integer                           :: tru_total         !! total agreement ( diagonal sum )
     integer                           :: off_total         !! total off diagonal
     integer                           :: total             !! grand total
     
   contains

     procedure, private :: cm_set_title_vs
     procedure, private :: cm_set_title_chr

     procedure, private :: cm_set_shortname_vs
     procedure, private :: cm_set_shortname_chr

     procedure, private :: cm_set_longname_vs
     procedure, private :: cm_set_longname_chr

     procedure, private :: cm_load_int_exemplars
     procedure, private :: cm_load_char_exemplars
     
     procedure :: show_matrix   => cm_show_matrix
     procedure :: show_metrics  => cm_show_metrics
     procedure :: show          => cm_show_all


     generic :: set_title     => cm_set_title_vs,     cm_set_title_chr
     generic :: set_shortname => cm_set_shortname_vs, cm_set_shortname_chr
     generic :: set_longname  => cm_set_longname_vs,  cm_set_longname_chr

     generic :: load => cm_load_int_exemplars, cm_load_char_exemplars
     
  end type confusion_matrix



  
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  subroutine cm_set_title_vs( cm, tvs )
     !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    type(varying_string),    intent(in)    :: tvs
     !/ ----------------------------------------------------------------------------------
    cm%title = tvs
  end subroutine cm_set_title_vs

  
  !/ =====================================================================================
  subroutine cm_set_title_chr( cm, tchr )
     !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    character(*),            intent(in)    :: tchr
     !/ ----------------------------------------------------------------------------------
    cm%title = tchr
  end subroutine cm_set_title_chr


  !/ =====================================================================================
  subroutine cm_set_shortname_vs( cm, tvs )
     !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    type(varying_string),    intent(in)    :: tvs(:)
    !/ ----------------------------------------------------------------------------------
    integer :: i, n
     !/ ----------------------------------------------------------------------------------
    n = size(tvs)
    allocate( cm%short_name(n) )
    do i=1,n
       cm%short_name(i) = tvs(i)
    end do

  end subroutine cm_set_shortname_vs

  
  !/ =====================================================================================
  subroutine cm_set_shortname_chr( cm, tchr )
     !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    character(*),            intent(in)    :: tchr(:)
    !/ ----------------------------------------------------------------------------------
    integer :: i, n
     !/ ----------------------------------------------------------------------------------
    n = size(tchr)
    allocate( cm%short_name(n) )
    do i=1,n
       cm%short_name(i) = tchr(i)
    end do

  end subroutine cm_set_shortname_chr


  !/ =====================================================================================
  subroutine cm_set_longname_vs( cm, tvs )
     !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    type(varying_string),    intent(in)    :: tvs(:)
    !/ ----------------------------------------------------------------------------------
    integer :: i, n
     !/ ----------------------------------------------------------------------------------
    n = size(tvs)
    allocate( cm%long_name(n) )
    do i=1,n
       cm%long_name(i) = tvs(i)
    end do

  end subroutine cm_set_longname_vs

  
  !/ =====================================================================================
  subroutine cm_set_longname_chr( cm, tchr )
     !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    character(*),            intent(in)    :: tchr(:)
    !/ ----------------------------------------------------------------------------------
    integer :: i, n
     !/ ----------------------------------------------------------------------------------
    n = size(tchr)
    allocate( cm%long_name(n) )
    do i=1,n
       cm%long_name(i) = tchr(i) // '         '
    end do

  end subroutine cm_set_longname_chr


  !/ =====================================================================================
  subroutine cm_compute_metrics( cm )
    !/ ----------------------------------------------------------------------------------
    !!
    !/ ----------------------------------------------------------------------------------
    implicit none
    type(confusion_matrix), intent(inout) :: cm
    !/ ----------------------------------------------------------------------------------
    integer :: row, col, n_class
    !/ ----------------------------------------------------------------------------------

    n_class = size(cm%mat,DIM=1)
    
    allocate( cm%act_total(n_class) )
    allocate( cm%prd_total(n_class) )

    cm%tru_total = 0
    cm%off_total = 0

    do col=1,n_class
       cm%prd_total(col) = 0
       do row=1,n_class
          cm%prd_total(col) = cm%prd_total(col) + cm%mat(row,col)
       end do
       cm%tru_total = cm%tru_total + cm%mat(col,col)
       cm%off_total = cm%off_total + cm%mat(col,n_class-col+1)
    end do

    cm%total = 0
    do row=1,n_class
       cm%act_total(row) = 0
       do col=1,n_class
          cm%act_total(row) = cm%act_total(row) + cm%mat(row,col)
       end do
       cm%total = cm%total + cm%act_total(row)
    end do


  end subroutine cm_compute_metrics


    !/ =====================================================================================
  subroutine cm_check_labels( cm )
    !/ ----------------------------------------------------------------------------------
    !!
    !/ ----------------------------------------------------------------------------------
    implicit none
    type(confusion_matrix), intent(inout) :: cm
    !/ ----------------------------------------------------------------------------------
    integer :: i, n
    !/ ----------------------------------------------------------------------------------

    n = size(cm%mat,DIM=1)

    if ( .not.allocated( cm%long_name ) ) then
       allocate( cm%long_name(n) )
       do i=1,n
          cm%long_name(i) = achar(64+i) // 'xxxxxxx '
       end do
    end if
    
    if ( .not.allocated( cm%short_name ) ) then
       allocate( cm%short_name(n) )
       do i=1,n
          cm%short_name(i) = achar(64+i) // achar(64+i) // achar(64+i)
       end do
    end if

  end subroutine cm_check_labels
  
  !/ =====================================================================================
  subroutine cm_load_int_exemplars( cm, A, P, LABELS, NCLS )
    !/ ----------------------------------------------------------------------------------
    !!
    !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    integer,           intent(in) :: A(:)  !! Actual
    integer,           intent(in) :: P(:)  !! Predeicted
    integer, optional, intent(in) :: LABELS(:)
    integer, optional, intent(in) :: NCLS
    !/ ----------------------------------------------------------------------------------
    integer :: i, n, n_class, row, col
    integer,allocatable :: tlab(:)
    !/ ----------------------------------------------------------------------------------
    n_class = 0
    if ( present( LABELS ) ) then
       n_class = size( LABELS )
       if ( present( NCLS ) ) then
          if ( NCLS.lt.n_class ) then
             n_class = NCLS
          end if
       end if
       allocate( tlab(n_class) )
       do i =1,n_class
          tlab(i) = LABELS(i)
       end do
    else
       if ( present( NCLS ) ) then
          n_class = NCLS
          allocate( tlab(n_class) )
          do i =1,n_class
             tlab(i) = i
          end do
       else
          write(ERROR_UNIT,*) 'You must have either LABELS or NCLS'
          goto 999
       end if
    end if

    if ( 0.eq.n_class ) then
       write(ERROR_UNIT,*) 'Number of classes was not assigned'
       goto 999
    end if

    if ( allocated( cm%mat ) ) then
       deallocate( cm%mat )
    end if

    allocate( cm%mat(n_class,n_class) )

    cm%mat = 0

    n = MIN( size(A), size(P) )

    do i=1,n
       row = findloc( tlab, A(i), DIM=1 )
       if ( 0.lt.row ) then
          col = findloc( tlab, P(i), DIM=1 )
          if ( 0.lt.col ) then
             cm%mat(row,col) = cm%mat(row,col) + 1
          end if
       end if
    end do

    cm%non_class_actual  = 0
    cm%non_class_predict = 0

    do i=1,n
       row = findloc( tlab, A(i), DIM=1 )
       col = findloc( tlab, P(i), DIM=1 )
       if ( 0.ge.row ) then
          cm%non_class_actual = cm%non_class_actual + 1
       end if
       if ( 0.ge.col ) then
          cm%non_class_predict = cm%non_class_predict + 1
       end if
    end do

    deallocate( tlab )
    
    call cm_compute_metrics( cm )
    call cm_check_labels( cm )
    
999 continue

  end subroutine cm_load_int_exemplars


  

  !/ =====================================================================================
  subroutine cm_load_char_exemplars( cm, A, P, LABELS, NCLS )
    !/ ----------------------------------------------------------------------------------
    !!
    !/ ----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    character(1), intent(in) :: A(:)  !! Actual
    character(1), intent(in) :: P(:)  !! Predeicted
    character(1), optional, intent(in) :: LABELS(:)
    integer, optional, intent(in) :: NCLS
    !/ ----------------------------------------------------------------------------------
    integer :: i, n, n_class, row, col
    character(1), allocatable :: tlab(:)
    !/ ----------------------------------------------------------------------------------
    n_class = 0
    if ( present( LABELS ) ) then
       n_class = size( LABELS )
       if ( present( NCLS ) ) then
          if ( NCLS.lt.n_class ) then
             n_class = NCLS
          end if
       end if
       allocate( tlab(n_class) )
       do i =1,n_class
          tlab(i) = LABELS(i)
       end do
    else
       if ( present( NCLS ) ) then
          n_class = NCLS
          allocate( tlab(n_class) )
          do i =1,n_class
             tlab(i) = achar(64+i)
          end do
       else
          write(ERROR_UNIT,*) 'You must have either LABELS or NCLS'
          goto 999
       end if
    end if

    if ( 0.eq.n_class ) then
       write(ERROR_UNIT,*) 'Number of classes was not assigned'
       goto 999
    end if

    if ( allocated( cm%mat ) ) then
       deallocate( cm%mat )
    end if

    allocate( cm%mat(n_class,n_class) )

    cm%mat = 0

    n = MIN( size(A), size(P) )

     cm%non_class_actual  = 0
     cm%non_class_predict = 0

    do i=1,n
       row = findloc( tlab, A(i), DIM=1 )
       if ( 0.lt.row ) then
          col = findloc( tlab, P(i), DIM=1 )
          if ( 0.lt.col ) then
             cm%mat(row,col) = cm%mat(row,col) + 1
          else
             cm%non_class_predict = cm%non_class_predict + 1
          end if
       else
          cm%non_class_actual = cm%non_class_actual + 1
       end if
    end do

    deallocate( tlab )
    
    call cm_compute_metrics( cm )
    call cm_check_labels( cm )

999 continue

  end subroutine cm_load_char_exemplars


  !/ =====================================================================================
  subroutine cm_show_matrix( cm, UNIT, FILE, IOSTAT )
    !! -----------------------------------------------------------------------------------
    !   !
    !100!      Test Matrix
    !   ! 
    !110!      Prediction                      +--       8
    !120!A       Target    Neutral   Clutter  /
    !130!c     +---------+---------+---------+
    !140!t TGT |       4 |       2 |       1 |           7
    !130!u     +---------+---------+---------+
    !140!a NEU |       1 |       5 |       3 |           9
    !130!l     +---------+---------+---------+
    !140!  CLU |       2 |       0 |       6 |           8
    !130!      +---------+---------+---------+
    !150!              7         7        10  \
    !160!                                      +--      15
    !170!  Total     Samples  = 24
    !170!  Actual    Unknowns = 2
    !170!  Predicted Unknowns = 3
    !
!! -----------------------------------------------------------------------------------
    use file_tools, only : WriteUnit
    implicit none
    class(confusion_matrix),    intent(inout) :: cm
    character(len=*), optional, intent(in)    :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)    :: UNIT   !! file unit for an open unit
    integer,          optional, intent(out)   :: IOSTAT !! error return.
    !! -----------------------------------------------------------------------------------
    integer :: un, r, c, n
    character(:), allocatable :: sep_line
    character(:), allocatable :: fill
    character(:), allocatable :: top_line
    character(:), allocatable :: bottom_line
    character(1), parameter :: ACT(6) = [ 'A','c','t','u','a','l' ]
    !! -----------------------------------------------------------------------------------
    n = size(cm%mat,DIM=1)

    un = WriteUnit( UNIT=UNIT, FILE=FILE, IOSTAT=IOSTAT )

    sep_line = '+'
    fill     = ''
    top_line = ACT(1) // '     '
    bottom_line = '       '
    do c=1,n
       sep_line = sep_line // '----------+'
       fill = fill // '           '
       top_line = top_line // '  ' // cm%long_name(c)%chars(1:9)
       bottom_line = bottom_line // '           '
    end do
    bottom_line = bottom_line // ' +-- '
    
    !! -----------------------------------------------------------------------------------

    if ( 0.lt.len(cm%title) ) then
       write(un,100) cm%title%chars
    end if

    write(un,*)

    write(un,110) fill(8:), cm%off_total

    write(un,120) top_line

    write(un,130) ACT(2), sep_line
    do r=1,n
       if ( r.gt.2 ) then
          write(un,140,ADVANCE='no') ' ',cm%short_name(r)%chars,(cm%mat(r,c),c=1,n)
          write(un,142) cm%act_total(r)
          write(un,130) ' ',sep_line
       else
          write(un,140,ADVANCE='no') ACT(2*r+1),cm%short_name(r)%chars,(cm%mat(r,c),c=1,n)
          write(un,142) cm%act_total(r)
          write(un,130) ACT(2*r+2),sep_line
       end if
    end do

    write(un,150,ADVANCE='no') (cm%prd_total(c),c=1,n)
    write(un,152) achar(92)
    
    write(un,160) bottom_line, cm%tru_total
    
    write(un,170) 'Total     Samples ', cm%total
    if ( 0.lt.cm%non_class_actual ) then
       write(un,170) 'Actual    Unknowns', cm%non_class_actual
    end if
    
    if ( 0.lt.cm%non_class_predict ) then
       write(un,170) 'Predicted Unknowns', cm%non_class_predict
    end if
    
100 format( 6X, A )
110 format( 5X,'Prediction',A,'+-- ', I0 )
120 format( A,' /' )
130 format( A,5X,A )
140 format( A,1X,A3,' |',*(I9,1X,'|') )
142 format( 5X,I0 )
150 format( 5X,*(I11))
152 format( 2X,A )
160 format( A,I0 ) 
170 format( A,' = ',I0 ) 

  end subroutine cm_show_matrix

  
  !/ =====================================================================================
  subroutine cm_show_metrics( cm, UNIT, FILE, IOSTAT )
    !! -----------------------------------------------------------------------------------
    use file_tools, only : WriteUnit
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    character(len=*), optional, intent(in)  :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)  :: UNIT   !! file unit for an open unit
    integer,          optional, intent(out) :: IOSTAT !! error return.
    !! -----------------------------------------------------------------------------------
    integer  :: un, i, n
    real(dp) :: x, ppv, tpr, f1s, ppv_mu, tpr_mu, f1s_mu
    !! -----------------------------------------------------------------------------------
    n = size(cm%mat,DIM=1)

    un = WriteUnit( UNIT=UNIT, FILE=FILE, IOSTAT=IOSTAT )

    write(un,100)
    write(un,200)

    ppv_mu = D_ZERO
    tpr_mu = D_ZERO
    f1s_mu = D_ZERO
    
    do i=1,n
       x = real( cm%mat(i,i), dp )
       ppv = x / real( cm%prd_total(i), dp )
       tpr = x / real( cm%act_total(i), dp )
       f1s = D_TWO* ppv * tpr / (ppv + tpr)
       write(un,300) cm%long_name(i)%chars, ppv, tpr, f1s, cm%act_total(i)
       ppv_mu = ppv_mu + ppv
       tpr_mu = tpr_mu + tpr
       f1s_mu = f1s_mu + f1s
    end do

       ppv_mu = ppv_mu / real(n,dp)
       tpr_mu = tpr_mu / real(n,dp)
       f1s_mu = f1s_mu / real(n,dp)

       write(un,200)
    write(un,300) 'avg/total', ppv_mu, tpr_mu, f1s_mu, cm%total
    
    
    100 format( '           | Precision | Recall  | F1-Score |  Support' )
    200 format( '-----------+-----------+---------+----------+------------' )
    300 format( ' ',A9,' |  ',F7.4,'  | ',F7.4,' | ',F7.4,'  | ', I10 )
  end subroutine cm_show_metrics

  
  !/ =====================================================================================
  subroutine cm_show_all( cm, UNIT, FILE, IOSTAT )
    !! -----------------------------------------------------------------------------------
    implicit none
    class(confusion_matrix), intent(inout) :: cm
    character(len=*), optional, intent(in)  :: FILE   !! path to an new or existing file.
    integer,          optional, intent(in)  :: UNIT   !! file unit for an open unit
    integer,          optional, intent(out) :: IOSTAT !! error return.
    !! -----------------------------------------------------------------------------------

    call cm%show_matrix(  UNIT=UNIT, FILE=FILE, IOSTAT=IOSTAT )
    call cm%show_metrics( UNIT=UNIT, FILE=FILE, IOSTAT=IOSTAT )

  end subroutine cm_show_all
  

  
end module confusion_mod

!/ =======================================================================================
!/ **                             C O N F U S I O N _ M O D                             **
!/ ======================================================================== END FILE =====
