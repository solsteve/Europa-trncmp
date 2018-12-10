!/ ====================================================================== BEGIN FILE =====
!/ **                            E X E M P L A R _ C L A S S                            **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2017, Stephen W. Soliday                                           **
!/ **                      stephen.soliday@trncmp.org                                   **
!/ **                      http://research.trncmp.org                                   **
!/ **                                                                                   **
!/ **  -------------------------------------------------------------------------------  **
!/ **                                                                                   **
!/ **  This program is free software: you can redistribute it and/or modify it under    **
!/ **  the terms of the GNU General Public License as published by the Free Software    **
!/ **  Foundation, either version 3 of the License, or (at your option)                 **
!/ **  any later version.                                                               **
!/ **                                                                                   **
!/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
!/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
!/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
!/ **                                                                                   **
!/ **  You should have received a copy of the GNU General Public License along with     **
!/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
!/ **                                                                                   **
!/ ----- Modification History ------------------------------------------------------------
!
!> @brief   Exemplar Data Object.
!!
!! @details Provides the interface and procedures for readingand writing ASCII formated
!!          exemplar data.
!!
!! @author  Stephen W. Soliday
!! @date    2017-06-04
!
!/ =======================================================================================
module exemplar_class
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tlogger
  use file_tools
  implicit none

  !/ =====================================================================================
  type :: ExemplarMeta
     !/ ----------------------------------------------------------------------------------
     integer :: n_sample  =  0
     integer :: n_x       =  0
     integer :: n_y       =  0

   contains

     procedure :: toString => emeta_to_string

     final :: emeta_destroy

  end type ExemplarMeta
  
  !/ =====================================================================================
  type :: ExemplarPair
     !/ ----------------------------------------------------------------------------------
     real(dp), pointer :: X(:,:) => null()
     real(dp), pointer :: Y(:,:) => null()

   contains

     procedure, private :: expair_get_transpose_alloc
     procedure, private :: expair_get_transpose_pointer
     procedure, private :: expair_set_transpose

     generic :: getTranspose => expair_get_transpose_alloc, expair_get_transpose_pointer
     generic :: setTranspose => expair_set_transpose

     final :: epair_destroy

  end type ExemplarPair

!   private :: internal_read_single
!   private :: internal_write_single
!   private :: internal_read_pair
!   private :: internal_write_pair
  
 
  !/ -------------------------------------------------------------------------------------
  interface create
     !/ ----------------------------------------------------------------------------------
     module procedure :: emeta_create
     module procedure :: epair_create
  end interface create

  !/ -------------------------------------------------------------------------------------
  interface ExemplarMeta
     !/ ----------------------------------------------------------------------------------
     module procedure :: emeta_allocate
  end interface ExemplarMeta

  !/ -------------------------------------------------------------------------------------
  interface ExemplarPair
     !/ ----------------------------------------------------------------------------------
     module procedure :: epair_allocate
  end interface ExemplarPair


  
  !/ -------------------------------------------------------------------------------------
  interface read_meta
     !/ ----------------------------------------------------------------------------------
     module procedure :: emeta_read
  end interface read_meta

  !/ -------------------------------------------------------------------------------------
  interface write_meta
     !/ ----------------------------------------------------------------------------------
     module procedure :: emeta_write
  end interface write_meta


  !/ -------------------------------------------------------------------------------------
  interface write
     !/ ----------------------------------------------------------------------------------
     module procedure :: exemp_write_single
  end interface write


  !/ -------------------------------------------------------------------------------------
  interface write_pair
     !/ ----------------------------------------------------------------------------------
     module procedure :: exemp_write_pair
  end interface write_pair


  !/ -------------------------------------------------------------------------------------
  interface read
     !/ ----------------------------------------------------------------------------------
     module procedure :: exemp_read_single
  end interface read


  !/ -------------------------------------------------------------------------------------
  interface read_pair
     !/ ----------------------------------------------------------------------------------
     module procedure :: exemp_read_pair
  end interface read_pair



  !/ -------------------------------------------------------------------------------------
  interface SingleTranspose
     !/ ----------------------------------------------------------------------------------
     module procedure :: single_transpose_noalloc
  end interface SingleTranspose


  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  !> @brief Constructor.
  !! @param[in,out] mt reference to an ExemplarMeta object.
  !! @param[in]     R  optional number of rows.
  !! @param[in]     X  optional number of inputs.
  !! @param[in]     Y  optional number of outputs.
  !!
  !! 
  !/ -------------------------------------------------------------------------------------
  subroutine emeta_create( mt, R, X, Y )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarMeta), intent(inout) :: mt
    integer, optional,  intent(in)    :: R
    integer, optional,  intent(in)    :: X
    integer, optional,  intent(in)    :: Y
    !/ -----------------------------------------------------------------------------------

    mt%n_sample = 0
    mt%n_x      = 0
    mt%n_y      = 0

    if ( present( R ) ) mt%n_sample = R
    if ( present( X ) ) mt%n_x      = X
    if ( present( Y ) ) mt%n_y      = Y
    
  end subroutine emeta_create

  !/ =====================================================================================
  !> @brief Constructor.
  !! @param[in]     R  optional number of rows.
  !! @param[in]     X  optional number of inputs.
  !! @param[in]     Y  optional number of outputs.
  !! @return pointer to a new ExemplarMeta object.
  !/ -------------------------------------------------------------------------------------
  function emeta_allocate( R, X, Y ) result( mt )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ExemplarMeta), pointer      :: mt
    integer, optional,  intent(in)    :: R
    integer, optional,  intent(in)    :: X
    integer, optional,  intent(in)    :: Y
    !/ -----------------------------------------------------------------------------------

    allocate( mt )
    call emeta_create( mt, R, X, Y )

  end function emeta_allocate

  
  !/ =====================================================================================
  !> @brief Destructor.
  !! @param[in,out] mt reference to an ExemplarMeta object.
  !/ -------------------------------------------------------------------------------------
  subroutine emeta_destroy( mt )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarMeta), intent(inout) :: mt
    !/ -----------------------------------------------------------------------------------

    mt%n_sample = 0
    mt%n_x      = 0
    mt%n_y      = 0

  end subroutine emeta_destroy


  !/ =====================================================================================
  !> @brief Destructor.
  !! @param[in,out] self reference to this ExemplarMeta object.
  !! @return string number of samples, inputs and outputs.
  !/ -------------------------------------------------------------------------------------
  function emeta_to_string( self ) result( str )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(len=:),    allocatable   :: str
    class(ExemplarMeta), intent(inout) :: self
    !/ -----------------------------------------------------------------------------------
    character(32) :: work
    !/ -----------------------------------------------------------------------------------

    write( work, 10 ) self%n_sample, self%n_x, self%n_y

    str = trim(adjustl(work))

10  format(I0,1X,I0,1X,I0)

  end function emeta_to_string

  
  !/ =====================================================================================
  !> @brief Constructor.
  !! @param[in,out] ep reference to an ExemplarPair object.
  !! @param[in]     XA  optional number of inputs.
  !! @param[in]     YA  optional number of outputs.
  !! @param[out]    ERR optional error return value.
  !!
  !! 
  !/ -------------------------------------------------------------------------------------
  subroutine epair_create( ep, XA, YA, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarPair),         intent(inout) :: ep
    real(dp), optional, target, intent(inout) :: XA(:,:)
    real(dp), optional, target, intent(inout) :: YA(:,:)
    integer,  optional,         intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    nullify( ep%X )
    nullify( ep%Y )

    if ( present( XA ) ) then
       ep%X => XA
    end if

    if ( present( YA ) ) then
       ep%Y => YA
    end if

    if ( associated( ep%X ) .and. associated( ep%Y ) ) then
       if ( size( ep%X, dim=2 ) .ne. size( ep%Y, dim=2 ) ) then
          write(*,*) size( ep%X, dim=2 ), size( ep%Y, dim=2 )
          if ( report ) then
             call log_error( 'ExamplarPair: X & Y do not have the same number of samples' )
          end if
          ier = 1
       end if
    end if

    if ( present( ERR ) ) ERR = ier

  end subroutine epair_create

  
  !/ =====================================================================================
  !> @brief Constructor.
  !! @param[in]     XA  optional number of inputs.
  !! @param[in]     YA  optional number of outputs.
  !! @param[out]    ERR optional error return value.
  !! @return pointer to a new ExemplarPair object.
  !/ -------------------------------------------------------------------------------------
  function epair_allocate( XA, YA, ERR ) result( ep )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ExemplarPair), pointer              :: ep
    real(dp), optional, target, intent(inout) :: XA(:,:)
    real(dp), optional, target, intent(inout) :: YA(:,:)
    integer,  optional,         intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------

    allocate( ep )
    call epair_create( ep, XA, YA, ERR )

  end function epair_allocate

  
  !/ =====================================================================================
  !> @brief Destructor.
  !! @param[in,out] ep reference to an ExemplarPair object.
  !/ -------------------------------------------------------------------------------------
  subroutine epair_destroy( ep )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarPair), intent(inout) :: ep
    !/ -----------------------------------------------------------------------------------

    if ( associated( ep%X ) ) deallocate( ep%X )
    if ( associated( ep%Y ) ) deallocate( ep%Y )

    nullify( ep%X )
    nullify( ep%Y )

  end subroutine epair_destroy


  !/ =====================================================================================
  !< @brief Transpose
  !! @param[in,out] T reference to the transposed output.
  !! @param[in,out] S reference to a source matrix.
  !/ -------------------------------------------------------------------------------------
  subroutine single_transpose_noalloc( T, S, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(inout) :: T(:,:)
    real(dp), intent(inout) :: S(:,:)
    integer, optional, intent(out) :: ERR
    !/ -----------------------------------------------------------------------------------
    integer :: ier, i, j, n, m
    logical :: report
    !/ -----------------------------------------------------------------------------------

    ier = 0
    report = .true.
    if ( present( ERR ) ) report = .false.
    
    n = size(S,1)
    m = size(S,2)
    
    if ( n.ne.size(T,2) ) then
       if ( report ) then
          call log_error( 'Transpose: Rows of source do not match Columns of destination' )
       end if
       ier = 1
       goto 999
    end if

    if ( m.ne.size(T,1) ) then
       if ( report ) then
          call log_error( 'Transpose: Columns of source do not match Rows of destination' )
       end if
       ier = 2
       goto 999
    end if

    do i=1,n
       do j=1,m
          T(j,i) = S(i,j)
       end do
    end do

999 continue

    if ( present( ERR ) ) ERR = ier

  end subroutine single_transpose_noalloc










  

  !/ =====================================================================================
  !> @brief Get Transpose.
  !! @param[in,out] self reference to this ExemplarPair object.
  !! @param[in,out] pX   pointer to first  part array.
  !! @param[in,out] pY   pointer to second part array.
  !! @param[out]    ERR  optional error reporting.
  !!
  !! @note it is the user's responsibility to deallocate X and Y.
  !/ -------------------------------------------------------------------------------------
  subroutine expair_get_transpose_pointer( self, pX, pY, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ExemplarPair), intent(inout) :: self
    real(dp), pointer,   intent(inout) :: pX(:,:)
    real(dp), pointer,   intent(inout) :: pY(:,:)
    integer,  optional,  intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier !, i, j, ni, nj
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    if ( associated( pX ) ) then
       if ( size(pX,1).ne.size(self%X,2) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - X is already associated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 1
          goto 999
       end if
       if ( size(pX,2).ne.size(self%X,1) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - X is already associated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 2
          goto 999
       end if
    else
       allocate( pX( size(self%X,2), size(self%X,1) ) )
    end if


    if ( associated( pY ) ) then
       if ( size(pY,1).ne.size(self%Y,2) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - Y is already associated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 1
          goto 999
       end if
       if ( size(pY,2).ne.size(self%Y,1) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - Y is already associated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 2
          goto 999
       end if
    else
       allocate( pY( size(self%Y,2), size(self%Y,1) ) )
    end if


    !/ -----------------------------------------------------------------------------------

    call single_transpose_noalloc( pX, self%X )
    call single_transpose_noalloc( pY, self%Y )
    
!    ni = size(pX,1)
!    nj = size(pX,2)
!    do concurrent( i=1:ni, j=1:nj )
!       pX(i,j) = self%X(j,i)
!    end do

!    ni = size(pY,1)
!    nj = size(pY,2)
!    do concurrent( i=1:ni, j=1:nj )
!       pY(i,j) = self%Y(j,i)
!    end do

    !/ -----------------------------------------------------------------------------------

999 continue
    
    if ( present( ERR ) ) ERR = ier


  end subroutine expair_get_transpose_pointer
  

  !/ =====================================================================================
  !> @brief Get Transpose.
  !! @param[in,out] self reference to this ExemplarPair object.
  !! @param[in,out] X    allocatable first  part array.
  !! @param[in,out] Y    allocatable second part array.
  !! @param[out]    ERR  optional error reporting.
  !!
  !! @note it is the user's responsibility to deallocate X and Y.
  !/ -------------------------------------------------------------------------------------
  subroutine expair_get_transpose_alloc( self, X, Y, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ExemplarPair),   intent(inout) :: self
    real(dp), allocatable, intent(inout) :: X(:,:)
    real(dp), allocatable, intent(inout) :: Y(:,:)
    integer,  optional,     intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier !, i, j, ni, nj
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    if ( allocated( X ) ) then
       if ( size(X,1).ne.size(self%X,2) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - X is already allocated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 1
          goto 999
       end if
       if ( size(X,2).ne.size(self%X,1) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - X is already allocated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 2
          goto 999
       end if
    else
       allocate( X( size(self%X,2), size(self%X,1) ) )
    end if

    if ( allocated( Y ) ) then
       if ( size(Y,1).ne.size(self%Y,2) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - Y is already allocated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 3
          goto 999
       end if
       if ( size(Y,2).ne.size(self%Y,1) ) then
          if ( report ) then
             call log_error( 'exemplar::getTranspose - Y is already allocated' // &
                  &          ' and is the wrong size' )
          end if
          ier = 4
          goto 999
       end if
    else
       allocate( Y( size(self%Y,2), size(self%Y,1) ) )
    end if

    !/ -----------------------------------------------------------------------------------

    call single_transpose_noalloc( X, self%X )
    call single_transpose_noalloc( Y, self%Y )

    !ni = size(X,1)
    !nj = size(X,2)
    !do concurrent( i=1:ni, j=1:nj )
    !   X(i,j) = self%X(j,i)
    !end do

    !ni = size(Y,1)
    !nj = size(Y,2)
    !do concurrent( i=1:ni, j=1:nj )
    !   Y(i,j) = self%Y(j,i)
    !end do

    !/ -----------------------------------------------------------------------------------

999 continue
    
    if ( present( ERR ) ) ERR = ier

  end subroutine expair_get_transpose_alloc



  !/ =====================================================================================
  !> @brief Set Transpose.
  !! @param[in,out] self reference to this ExemplarPair object.
  !! @param[in,out] X    first  part array.
  !! @param[in,out] Y    second part array.
  !!
  !/ -------------------------------------------------------------------------------------
  subroutine expair_set_transpose( self, X, Y )
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(ExemplarPair), intent(inout) :: self
    real(dp),            intent(inout) :: X(:,:)
    real(dp),            intent(inout) :: Y(:,:)
    !/ -----------------------------------------------------------------------------------
    !integer :: i, j, ni, nj
    !/ -----------------------------------------------------------------------------------

    if ( associated( self%X ) ) then
       if (   ( size(X,1).ne.size(self%X,2) ).or. &
            & ( size(X,2).ne.size(self%X,1) ) ) then
          deallocate( self%X )
          allocate( self%X( size(X,2), size(X,1) ) )
       end if
    else
       allocate( self%X( size(X,2), size(X,1) ) )
    end if

    if ( associated( self%Y ) ) then
       if (   ( size(Y,1).ne.size(self%Y,2) ).or. &
            & ( size(Y,2).ne.size(self%Y,1) ) ) then
          deallocate( self%Y )
          allocate( self%Y( size(Y,2), size(Y,1) ) )
       end if
    else
       allocate( self%Y( size(Y,2), size(Y,1) ) )
    end if
    
    !/ -----------------------------------------------------------------------------------

!    ni = size(X,1)
!    nj = size(X,2)
!    do concurrent( i=1:ni, j=1:nj )
!       self%X(j,i) = X(i,j)
!    end do

!    ni = size(Y,1)
!    nj = size(Y,2)
!    do concurrent( i=1:ni, j=1:nj )
!       self%Y(j,i) = Y(i,j)
!    end do

    call single_transpose_noalloc( self%X, X )
    call single_transpose_noalloc( self%Y, Y )

    !/ -----------------------------------------------------------------------------------

  end subroutine expair_set_transpose










  


  !/ =====================================================================================
  !> @brief Read.
  !! @param[in,out] met  reference to the meta data
  !! @param[in]     FILE path to the metat data file.
  !! @param[in]     ERR  optional error return.
  !!
  !! Read the single line meta data from a file: either (nr nx ny) or (nr nx)
  !/ -------------------------------------------------------------------------------------
  subroutine emeta_read( met, FILE, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarMeta), intent(inout) :: met
    character(*),       intent(in)    :: FILE
    integer, optional,  intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier, inf, a, b, c
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    inf = ReadUnit( FILE=FILE, IOSTAT=ier )
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar: read meta - failed to open file', STR=FILE )
       end if
       ier=123
       goto 999
    end if

    a = 0
    b = 0
    c = 0
    
    read(inf,*,IOSTAT=ier) a, b, c

!    write(*,'(I0,1X,A,3(1X,I0))') ier,'|', a, b, c

    if ( 0.eq.ier ) goto 777

    if ((.not.(-1.eq.ier)).and.(.not.(5010.eq.ier))) then
       if ( report ) then
          call log_error( 'Exemplar: read meta - failed to parse', STR=FILE, I4=ier )
          a = 0
          b = 0
          c = 0
       end if
    end if

    if ( 0.eq.a ) then
       if ( report ) then
          call log_error( 'Exemplar: read meta - no values found', STR=FILE, I4=ier )
       end if
       ier=123
       goto 888
    end if

    if ( 0.eq.b ) then
       if ( report ) then
          call log_error( 'Exemplar: read meta - only sample count found', STR=FILE, I4=ier )
       end if
       ier=124
       goto 888
    end if

777 continue

    met%n_sample = a
    met%n_x      = b
    met%n_y      = c
    ier = 0

888 continue

    close( inf )

999 continue

    if ( present( ERR ) ) ERR = ier

  end subroutine emeta_read



  !/ =====================================================================================
  !> @brief Read.
  !! @param[in,out] met  reference to the meta data
  !! @param[in]     FILE path to the metat data file.
  !! @param[in]     ERR  optional error return.
  !!
  !! Read the single line meta data from a file: either (nr nx ny) or (nr nx)
  !/ -------------------------------------------------------------------------------------
  subroutine emeta_write( met, FILE, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarMeta), intent(inout) :: met
    character(*),       intent(in)    :: FILE
    integer, optional,  intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier, outf
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    if ( 0.eq.met%n_sample ) then
       if ( report ) then
          call log_error( 'Exemplar: write meta - number of samples is zero', STR=FILE )
       end if
       ier=1
       goto 999
    end if

    if ( 0.eq.met%n_x ) then
       if ( report ) then
          call log_error( 'Exemplar: write meta - number of input columns is zero', STR=FILE )
       end if
       ier=1
       goto 999
    end if

    outf = WriteUnit( FILE=FILE, IOSTAT=ier )
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar: write meta - failed to open file', STR=FILE )
       end if
       goto 999
    end if

    if ( 0.lt.met%n_y ) then
       write(outf,10,IOSTAT=ier) met%n_sample, met%n_x, met%n_y
    else
       write(outf,20,IOSTAT=ier) met%n_sample, met%n_x
    end if

    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar: write meta - failed to write record', STR=FILE )
       end if
    end if
    
    close( outf )

999 continue

    if ( present( ERR ) ) ERR = ier

10 format( I0,1X,I0,1X,I0 )
20 format( I0,1X,I0 )
    
  end subroutine emeta_write




  !/ =====================================================================================
  !> @brief Internal Read.
  !! @param[in] unit file unit.
  !! @param[in] NS   number of samples.
  !! @param[in] NF   number of fields.
  !! @param[in] ERR  optional error return.
  !! @return pointer to a new array.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  function internal_read_single( unit, NS, NF, ERR ) result( ary )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),          pointer     :: ary(:,:)
    integer,           intent(in)  :: unit
    integer,           intent(in)  :: NS
    integer,           intent(in)  :: NF
    integer, optional, intent(out) :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier, f, s
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    allocate( ary(NF, NS) )
    
    do s=1,NS
       read(unit,*,IOSTAT=ier) (ary(f,s),f=1,NF)
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar: internal single read - failed' )
          end if
          goto 999
       end if
    end do

999 continue
    
    if ( present( ERR ) ) ERR = ier

  end function internal_read_single

  !/ =====================================================================================
  !> @brief Internal Write.
  !! @param[in,out] ary  data array.
  !! @param[in]     unit file unit.
  !! @param[in]     FMT  optional edit descriptor.
  !! @param[in]     ERR  optional error return.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  subroutine internal_write_single( ary, unit, FMT, ERR )  
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(inout) :: ary(:,:)
    integer,                intent(in)    :: unit
    character(*), optional, intent(in)    :: FMT
    integer,      optional, intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical                       :: report
    integer                       :: ier, f, s, ns, nf
    character(64)                 :: work
    character(len=:), allocatable :: sfmt
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    sfmt='ES15.8'
    if ( present( FMT ) ) sfmt=FMT

    nf = size( ary, dim=1 )
    ns = size( ary, dim=2 )

    write( work, 10 ) sfmt, nf, sfmt
    
    do s=1,ns
       write(UNIT=unit,FMT=trim(work),IOSTAT=ier) ary(1,s), (ary(f,s),f=2,nf)
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar: internal single write - failed' )
          end if
          goto 999
       end if
    end do
    
999 continue
    
    if ( present( ERR ) ) ERR = ier

10  format( '(',A,',',I0,'(1X,',A,'))' )

  end subroutine internal_write_single




  !/ =====================================================================================
  !> @brief Internal Read.
  !! @param[in,out] pair reference to an ExemplarPair object.
  !! @param[in]     unit file unit.
  !! @param[in]     NS   number of samples
  !! @param[in]     NX   number of columns in first  part to read.
  !! @param[in]     NY   number of columns in second part to read.
  !! @param[in]     ERR  optional error return.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  subroutine internal_read_pair( pair, unit, NS, NX, NY, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarPair), intent(inout) :: pair
    integer,            intent(in)    :: unit
    integer,            intent(in)    :: NS
    integer,            intent(in)    :: NX
    integer,            intent(in)    :: NY
    integer, optional,  intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier, f1, f2, s, tt
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    call epair_destroy( pair )

    allocate( pair%X(NX, NS) )
    if ( 0.lt.NY ) then
       allocate( pair%Y(NY, NS) )
    end if

    tt = 0
    
    do s=1,NS
       if ( 0.lt.NY ) then
          read(unit,*,IOSTAT=ier) (pair%X(f1,s),f1=1,NX), (pair%Y(f2,s),f2=1,NY)
          tt = 1
       else
          read(unit,*,IOSTAT=ier) (pair%X(f1,s),f1=1,NX)
          tt = 2
       end if
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar: internal pair read - failed' )
          end if
          goto 999
       end if
    end do

999 continue

    if ( present( ERR ) ) ERR = ier

  end subroutine internal_read_pair




  !/ =====================================================================================
  !> @brief Internal Write.
  !! @param[in,out] X    data array containing the first  part data to be written.
  !! @param[in,out] Y    data array containing the second part data to be written.
  !! @param[in]     unit file unit.
  !! @param[in]     FMT  optional edit descriptor.
  !! @param[in]     FMT2 optional second part edit descriptor.
  !! @param[in]     ERR  optional error return.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  subroutine internal_write_pair( X, Y, unit, FMT, FMT2, ERR )  
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(inout) :: X(:,:)
    real(dp),               intent(inout) :: Y(:,:)
    integer,                intent(in)    :: unit
    character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: FMT2
    integer,      optional, intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical                       :: report
    integer                       :: ier, f1, f2, s, ns, n1, n2
    character(64)                 :: work
    character(len=:), allocatable :: sfmt1
    character(len=:), allocatable :: sfmt2
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    sfmt1='ES15.8'
    if ( present( FMT ) ) sfmt1=FMT

    sfmt2=sfmt1
    if ( present( FMT2 ) ) sfmt2=FMT2

    n1 = size( X, dim=1 )
    n2 = size( Y, dim=1 )
    ns = size( X, dim=2 )

    if ( ns.ne.size( Y, dim=2 ) ) then
          if ( report ) then
             call log_error( 'Exemplar: internal pair write - arrays have ' // &
                  &          'different number of samples' )
          end if
          ier = 1
          goto 999
    end if

    write( work, 10 ) n1, sfmt1, n2, sfmt2

    do s=1,ns
       write(UNIT=unit,FMT=trim(work),IOSTAT=ier) (X(f1,s),f1=1,n1), (Y(f2,s),f2=1,n2)
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar: internal pair write - failed' )
          end if
          goto 999
       end if
    end do
    
999 continue
    
    if ( present( ERR ) ) ERR = ier

10  format( '(',I0,'(',A,',1X),',I0,'(1X,',A,'))' )

  end subroutine internal_write_pair



  !/ =====================================================================================
  !> @brief Write.
  !! @param[in,out] DATA single data array.
  !! @param[in]     FILE path to the data file.
  !! @param[in]     META optional path to a meta file.
  !! @param[in]     MODE optional mode 'H' - header, 'N' - no header.
  !! @param[in]     FMT  optional edit descriptor.
  !! @param[in]     ERR  optional error return.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  subroutine exemp_write_single( DATA, FILE, META, MODE, FMT, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(inout) :: DATA(:,:)
    character(*),           intent(in)    :: FILE
    character(*), optional, intent(in)    :: META
    character(1), optional, intent(in)    :: MODE
    character(*), optional, intent(in)    :: FMT
    integer,      optional, intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical :: report
    integer :: ier, ns, nx, outf
    type(ExemplarMeta) :: met
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    nx = size( DATA, dim=1 )
    ns = size( DATA, dim=2 )

    if ( present( META ) ) then
       if ( present( MODE ) ) then
          if ( report ) then
             call log_error( 'Exemplar write - META and MODE are not to be used together' )
          end if
          ier = 1
          goto 999
       end if
       call create( met, R=ns, X=nx )
       call emeta_write( met, FILE=META, ERR=ier )
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar write - Cannot write meta data', STR=META )
          end if
          goto 999      
       end if
    else
       if ( present( META ) ) then
          if ( report ) then
             call log_error( 'Exemplar write - MODE and META are not to be used together' )
          end if
          ier = 2
          goto 999
       end if
    end if
    
    outf = WriteUnit( FILE=FILE, IOSTAT=ier )
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar write - Cannot open file for write', STR=FILE )
       end if
       goto 999      
    end if

    if ( present( MODE ) ) then
       if (( 'H'.eq.MODE ).or.( 'h'.eq.MODE )) then
          write(outf,10,IOSTAT=ier) ns, nx 
          if ( 0.ne.ier ) then
             if ( report ) then
                call log_error( 'Exemplar write - Cannot write header', STR=FILE )
             end if
             goto 888      
          end if
       end if
    end if
    
    call internal_write_single( DATA, outf, FMT=FMT, ERR=ier )  
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar write - Cannot write data', STR=FILE )
       end if
    end if  

888 continue

    close( outf )
    
999 continue
    
    if ( present( ERR ) ) ERR = ier

10  format( I0,1X,I0 )

  end subroutine exemp_write_single

  
  !/ =====================================================================================
  !> @brief Write.
  !! @param[in,out] X    reference to part 1 data
  !! @param[in,out] Y    reference to part 2 data
  !! @param[in]     FILE path to the data file.
  !! @param[in]     META optional path to a meta file.
  !! @param[in]     MODE optional mode 'H' - header, 'N' - no header.
  !! @param[in]     FMT  optional edit descriptor.
  !! @param[in]     FMT2 optional second part edit descriptor.
  !! @param[in]     ERR  optional error return.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  subroutine exemp_write_pair( X, Y, FILE, META, MODE, FMT, FMT2, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),               intent(inout) :: X(:,:)
    real(dp),               intent(inout) :: Y(:,:)
    character(*),           intent(in)    :: FILE
    character(*), optional, intent(in)    :: META
    character(1), optional, intent(in)    :: MODE
    character(*), optional, intent(in)    :: FMT
    character(*), optional, intent(in)    :: FMT2
    integer,      optional, intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical            :: report
    integer            :: ier, ns, nx, ny, outf
    type(ExemplarMeta) :: met
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    nx = size( X, dim=1 )
    ny = size( Y, dim=1 )
    ns = size( X, dim=2 )

    if ( ns.ne.size( Y, dim=2 ) ) then
           if ( report ) then
              call log_error( 'Exemplar pair write - X and Y data sets have different ' // &
                   &          'number of samples' )
          end if
          ier = 3
          goto 999
    end if

    if ( present( META ) ) then
       if ( present( MODE ) ) then
          if ( report ) then
             call log_error( 'Exemplar pair write - META and MODE are not to be used together' )
          end if
          ier = 1
          goto 999
       end if
       call create( met, R=ns, X=nx, Y=ny )
       call emeta_write( met, FILE=META, ERR=ier )
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar pair write - Cannot write meta data', STR=META )
          end if
          goto 999      
       end if
    else
       if ( present( META ) ) then
          if ( report ) then
             call log_error( 'Exemplar pair write - MODE and META are not to be used together' )
          end if
          ier = 2
          goto 999
       end if
    end if
    
    outf = WriteUnit( FILE=FILE, IOSTAT=ier )
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar pair write - Cannot open file for write', STR=FILE )
       end if
       goto 999      
    end if

    if ( present( MODE ) ) then
       if (( 'H'.eq.MODE ).or.( 'h'.eq.MODE )) then
          write(outf,10,IOSTAT=ier) ns, nx, ny
          if ( 0.ne.ier ) then
             if ( report ) then
                call log_error( 'Exemplar pair write - Cannot write header', STR=FILE )
             end if
             goto 888      
          end if
       end if
    end if
    
    call internal_write_pair( X, Y, outf, FMT=FMT, FMT2=FMT2, ERR=ier )  
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar pair write - Cannot write data', STR=FILE )
       end if
    end if  

888 continue

    close( outf )
    
999 continue
    
    if ( present( ERR ) ) ERR = ier

10  format( I0,1X,I0,1X,I0 )

 end subroutine exemp_write_pair


  !/ =====================================================================================
  !> @brief Write.
  !! @param[in]     FILE path to the data file.
  !! @param[in]     META optional path to a meta file.
  !! @param[in]     NS   optional number of samples.
  !! @param[in]     NX   optional numper of columns.
  !! @param[in]     ERR  optional error return.
  !! @return a pointer to a new single data array.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  function exemp_read_single( FILE, META, NS, NX, ERR ) result( X )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),     pointer               :: X(:,:)
    character(*),           intent(in)  :: FILE
    character(*), optional, intent(in)  :: META
    integer,      optional, intent(in)  :: NS
    integer,      optional, intent(in)  :: NX
    integer,      optional, intent(out) :: ERR
    !/ -----------------------------------------------------------------------------------
    logical            :: report
    integer            :: ier, n_sam, n_x, inf
    type(ExemplarMeta) :: met
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    n_sam = 0
    n_x   = 0
    
    if ( present( META ) ) then
       call emeta_create( met )
       call emeta_read( met, META, ier )
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar read - unable to open meta file', STR=META )
          end if
          goto 999
       end if
       n_sam = met%n_sample
       n_x   = met%n_x
    else
       if ( present( NS ) ) then
          if ( present( NX ) ) then
             n_sam = NS
             n_x   = NX
          else
             if ( report ) then
                call log_error( 'Exemplar read - NX is required when using NS' )
             end if
             goto 999            
          end if
       end if
    end if

    inf = ReadUnit( FILE=FILE, IOSTAT=ier )
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar read - failed to open file', STR=FILE )
       end if
       goto 999
    end if

    if ( 0.eq.n_sam ) then
       read( inf, *, IOSTAT=ier ) n_sam, n_x
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar read - failed to read header', STR=FILE )
          end if
          goto 888
       end if
   end if

    X => internal_read_single( inf, n_sam, n_x, ERR=ier )

    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar read - failed to read file', STR=FILE )
       end if
    end if
    
888 continue
    
    close( inf )
    
999 continue
    
    if ( present( ERR ) ) ERR = ier

  end function exemp_read_single


  !/ =====================================================================================
  !> @brief Write.
  !! @param[in,out] PAIR reference to an ExemplarPair object.
  !! @param[in]     FILE path to the data file.
  !! @param[in]     META optional path to a meta file.
  !! @param[in]     NS   optional number of samples.
  !! @param[in]     NX   optional number of columns in first part.
  !! @param[in]     NY   optional number of columns in second part.
  !! @param[in]     ERR  optional error return.
  !! @return a pointer to a new single data array.
  !!
  !! @note the row/first index is the field, col/second index is the sample.
  !/ -------------------------------------------------------------------------------------
  subroutine exemp_read_pair( PAIR, FILE, META, NS, NX, NY, ERR )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(ExemplarPair),     intent(inout) :: PAIR
    character(*),           intent(in)    :: FILE
    character(*), optional, intent(in)    :: META
    integer,      optional, intent(in)    :: NS
    integer,      optional, intent(in)    :: NX
    integer,      optional, intent(in)    :: NY
    integer,      optional, intent(out)   :: ERR
    !/ -----------------------------------------------------------------------------------
    logical            :: report
    integer            :: ier, n_sam, n_x, n_y, inf
    type(ExemplarMeta) :: met
    character(128)     :: line_buffer
    !/ -----------------------------------------------------------------------------------

    ier    = 0
    report = .true.
    if ( present( ERR ) ) report = .false.

    n_sam = 0
    n_x   = 0
    n_y   = 0

    if ( present( META ) ) then
       call emeta_create( met )
       call emeta_read( met, META, ier )
       if ( 0.ne.ier ) then
          if ( report ) then
             call log_error( 'Exemplar pair read - unable to open meta file', STR=META )
          end if
          goto 999
       end if
       n_sam = met%n_sample
       n_x   = met%n_x
       n_y   = met%n_y
    else
       if ( present( NS ) ) then
          if ( present( NX ) ) then
             if ( present( NY ) ) then
                n_sam = NS
                n_x   = NX
                n_y   = NY
             else
                if ( report ) then
                   call log_error( 'Exemplar pair read - NY is required when using NX & NS' )
                end if
                goto 999            
             end if
          else
             if ( report ) then
                call log_error( 'Exemplar pair read - NX is required when using NS' )
             end if
             goto 999            
          end if
       end if
    end if

     inf = ReadUnit( FILE=FILE, IOSTAT=ier )
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar pair read - failed to open file', STR=FILE )
       end if
       goto 999
    end if

    if ( 0.eq.n_sam ) then
       read( inf, '(A128)', IOSTAT=ier ) line_buffer
       if ( 0.ne.ier ) then
          call log_error( 'Exemplar pair read - line_buffer failed' )
          goto 999
       end if

       read( line_buffer, *, IOSTAT=ier ) n_sam, n_x, n_y
       if ( 5010.eq.ier ) then
          ier = 0
          n_y = 0
       end if
    end if

    call internal_read_pair( PAIR, inf, n_sam, n_x, n_y, ERR=ier )
    
    if ( 0.ne.ier ) then
       if ( report ) then
          call log_error( 'Exemplar pair read - failed to read file', STR=FILE )
       end if
    end if

    close( inf )

999 continue
    
    if ( present( ERR ) ) ERR = ier

  end subroutine exemp_read_pair


end module exemplar_class


!/ =======================================================================================
!/ **                            E X E M P L A R _ C L A S S                            **
!/ =========================================================================== END FILE ==
