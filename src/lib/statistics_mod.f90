!/ ====================================================================== BEGIN FILE =====
!/ **                            S T A T I S T I C S _ M O D                            **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2015-19, Stephen W. Soliday                                        **
!/ **                         stephen.soliday@trncmp.org                                **
!/ **                         http://research.trncmp.org                                **
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
!/ =======================================================================================
module statistics_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2019-11-21
  !! license: GPL
  !!
  !! ## Statistics Module
  !!
  !! Provides a classes for performing running, single, and multi-dimensional statistics.
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use file_tools
  use dice_mod
  use entropy_mod
  use tlogger
  implicit none

  public :: LeastSquares


  !/ =====================================================================================
  type :: running_stats
     !/ ----------------------------------------------------------------------------------
     !/ Provives a minimal memory running average.
     !/ ----------------------------------------------------------------------------------

     real(dp), private :: min_val     = D_MAX_POS  !! Minimum sampled value.
     real(dp), private :: max_val     = D_MAX_NEG  !! Maximum sampled value.
     real(dp), private :: mean_val    = D_ZERO     !! Running mean of the sampled values.
     real(dp), private :: mean_sq_val = D_ZERO     !! Running mean of the squares of the samples.
     logical,  private :: is_first    = .true.     !! Set to true until the first value is sampled.
     integer,  private :: num_samp    = 0          !! Number of values sampled.
     integer,  private :: min_idx     = 0          !! Index of the minimum value sampled.
     integer,  private :: max_idx     = 0          !! Index of the maximum value sampled.

   contains

     procedure :: reset  => running_reset
     procedure :: count  => running_count
     procedure :: minv   => running_minimum_value
     procedure :: maxv   => running_maximum_value
     procedure :: mean   => running_mean
     procedure :: var    => running_variance
     procedure :: stdev  => running_standard_deviation
     procedure :: minidx => running_minimum_index
     procedure :: maxidx => running_maximum_index
     procedure :: report => running_report

     procedure, private :: running_sample_single_real8
     procedure, private :: running_sample_list_real8
     procedure, private :: running_sample_single_real4
     procedure, private :: running_sample_list_real4
     procedure, private :: running_sample_single_int32
     procedure, private :: running_sample_list_int32
     procedure, private :: running_sample_single_int16
     procedure, private :: running_sample_list_int16

     generic   :: sample => running_sample_single_real8, &
          &                 running_sample_list_real8,   &
          &                 running_sample_single_real4, &
          &                 running_sample_list_real4,   &
          &                 running_sample_single_int32, &
          &                 running_sample_list_int32,   &
          &                 running_sample_single_int16, &
          &                 running_sample_list_int16


  end type running_stats




  !/ =====================================================================================
  type :: histogram
     !/ ----------------------------------------------------------------------------------
     !/ Provives a histogram.
     !/ ----------------------------------------------------------------------------------
     integer               :: num_bin  = 0 !! Number of bins
     integer               :: num_samp = 0 !! Number of samples
     integer,  allocatable :: hbin(:)      !! The bins
     real(dp), allocatable :: hctr(:)      !! Numeric center of each bin

   contains

     procedure :: init   => histogram_init
     procedure :: reset  => histogram_reset
     procedure :: count  => histogram_count
     procedure :: bin    => histogram_bin
     procedure :: max    => histogram_max
     procedure :: center => histogram_center
     procedure :: map    => histogram_map

     procedure, private :: histogram_add_integer
     procedure, private :: histogram_add_real;
     procedure, private :: histogram_add_integer_list
     procedure, private :: histogram_add_real_list

     generic :: add => histogram_add_integer,      histogram_add_real,  &
          &            histogram_add_integer_list, histogram_add_real_list

  end type histogram




  !/ =====================================================================================
  type :: single_stats
     !/ ----------------------------------------------------------------------------------
     !! Provives a histogram.
     !/ ----------------------------------------------------------------------------------
     integer,  private :: t_num_var !!  Number of Variables.
     integer,  private :: t_count   !!  Number of samples.
     logical,  private :: t_level1  !!  Flag indicating that level one is complete.
     logical,  private :: t_level2  !!  Flag indicating that level two is complete.

     !/ ----- level 1 ---------------------------------------------------------------------

     real(dp), private :: t_minv    !!  Minimum sample value.
     real(dp), private :: t_maxv    !!  Maximum sample value.
     integer,  private :: t_minidx  !!  Index of minimum value.
     integer,  private :: t_maxidx  !!  Index of maximum value.
     real(dp), private :: t_mean    !!  Mean value of sample.
     real(dp), private :: t_var     !!  Sample variance.

     !/ ----- level 2 ---------------------------------------------------------------------

     real(dp), private :: t_std     !!  Standard deviation of the mean.
     real(dp), private :: t_adev    !!  Absolute deviation of the mean.
     real(dp), private :: t_skew    !!  Sample skew.
     real(dp), private :: t_kurt    !!  Sample kurt.


   contains


     procedure :: count   => single_count
     procedure :: minv    => single_minv
     procedure :: maxv    => single_maxv
     procedure :: minidx  => single_minidx
     procedure :: maxidx  => single_maxidx
     procedure :: mean    => single_mean
     procedure :: var     => single_var
     procedure :: std     => single_std
     procedure :: adev    => single_adev
     procedure :: skew    => single_skew
     procedure :: kurt    => single_kurt

     procedure :: init    => single_init
     procedure :: clear   => single_clear
     procedure :: report  => single_report
     procedure :: compile => single_compile
     procedure :: extra   => single_extra

  end type single_stats




  !/ =====================================================================================
  type :: multi_stats
     !/ ----------------------------------------------------------------------------------
     !! Provives a histogram.
     !/ ----------------------------------------------------------------------------------

     integer, private :: t_num_var =  0       !! number of dimensions.
     logical, private :: t_level1  = .false.  !! flag indicating that level one was complete.
     logical, private :: t_level2a = .false.  !! flag indicating that level two A was complete.
     logical, private :: t_level2b = .false.  !! flag indicating that level two B was complete.

     class(single_stats), private, allocatable :: SS(:)

     real(dp), private, allocatable :: covariance(:,:)  !! Covariance  matrix.
     real(dp), private, allocatable :: correlation(:,:) !! Correlation matrix.
     real(dp), private, allocatable :: inv_cov(:,:)     !! Inverse covariance.


   contains


     procedure :: nvar       => multi_num_var
     procedure :: count      => multi_count
     procedure :: minv       => multi_minv
     procedure :: maxv       => multi_maxv
     procedure :: minidx     => multi_minidx
     procedure :: maxidx     => multi_maxidx
     procedure :: mean       => multi_mean
     procedure :: var        => multi_var
     procedure :: std        => multi_std
     procedure :: adev       => multi_adev
     procedure :: skew       => multi_skew
     procedure :: kurt       => multi_kurt

     procedure :: init       => multi_init
     procedure :: clear      => multi_clear
     procedure :: report     => multi_report
     procedure :: compile    => multi_compile
     procedure :: extra      => multi_extra
     procedure :: correlate  => multi_correlate
     procedure :: invert_cov => multi_invert_cov

  end type multi_stats





  !/ =====================================================================================
  type Roulette
     !/ ----------------------------------------------------------------------------------
     integer, private               :: count = 0 !! number of weights
     real(dp), private, allocatable :: weight(:) !! weights
   contains
     procedure, private :: roulette_set_all
     procedure, private :: roulette_set_one
     procedure, private :: roulette_renormalize
     
     procedure :: select  => roulette_select
     procedure :: destroy => roulette_destroy

     procedure :: display => roulette_display

     generic   :: set     => roulette_set_all, roulette_set_one

     final :: roulette_final
     
  end type Roulette



  !/ -------------------------------------------------------------------------------------
  interface mean_variance
     !/ ----------------------------------------------------------------------------------
     module procedure :: fast_mean_variance
  end interface mean_variance


  
  !/ -------------------------------------------------------------------------------------
  interface size
     !/ ----------------------------------------------------------------------------------
     module procedure :: histrogram_size
     module procedure :: roulette_size
  end interface size




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
  !/ =====================================================================================
  subroutine LeastSquares( m, b, x, y, IERR )
    !/ -----------------------------------------------------------------------------------
    !! Calculate Linear Least Squares
    !!
    !! | ierr | meaning                |
    !! | :--: | :--------------------- |
    !! |  0   | success                |
    !! |  1   | arrays not same length |
    !! |  2   | data is singular       |
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp),          intent(out) :: m     !! slope of the line.
    real(dp),          intent(out) :: b     !! Y-intercept.
    real(dp),          intent(in)  :: x(:)  !! independent variable array
    real(dp),          intent(in)  :: y(:)  !! dependent   variable array
    integer, optional, intent(out) :: IERR  !! error return value
    !/ -----------------------------------------------------------------------------------
    real(dp) :: SX, SY, SXY, SXX, S2X, D
    integer  :: i, n, ier
    logical  :: report
    !/ -----------------------------------------------------------------------------------

    report = .true.
    if ( present( IERR ) ) report = .false.
    
    n = size(x)
    m = D_ZERO
    b = D_ZERO

    ier = 0

    if ( n.ne.size(y) ) then
       ier = 1
       if ( report ) then
          call log_error( 'dimensions for x and y must match' )
       end if
       goto 999
    end if
    
    SX  = D_ZERO
    SY  = D_ZERO
    SXY = D_ZERO
    SXX = D_ZERO

    do i=1,n
       SX  = SX  + x(i)
       SY  = SY  + y(i)
       SXY = SXY + ( x(i) * y(i) )
       SXX = SXX + ( x(i) * x(i) )
    end do

    S2X = SX * SX

    D = real(n,dp) * SXX - S2X

    if ( isZero(D) ) then
       ier = 2
       if ( report ) then
          call log_error( 'data has singularity' )
       end if
       goto 999       
    end if

    m = ( ( real(n,dp) * SXY ) - ( SX * SY ) ) / D
    b = ( ( SXX * SY ) - ( SX * SXY ) ) / D
  
999 continue
    
    if ( present( IERR ) ) IERR = ier
    
  end subroutine LeastSquares


  

  !/ =====================================================================================
  subroutine running_reset( dts )
    !/ -----------------------------------------------------------------------------------
    !! Clear the values and start over.
    !! **note** this will not take effect until the first update after calling a reset.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    !/ -----------------------------------------------------------------------------------
    dts%is_first = .true.
  end subroutine running_reset

  !/ =====================================================================================
  function running_count( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Get number of samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object.
    integer                          :: n   !! number of samples collected.
    !/ -----------------------------------------------------------------------------------
    n = dts%num_samp
  end function running_count

  !/ =====================================================================================
  function running_minimum_value( dts ) result( mnv )
    !/ -----------------------------------------------------------------------------------
    !! Get minimum sampled value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object.
    real(dp)                         :: mnv !! minimum sampled value.
    !/ -----------------------------------------------------------------------------------
    mnv = dts%min_val
  end function running_minimum_value

  !/ =====================================================================================
  function running_maximum_value( dts ) result( mxv )
    !/ -----------------------------------------------------------------------------------
    !! Get maximum sampled value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object.
    real(dp)                         :: mxv !! maximum sampled value.
    !/ -----------------------------------------------------------------------------------
    mxv = dts%max_val
  end function running_maximum_value

  !/ =====================================================================================
  function running_mean( dts ) result( mu )
    !/ -----------------------------------------------------------------------------------
    !! Get the mean of the sampled distribution.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object
    real(dp)                         :: mu !! Mean
    !/ -----------------------------------------------------------------------------------
    mu = dts%mean_val
  end function running_mean

  !/ =====================================================================================
  function running_variance( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Get the variance of the sampled distribution.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object
    real(dp)                         :: v   !! Variance
    !/ -----------------------------------------------------------------------------------
    v = dts%mean_sq_val - (dts%mean_val*dts%mean_val)
  end function running_variance

  !/ =====================================================================================
  function running_standard_deviation( dts ) result( sd )
    !/ -----------------------------------------------------------------------------------
    !! Get the mean standard deviation form the norm.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object
    real(dp)                         :: sd  !! Standard deviation
    !/ -----------------------------------------------------------------------------------
    sd = sqrt( dts%var() )
  end function running_standard_deviation

  !/ =====================================================================================
  function running_minimum_index( dts ) result( imn )
    !/ -----------------------------------------------------------------------------------
    !! Get index of the minimum sampled value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object.
    integer                          :: imn !! index of the minimum sampled value.
    !/ -----------------------------------------------------------------------------------
    imn = dts%min_idx
  end function running_minimum_index

  !/ =====================================================================================
  function running_maximum_index( dts ) result( imx )
    !/ -----------------------------------------------------------------------------------
    !!Get index of the maximum sampled value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(in) :: dts !! reference to this running_stats object.
    integer                          :: imx !! index of the maximum sampled value.
    !/ -----------------------------------------------------------------------------------
    imx = dts%max_idx
  end function running_maximum_index

  !/ =====================================================================================
  subroutine running_report( dts, FILE, UNIT, IOSTAT, FMT )
    !/ -----------------------------------------------------------------------------------
    !! Report
    !/ -----------------------------------------------------------------------------------
    use file_tools, only : WriteUnit
    implicit none
    class(running_stats),   intent(in)  :: dts    !! reference to this running_stats object
    character(*), optional, intent(in)  :: FILE   !! full path to file
    integer,      optional, intent(in)  :: UNIT   !! unit to write report
    integer,      optional, intent(out) :: IOSTAT !! error return.
    character(*), optional, intent(in)  :: FMT    !! edit descriptor for real
    !/ -----------------------------------------------------------------------------------
    integer                   :: un, ios
    character(:), allocatable :: rfmt
    character(:), allocatable :: ifmt
    !/ -----------------------------------------------------------------------------------

    rfmt = "(A8,': ',ES11.4)"
    ifmt = "(A8,': ',I0)"

    if ( present( FMT ) ) then
       rfmt = "(A8,': '," // trim(FMT) // ')'
    end if

    un = WriteUnit( FILE=file, UNIT=unit, IOSTAT=ios )

    if (0.eq.ios) then
       write( un, ifmt) 'Num     ', dts%count()
       write( un, rfmt) 'MinValue', dts%minv()
       write( un, rfmt) 'MaxValue', dts%maxv()
       write( un, ifmt) 'MinIndex', dts%minidx()
       write( un, ifmt) 'MaxIndex', dts%maxidx()
       write( un, rfmt) 'Mean    ', dts%mean()
       write( un, rfmt) 'Var     ', dts%var()
       write( un, *)
    end if

    if ( present( FILE ) ) then
       close(un)
    end if

    if ( present( IOSTAT ) ) then
       IOSTAT=ios
    end if
  end subroutine running_report

  !/ =====================================================================================
  subroutine running_sample_single_real8( dts, samp )
    !/ -----------------------------------------------------------------------------------
    !! Sample.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    real(dp), intent(in)                :: samp  !! sample value.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: t, t2, fn, fnp1
    !/ -----------------------------------------------------------------------------------
    if ( dts%is_first ) then

       dts%min_val     = samp
       dts%max_val     = samp
       dts%mean_val    = samp
       dts%mean_sq_val = samp*samp
       dts%is_first    = .false.
       dts%num_samp    = 1
       dts%min_idx     = 1
       dts%max_idx     = 1

    else

       fn   = real(dts%num_samp,dp)
       fnp1 = real(dts%num_samp+1,dp)

       t  = fn * dts%mean_val
       t2 = fn * dts%mean_sq_val 

       dts%mean_val    = (samp      + t  ) / fnp1
       dts%mean_sq_val = (samp*samp + t2 ) / fnp1

       dts%num_samp = dts%num_samp + 1

       if ( samp.lt.dts%min_val ) then
          dts%min_val = samp
          dts%min_idx = dts%num_samp
       end if

       if ( samp.gt.dts%max_val ) then
          dts%max_val = samp
          dts%max_idx = dts%num_samp
       end if

    end if
  end subroutine running_sample_single_real8





  !/ =====================================================================================
  subroutine running_sample_single_real4( dts, samp )
    !/ -----------------------------------------------------------------------------------
    !! Sample.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    real(sp), intent(in)                :: samp  !! sample value.
    !/ -----------------------------------------------------------------------------------
    call dts%running_sample_single_real8( real(samp,dp) )
  end subroutine running_sample_single_real4


  !/ =====================================================================================
  subroutine running_sample_single_int32( dts, samp )
    !/ -----------------------------------------------------------------------------------
    !! Sample.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    integer(int32), intent(in)                :: samp  !! sample value.
    !/ -----------------------------------------------------------------------------------
    call dts%running_sample_single_real8( real(samp,dp) )
  end subroutine running_sample_single_int32


  !/ =====================================================================================
  subroutine running_sample_single_int16( dts, samp )
    !/ -----------------------------------------------------------------------------------
    !! Sample.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    integer(int16), intent(in)                :: samp  !! sample value.
    !/ -----------------------------------------------------------------------------------
    call dts%running_sample_single_real8( real(samp,dp) )
  end subroutine running_sample_single_int16











  !/ =====================================================================================
  subroutine running_sample_list_real8( dts, list )
    !/ -----------------------------------------------------------------------------------
    !! Batch sample a list of values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    real(dp), intent(in) :: list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------

    n = size(list)

    do i=1,n
       call dts%running_sample_single_real8( list(i) )
    end do

  end subroutine running_sample_list_real8


  !/ =====================================================================================
  subroutine running_sample_list_real4( dts, list )
    !/ -----------------------------------------------------------------------------------
    !! Batch sample a list of values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    real(sp), intent(in) :: list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------

    n = size(list)

    do i=1,n
       call dts%running_sample_single_real8( real(list(i),dp) )
    end do

  end subroutine running_sample_list_real4


  !/ =====================================================================================
  subroutine running_sample_list_int32( dts, list )
    !/ -----------------------------------------------------------------------------------
    !! Batch sample a list of values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    integer(int32), intent(in) :: list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------

    n = size(list)

    do i=1,n
       call dts%running_sample_single_real8( real(list(i),dp) )
    end do

  end subroutine running_sample_list_int32


  !/ =====================================================================================
  subroutine running_sample_list_int16( dts, list )
    !/ -----------------------------------------------------------------------------------
    !! Batch sample a list of values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(running_stats), intent(inout) :: dts !! reference to this running_stats object
    integer(int16), intent(in) :: list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i,n
    !/ -----------------------------------------------------------------------------------

    n = size(list)

    do i=1,n
       call dts%running_sample_single_real8( real(list(i),dp) )
    end do

  end subroutine running_sample_list_int16
















  !/ =====================================================================================
  function histrogram_size( hist ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of bins.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(histogram), intent(in) :: hist !! reference to this histogram object.
    integer                     :: n    !! number of bins.
    !/ -----------------------------------------------------------------------------------
    n = hist%num_bin
  end function histrogram_size


  !/ =====================================================================================
  subroutine histogram_init( dts, CENTERS, NUMBER, LOWER, UPPER, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Initialize.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram),                    intent(inout) :: dts        !! reference to this histogram object.
    real(dp),     optional,              intent(in)    :: CENTERS(:) !! list on bin centers.
    integer,      optional,              intent(in)    :: NUMBER     !! number of centers.
    real(dp),     optional,              intent(in)    :: LOWER      !! lower value
    real(dp),     optional,              intent(in)    :: UPPER      !! upper value
    integer,      optional,              intent(out)   :: IERR       !! error number
    character(:), optional, allocatable, intent(out)   :: EMSG       !! error message
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n, cde
    real(dp) :: start, delta
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

    cde = 0
    if ( present( CENTERS ) ) cde = cde + 1
    if ( present( NUMBER  ) ) cde = cde + 2
    if ( present( LOWER   ) ) cde = cde + 4
    if ( present( UPPER   ) ) cde = cde + 8

    if (  1.eq.cde ) goto 100 !! from centers
    if (  2.eq.cde ) goto 200 !! number (0.le.X.le.1)
    if ( 10.eq.cde ) goto 300 !! number (0.le.X.le.upper)
    if ( 14.eq.cde ) goto 400 !! number (lower.le.X.le.upper)

    ! errors

    if (  3.eq.cde ) goto 500 !! center and number used together
    if (  7.eq.cde ) goto 500
    if ( 11.eq.cde ) goto 500
    if ( 15.eq.cde ) goto 500

    if (  5.eq.cde ) goto 600 !! center and lower and/or upper used together
    if (  9.eq.cde ) goto 600
    if ( 13.eq.cde ) goto 600

    if (  4.eq.cde ) goto 700 !! lower and/or upper used without number
    if (  8.eq.cde ) goto 700
    if ( 12.eq.cde ) goto 700

    if (  6.eq.cde ) goto 800 !! lower and number cannot be used

    call standard_error( 'histogram_init: no options were provided', &
         &              1, IERR=IERR, EMSG=EMSG )
    goto 999

    !/ ----- from centers ----------------------------------------------------------------
100 continue
    n        = size(CENTERS)
    dts%num_bin = n
    allocate( dts%hbin(n) )
    allocate( dts%hctr(n) )
    do i=1,n
       dts%hctr(i) = CENTERS(i)
       dts%hbin(i) = 0
    end do
    dts%num_samp = 0
    goto 999

    !/ ----- number (0.le.X.le.1) --------------------------------------------------------
200 continue
    n     = NUMBER
    delta = D_ONE / real(n-1,dp)
    start = D_ZERO
    goto 450

    !/ ----- (0.le.X.le.upper) -----------------------------------------------------------
300 continue
    n     = NUMBER
    delta = UPPER / real(n-1,dp)
    start = D_ZERO
    goto 450

    !/ ----- number (lower.le.X.le.upper) ------------------------------------------------
400 continue
    n           = NUMBER
    delta       = (UPPER-LOWER) / real(n-1,dp)
    start       = LOWER

450 continue

    dts%num_bin = n
    allocate( dts%hbin(n) )
    allocate( dts%hctr(n) )
    dts%hctr(1) = start
    do i=2,n
       dts%hctr(i) = dts%hctr(i-1) + delta
    end do
    dts%num_samp = 0
    goto 999

    !/ ----- center and number used together ---------------------------------------------
500 continue
    call standard_error( 'histogram_init: centers and number can not be used togeather', &
         &              1, IERR=IERR, EMSG=EMSG )
    goto 999

    !/ ----- center and lower and/or upper used together ---------------------------------
600 continue
    call standard_error( 'histogram_init: centers and upper and/or lower can not be used togeather', &
         &              2, IERR=IERR, EMSG=EMSG )
    goto 999

    !/ ----- lower and/or upper used without number --------------------------------------
700 continue
    call standard_error( 'histogram_init: using upper and/or lower requires number', &
         &              2, IERR=IERR, EMSG=EMSG )
    goto 999

    !/ ----- lower and number cannot be used ---------------------------------------------
800 continue
    call standard_error( 'histogram_init: lower and number requires upper', &
         &              2, IERR=IERR, EMSG=EMSG )
999 continue
  end subroutine histogram_init


  !/ =====================================================================================
  subroutine histogram_reset( dts )
    !/ -----------------------------------------------------------------------------------
    !! Reset.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts !! reference to this histogram object.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    do i=1,dts%num_bin
       dts%hbin(i) = 0
    end do
    dts%num_samp = 0

  end subroutine histogram_reset


  !/ =====================================================================================
  function histogram_count( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(in) :: dts !! reference to this histogram object.
    integer                      :: n   !! number of samples added.
    !/ -----------------------------------------------------------------------------------
    n = dts%num_samp
  end function histogram_count


  !/ =====================================================================================
  function histogram_bin( dts, index ) result( h )
    !/ -----------------------------------------------------------------------------------
    !! Number of samples in bin.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(in) :: dts   !! reference to this histogram object.
    integer,          intent(in) :: index !! index.
    integer                      :: h     !! samples in bin at index.
    !/ -----------------------------------------------------------------------------------
    h = dts%hbin(index)
  end function histogram_bin


  !/ =====================================================================================
  function histogram_max( dts ) result( idx )
    !/ -----------------------------------------------------------------------------------
    !! Find bin with maximum count.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts !! reference to this histogram object.
    integer                         :: idx !! index of bin with maximum count.
    !/ -----------------------------------------------------------------------------------
    integer :: i, mxv, x
    !/ -----------------------------------------------------------------------------------
    mxv = dts%hbin(1)
    idx = 1
    do i=2,dts%num_bin
       x = dts%hbin(i)
       if ( x.lt.mxv ) then
          mxv = x
          idx = i
       end if
    end do
  end function histogram_max


  !/ =====================================================================================
  function histogram_center( dts, index ) result( ctr )
    !/ -----------------------------------------------------------------------------------
    !! Center of an indexed bin.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts   !! reference to this histogram object.
    integer,          intent(in)    :: index !! index.
    real(dp)                        :: ctr   !! center of indexed bin.
    !/ -----------------------------------------------------------------------------------
    ctr = dts%hctr(index)
  end function histogram_center


  !/ =====================================================================================
  subroutine histogram_add_integer( dts, index )
    !/ -----------------------------------------------------------------------------------
    !! Increment a bin.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts   !! reference to this histogram object.
    integer,          intent(in)    :: index !! index of bin to increment.
    !/ -----------------------------------------------------------------------------------
    dts%hbin(index) = dts%hbin(index) + 1
  end subroutine histogram_add_integer


  !/ =====================================================================================
  subroutine histogram_add_real( dts, sample )
    !/ -----------------------------------------------------------------------------------
    !! Sample a new value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts    !! reference to this histogram object.
    real(dp),         intent(in)    :: sample !! new sample.
    !/ -----------------------------------------------------------------------------------
    call dts%add( dts%map( sample ) )
  end subroutine histogram_add_real


  !/ =====================================================================================
  subroutine histogram_add_integer_list( dts, index_list )
    !/ -----------------------------------------------------------------------------------
    !! Increment a list of bins.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts !! reference to this histogram object
    integer,          intent(in)    :: index_list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(index_list)
    do i=1,n
       call dts%add(index_list(i))
    end do
  end subroutine histogram_add_integer_list


  !/ =====================================================================================
  subroutine histogram_add_real_list( dts, sample_list )
    !/ -----------------------------------------------------------------------------------
    !! Sample a list of values.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts !! reference to this histogram object
    real(dp),         intent(in)    :: sample_list(:)
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    n = size(sample_list)
    do i=1,n
       call dts%add(sample_list(i))
    end do
  end subroutine histogram_add_real_list


  !/ =====================================================================================
  function histogram_map( dts, sample ) result( index )
    !/ -----------------------------------------------------------------------------------
    !! Find the center closest to the sample
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(histogram), intent(inout) :: dts    !! reference to this histogram object.
    real(dp),         intent(in)    :: sample !! sample value.
    integer                         :: index  !! index of bin with center closest to sample.
    !/ -----------------------------------------------------------------------------------
    real(dp) :: d, d2, mnv
    integer  :: i
    !/ -----------------------------------------------------------------------------------
    d     = sample - dts%hctr(1)
    d2    = d*d
    mnv   = d2
    index = 1

    do i=2,dts%num_bin
       d  = sample - dts%hctr(i)
       d2 = d*d
       if ( d2.lt.mnv ) then
          mnv   = d2
          index = i
       end if
    end do

  end function histogram_map









  !/ =====================================================================================
  function single_count( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    integer                         :: n    !! number of samples.
    !/ -----------------------------------------------------------------------------------
    n = dts%t_count
  end function single_count


  !/ =====================================================================================
  function single_minv( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Minimum sample value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! minimum value of the samples.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_minv
  end function single_minv


  !/ =====================================================================================
  function single_maxv( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Maximum sample value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! maximum value of the samples.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_maxv
  end function single_maxv


  !/ =====================================================================================
  function single_minidx( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Index of minimum value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    integer                         :: n    !! index of minimum value.
    !/ -----------------------------------------------------------------------------------
    n = dts%t_minidx
  end function single_minidx


  !/ =====================================================================================
  function single_maxidx( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Index of maximum value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    integer                         :: n    !! index of maximum value.
    !/ -----------------------------------------------------------------------------------
    n = dts%t_maxidx
  end function single_maxidx


  !/ =====================================================================================
  function single_mean( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Mean value of sample.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! mean value of sample.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_mean
  end function single_mean


  !/ =====================================================================================
  function single_var( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Sample variance.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! sample variance.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_var
  end function single_var


  !/ =====================================================================================
  function single_std( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Standard deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! standard deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_std
  end function single_std


  !/ =====================================================================================
  function single_adev( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Absolute deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! absolute deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_adev
  end function single_adev


  !/ =====================================================================================
  function single_skew( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Sample skew.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! sample skew.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_skew
  end function single_skew


  !/ =====================================================================================
  function single_kurt( dts ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Sample kurt.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(in) :: dts  !! reference to this single_stats object.
    real(dp)                        :: v    !! sample kurt.
    !/ -----------------------------------------------------------------------------------
    v = dts%t_kurt
  end function single_kurt


  !/ =====================================================================================
  subroutine single_init( dts )
    !/ -----------------------------------------------------------------------------------
    !! Initialize the Statistics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(inout) :: dts    !! reference to this single_stats object.
    !/ -----------------------------------------------------------------------------------
    call dts%clear
  end subroutine single_init


  !/ =====================================================================================
  subroutine single_clear( dts )
    !/ -----------------------------------------------------------------------------------
    !! Clear the Statistics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats), intent(inout) :: dts    !! reference to this single_stats object.
    !/ -----------------------------------------------------------------------------------
    dts%t_count   = 0
    dts%t_level1  = .false.
    dts%t_level2  = .false.
    dts%t_minv    = D_ZERO
    dts%t_maxv    = D_ZERO
    dts%t_minidx  = 0
    dts%t_maxidx  = 0
    dts%t_mean    = D_ZERO
    dts%t_var     = D_ZERO
    dts%t_std     = D_ZERO
    dts%t_adev    = D_ZERO
    dts%t_skew    = D_ZERO
    dts%t_kurt    = D_ZERO
  end subroutine single_clear


  !/ =====================================================================================
  subroutine single_report( dts, FILE, UNIT, IOSTAT, FMT, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Report the compiled statistics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats),    intent(inout) :: dts    !! reference to this single_stats object.
    character(*), optional, intent(in)    :: FILE   !! full path to file.
    integer,      optional, intent(in)    :: UNIT   !! unit to write report.
    integer,      optional, intent(out)   :: IOSTAT !! error return.
    character(*), optional, intent(in)    :: FMT    !! edit descriptor for real.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer                   :: un, ios
    character(:), allocatable :: rfmt
    character(:), allocatable :: ifmt
    !/ -----------------------------------------------------------------------------------

    if ( dts%t_level1 ) then
       rfmt = "(A10,': ',ES11.4)"
       ifmt = "(A10,': ',I0)"

       if ( present( FMT ) ) then
          rfmt = "(A10,': '," // trim(FMT) // ')'
       end if

       un = WriteUnit( FILE=file, UNIT=unit, IOSTAT=ios )

       if (0.eq.ios) then
          write( un, ifmt) 'Num.Samp..', dts%count()
          write( un, rfmt) 'Min Value.', dts%minv()
          write( un, rfmt) 'Max Value.', dts%maxv()
          write( un, ifmt) 'Min Index.', dts%minidx()
          write( un, ifmt) 'Max Index.', dts%maxidx()
          write( un, rfmt) 'Mean......', dts%mean()
          write( un, rfmt) 'Variance..', dts%var()
          if ( dts%t_level2 ) then
             write( un, rfmt) 'Std Dev...', dts%std()
             write( un, rfmt) 'Abs Dev...', dts%adev()
             write( un, rfmt) 'Skew......', dts%skew()
             write( un, rfmt) 'Kurt......', dts%kurt()
          end if
       end if

       if ( present( FILE ) ) then
          close(un)
       end if

       if ( present( IOSTAT ) ) then
          IOSTAT=ios
       end if
    else
       call standard_error( 'single_report: no samples were compiled', 1, &
            &               IERR=IOSTAT, EMSG=EMSG )
    end if

  end subroutine single_report


  !/ =====================================================================================
  subroutine single_compile( dts, samples, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compile the statistics data structure for a single dimensional data set.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats),                 intent(inout) :: dts         !! reference to this single_stats object.
    real(dp),                            intent(in)    :: samples(:)  !! array of sample values.
    integer,      optional,              intent(out)   :: IERR        !! return error.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer  :: i, num_samples
    real(dp) :: fn, x
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

    num_samples = size(samples)

    dts%t_level1 = .false.

    dts%t_mean   = D_ZERO
    dts%t_minv   = D_MAX_POS
    dts%t_maxv   = D_MAX_NEG
    dts%t_minidx = num_samples + 1
    dts%t_maxidx = num_samples + 1
    dts%t_count  = num_samples

    do i=1,num_samples
       x = samples(i);
       dts%t_mean = dts%t_mean + x
       if ( x.lt.dts%t_minv ) then
          dts%t_minv   = x
          dts%t_minidx = i
       end if
       if ( x.gt.dts%t_maxv ) then
          dts%t_maxv   = x
          dts%t_maxidx = i
       end if
    end do

    fn = real(dts%t_count,dp)
    dts%t_mean = dts%t_mean / fn

    dts%t_var = D_ZERO

    do i=1,num_samples
       x = samples(i) - dts%t_mean
       dts%t_var = dts%t_var + (x*x)
    end do

    dts%t_var = dts%t_var / (fn-D_ONE)

    if ( num_samples.lt.dts%t_minidx ) then
       call standard_error( 'single_compile: minimum value/index could not be found', &
            &               1, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    if ( num_samples.lt.dts%t_maxidx ) then
       call standard_error( 'single_compile: maximum value/index could not be found', &
            &               1, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    dts%t_level1 = .true.

999 continue

  end subroutine single_compile


  !/ =====================================================================================
  subroutine single_extra( dts, samples, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compile the extra statistics data structure for a single dimensional data set.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(single_stats),      intent(inout) :: dts         !! reference to this single_stats object.
    real(dp),                 intent(in)    :: samples(:)  !! array of sample values.
    integer,      optional,   intent(out)   :: IERR        !! return error
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer  :: i, ie, num_samples
    real(dp) :: fn, sd, x, x3
    !/ -----------------------------------------------------------------------------------

    num_samples = size(samples)

    dts%t_level2 = .false.

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

    if ( .not.dts%t_level1 ) then
       call standard_error( 'single_extra: this requires that you call compile first', &
            &               0, IERR=IERR, EMSG=EMSG )
       call dts%compile( samples, IERR=ie )
       if ( ie.ne.0 ) then
          call standard_error( 'single_extra:    I tried to do it for you, but it failed', &
               &               ie, IERR=IERR, EMSG=EMSG )
       end if
       call standard_error( 'single_extra:    I did it for you', &
            &               0, IERR=IERR, EMSG=EMSG )
    end if

    if ( num_samples.ne.dts%t_count ) then
       call standard_error( 'single_extra: you should run extra with the same data count as compile', &
            &               3, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    dts%t_std = sqrt(dts%t_var)

    dts%t_adev = D_ZERO
    dts%t_skew = D_ZERO
    dts%t_kurt = D_ZERO

    if (.not.isZero(dts%t_std)) then
       do i=1,num_samples
          sd = samples(i) - dts%t_mean
          x  = sd / dts%t_std
          x3 = x*x*x
          dts%t_adev = dts%t_adev + Abs(sd)
          dts%t_skew = dts%t_skew + x3
          dts%t_kurt = dts%t_kurt + (x3*x)
       end do

       fn = real(dts%t_count,dp)

       dts%t_adev = dts%t_adev / fn
       dts%t_skew = dts%t_skew / fn
       dts%t_kurt = dts%t_kurt / fn
       dts%t_kurt = dts%t_kurt - D_THREE
    else
       call standard_error( 'Standard deviation was ZERO, no values computed', &
            &               4, IERR=IERR, EMSG=EMSG )
       goto 999
    end if

    dts%t_level2 = .true.

999 continue

  end subroutine single_extra










  !/ =====================================================================================
  function multi_num_var( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of variables.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts  !! reference to this multi_stats object.
    integer                         :: n   !! number of variables.
    !/ -----------------------------------------------------------------------------------
    n = dts%t_num_var
  end function multi_num_var


  !/ =====================================================================================
  function multi_count( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of samples.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts  !! reference to this multi_stats object.
    integer                        :: n    !! number of samples.
    !/ -----------------------------------------------------------------------------------
    n = dts%SS(1)%count()
  end function multi_count


  !/ =====================================================================================
  function multi_minv( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Minimum sample value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! minimum value of the samples.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_minv
  end function multi_minv


  !/ =====================================================================================
  function multi_maxv( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Maximum sample value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! maximum value of the samples.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_maxv
  end function multi_maxv


  !/ =====================================================================================
  function multi_minidx( dts, index ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Index of minimum value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    integer                        :: n     !! index of minimum value.
    !/ -----------------------------------------------------------------------------------
    n = dts%SS(index)%t_minidx
  end function multi_minidx


  !/ =====================================================================================
  function multi_maxidx( dts, index ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Index of maximum value.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    integer                        :: n     !! index of maximum value.
    !/ -----------------------------------------------------------------------------------
    n = dts%SS(index)%t_maxidx
  end function multi_maxidx


  !/ =====================================================================================
  function multi_mean( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Mean value of sample.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! mean value of sample.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_mean
  end function multi_mean


  !/ =====================================================================================
  function multi_var( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Sample variance.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! sample variance.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_var
  end function multi_var


  !/ =====================================================================================
  function multi_std( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Standard deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! standard deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_std
  end function multi_std


  !/ =====================================================================================
  function multi_adev( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Absolute deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! absolute deviation of the mean.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_adev
  end function multi_adev


  !/ =====================================================================================
  function multi_skew( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Sample skew.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! sample skew.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_skew
  end function multi_skew


  !/ =====================================================================================
  function multi_kurt( dts, index ) result( v )
    !/ -----------------------------------------------------------------------------------
    !! Sample kurt.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(in) :: dts   !! reference to this multi_stats object.
    integer,            intent(in) :: index !! index.
    real(dp)                       :: v     !! sample kurt.
    !/ -----------------------------------------------------------------------------------
    v = dts%SS(index)%t_kurt
  end function multi_kurt


  !/ =====================================================================================
  subroutine multi_init( dts )
    !/ -----------------------------------------------------------------------------------
    !! Initialize the Statistics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(inout) :: dts    !! reference to this multi_stats object.
    !/ -----------------------------------------------------------------------------------
    call dts%clear
  end subroutine multi_init


  !/ =====================================================================================
  subroutine multi_clear( dts )
    !/ -----------------------------------------------------------------------------------
    !! Clear the Statistics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(inout) :: dts    !! reference to this multi_stats object.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    do i=1,dts%t_num_var
       call dts%SS(i)%clear
    end do
  end subroutine multi_clear


  !/ =====================================================================================
  subroutine multi_report( dts, FILE, UNIT, IOSTAT, FMT, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Report the compiled statistics.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats),     intent(inout) :: dts    !! reference to this multi_stats object.
    character(*), optional, intent(in)    :: FILE   !! full path to file.
    integer,      optional, intent(in)    :: UNIT   !! unit to write report.
    integer,      optional, intent(out)   :: IOSTAT !! error return.
    character(*), optional, intent(in)    :: FMT    !! edit descriptor for real.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer                   :: i, n, un, ios
    character(:), allocatable :: rfmt
    character(:), allocatable :: ifmt
    !/ -----------------------------------------------------------------------------------

    if ( present( IOSTAT ) ) IOSTAT=0
    if ( present( EMSG ) )   EMSG='success'

    if ( dts%t_level1 ) then
       rfmt = "(A10,':',*(1X,ES11.4))"
       ifmt = "(A10,':',*(1X,I11))"

       if ( present( FMT ) ) then
          rfmt = "(A10,':',*(1X," // trim(FMT) // '))'
       end if

       un = WriteUnit( FILE=file, UNIT=unit, IOSTAT=ios )

       n = dts%t_num_var

       if (0.eq.ios) then
          write( un, ifmt) 'Num.Var...', dts%t_num_var
          write( un, ifmt) 'Num.Samp..', dts%count()
          write( un, rfmt) 'Min Value.', (dts%minv(i),i=1,n)
          write( un, rfmt) 'Max Value.', (dts%maxv(i),i=1,n)
          write( un, ifmt) 'Min Index.', (dts%minidx(i),i=1,n)
          write( un, ifmt) 'Max Index.', (dts%maxidx(i),i=1,n)
          write( un, rfmt) 'Mean......', (dts%mean(i),i=1,n)
          write( un, rfmt) 'Variance..', (dts%var(i),i=1,n)
          if ( dts%t_level2a ) then
             write( un, rfmt) 'Std Dev...', (dts%std(i),i=1,n)
             write( un, rfmt) 'Abs Dev...', (dts%adev(i),i=1,n)
             write( un, rfmt) 'Skew......', (dts%skew(i),i=1,n)
             write( un, rfmt) 'Kurt......', (dts%kurt(i),i=1,n)
          end if
       end if

       if ( present( FILE ) ) then
          close(un)
       end if

       if ( present( IOSTAT ) ) then
          IOSTAT=ios
       end if
    else
       call standard_error( 'SingleStat: no samples were compiled', 1, IERR=IOSTAT, EMSG=EMSG )
    end if

  end subroutine multi_report


  !/ =====================================================================================
  subroutine multi_compile( dts, samples, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compile the statistics data structure for a single dimensional data set.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(inout) :: dts           !! reference to this multi_stats object.
    real(dp),           intent(in)    :: samples(:,:)  !! array of sample values.
    integer,      optional,              intent(out)   :: IERR    !! return error.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer :: i, m
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

    m = size(samples,DIM=2)
    if (m.lt.dts%t_num_var) then
       do i=1,dts%t_num_var
          call dts%SS(i)%compile( samples(:,i), IERR=IERR, EMSG=EMSG )
       end do
    else
       call standard_error( 'MultiStat: not enough dimensions', 2, IERR=IERR, EMSG=EMSG )
    end if
  end subroutine multi_compile


  !/ =====================================================================================
  subroutine multi_extra( dts, samples, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compile the extra statistics data structure for a single dimensional data set.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(inout) :: dts           !! reference to this multi_stats object.
    real(dp),           intent(in)    :: samples(:,:)  !! array of sample values.
    integer, optional,  intent(out)   :: IERR          !! return error.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    integer :: i, m
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

    m = size(samples,DIM=2)
    if (m.lt.dts%t_num_var) then
       do i=1,dts%t_num_var
          call dts%SS(i)%extra( samples(:,i), IERR=IERR )
       end do
    else
       call standard_error( 'MultiStat: not enough dimensions', 2, IERR=IERR, EMSG=EMSG )
    end if
  end subroutine multi_extra


  !/ =====================================================================================
  subroutine multi_correlate( dts, samples, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compute covariance and correlation matrices.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(inout) :: dts           !! reference to this multi_stats object.
    real(dp),           intent(in)    :: samples(:,:)  !! array of sample values.
    integer, optional,  intent(out)   :: IERR          !! return error.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

  end subroutine multi_correlate


  !/ =====================================================================================
  subroutine multi_invert_cov( dts, IERR, EMSG )
    !/ -----------------------------------------------------------------------------------
    !! Compute the inverse of the covariance matrix.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(multi_stats), intent(inout) :: dts    !! reference to this multi_stats object.
    integer, optional,  intent(out)   :: IERR          !! return error.
    character(:), optional, allocatable, intent(out)   :: EMSG    !! returned error message
    !/ -----------------------------------------------------------------------------------

    if ( present( IERR ) ) IERR=0
    if ( present( EMSG ) ) EMSG='success'

    write(*,*) 'multi_invert_cov: not yet available'
    stop

  end subroutine multi_invert_cov





  !/ =====================================================================================
  function roulette_size( dts ) result( n )
    !/ -----------------------------------------------------------------------------------
    !! Number of weights.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette), intent(in) :: dts !! reference to this Roulette object.
    integer                     :: n   !! number of weights.
    !/ -----------------------------------------------------------------------------------
    n = dts%count
  end function roulette_size

  
  !/ =====================================================================================
  subroutine roulette_destroy( dts )
    !/ -----------------------------------------------------------------------------------
    !! Destroy allocation.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette), intent(inout) :: dts !! reference to this Roulette object.
    !/ -----------------------------------------------------------------------------------
    if ( allocated( dts%weight ) ) then
       deallocate( dts%weight )
    end if
    dts%count = 0
  end subroutine roulette_destroy

  
  !/ =====================================================================================
  subroutine roulette_final( rtp )  
    !/ -----------------------------------------------------------------------------------
    !! Destructor.
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(Roulette), intent(inout) :: rtp !! reference to a Roulette object.
    !/ -----------------------------------------------------------------------------------
    call rtp%destroy
  end subroutine roulette_final

  
  !/ =====================================================================================
  subroutine roulette_set_all( dts, W )
    !/ -----------------------------------------------------------------------------------
    !! Set all weights.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette), intent(inout) :: dts  !! reference to this Roulette object.
    real(dp),        intent(in)    :: W(:) !! list of weights.
    !/ -----------------------------------------------------------------------------------
    integer :: i, n
    !/ -----------------------------------------------------------------------------------
    call dts%destroy
    n = size( W )
    allocate( dts%weight(n) )
    dts%count = n
    do i=1,n
       dts%weight(i) = W(i)
    end do

    call dts%roulette_renormalize

  end subroutine roulette_set_all

  
  !/ =====================================================================================
  subroutine roulette_set_one( dts, W, idx )
    !/ -----------------------------------------------------------------------------------
    !! Set a single weight.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette), intent(inout) :: dts  !! reference to this Roulette object.
    real(dp),        intent(in)    :: W    !! weight
    integer,         intent(in)    :: idx  !! index
    !/ -----------------------------------------------------------------------------------
    if ( 1.gt.idx ) then
       call log_error( 'Roulette%set: index below bounds', I4=idx )
       goto 999
    end if

    if ( dts%count.lt.idx ) then
       call log_error( 'Roulette%set: index above bounds', I4=idx )
       goto 999
    end if

    dts%weight(idx) = W
    call dts%roulette_renormalize
    
999 continue
  end subroutine roulette_set_one

  
  !/ =====================================================================================
  subroutine roulette_display( dts, UNIT )
    !/ -----------------------------------------------------------------------------------
    !! Renormalize the weights.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette),   intent(inout) :: dts  !! reference to this Roulette object.
    integer, optional, intent(in)    :: UNIT
    !/ -----------------------------------------------------------------------------------
    integer :: un, i
    !/ -----------------------------------------------------------------------------------
    
    if ( present( UNIT ) ) then
       un = UNIT
    else
       un = OUTPUT_UNIT
    end if

    write( un, 100 ) dts%count

    do i=1,dts%count
       write( un, 200 ) i, dts%weight(i)
    end do

100 format( 'Count = ',I0 )
200 format( '  W(',I0,') = ',ES15.8 )

  end subroutine roulette_display

  
  !/ =====================================================================================
  subroutine roulette_renormalize( dts )
    !/ -----------------------------------------------------------------------------------
    !! Renormalize the weights.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette), intent(inout) :: dts  !! reference to this Roulette object.
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    real(dp) :: s
    !/ -----------------------------------------------------------------------------------

    s = D_ZERO
    
    do i=1,dts%count
       s = s + dts%weight(i)
    end do

    do i=1,dts%count
       dts%weight(i) = dts%weight(i) / s
    end do

  end subroutine roulette_renormalize
  
    
  !/ =====================================================================================
  function roulette_select( dts, DD, ENT ) result( idx )
    !/ -----------------------------------------------------------------------------------
    !! Select an index distributed by weights.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(Roulette),                intent(inout) :: dts !! reference to this Roulette object.
    type(Dice),           optional, intent(inout) :: DD  !! reference to a Dice object.
    type(entropy_source), optional, intent(inout) :: ENT !! reference to an entropy source.
    integer                                       :: idx !! selected index
    !/ -----------------------------------------------------------------------------------
    integer  :: i
    real(dp) :: accum, test
    !/ -----------------------------------------------------------------------------------

     if ( present( ENT ) ) then
        test = ENT%R64()
     else if ( present( DD ) ) then
        test = DD%uniform()
     else
        call random_number( test )
     end if

     accum = D_ZERO
     idx   = 0
     
     do i=1,dts%count
        accum = accum + dts%weight(i)
        if ( accum.gt.test ) then
           idx = i
           exit
        end if
     end do
     
  end function roulette_select


  !/ =====================================================================================
 subroutine fast_mean_variance( mean, var, data )
    !/ -----------------------------------------------------------------------------------
    !! Compute fast mean and variance of a column of data
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(out) :: mean
    real(dp), intent(out) :: var
    real(dp), intent(in)  :: data(:)
    !/ -----------------------------------------------------------------------------------
    integer  :: i, n
    real(dp) :: dif
    !/ -----------------------------------------------------------------------------------
    n = size(data)

    mean = D_ZERO
    do concurrent(i=1:n)
       mean = mean + data(i)
    end do
    mean = mean / real(n,dp)

    var = D_ZERO
    do concurrent(i=1:n)
       dif = data(i) - mean
       var = var + (dif*dif)
    end do
    var = var / real(n-1,dp)
  end subroutine fast_mean_variance
    
  
end module statistics_mod


!/ =======================================================================================
!/ **                            S T A T I S T I C S _ M O D                            **
!/ =========================================================================== END FILE ==
