!/ ====================================================================== BEGIN FILE =====
!/ **                            S T A T I S T I C S _ M O D                            **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2019, Stephen W. Soliday                                           **
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
  implicit none

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

     generic   :: sample => running_sample_single_real8, &
          &                 running_sample_list_real8

  end type running_stats



  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  
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
    integer :: un, ios
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

end module statistics_mod

!/ =======================================================================================
!/ **                            S T A T I S T I C S _ M O D                            **
!/ =========================================================================== END FILE ==
