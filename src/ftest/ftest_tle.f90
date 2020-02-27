!/ ====================================================================== BEGIN FILE =====
!/ **                                F T E S T _ T I M E                                **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2020 Stephen W. Soliday                                           **
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
module ftest_tle
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-02-21
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use tle_mod
  use string_tools
  use file_tools
  implicit none

  interface Diff
     module procedure :: diff_int32
     module procedure :: diff_real8
     module procedure :: diff_string
  end interface Diff

  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  !/ =====================================================================================
  function diff_int32( a, b ) result( d )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: a
    integer, intent(in) :: b
    real(dp)            :: d
    !/ -----------------------------------------------------------------------------------
    real(dp) :: df
    !/ -----------------------------------------------------------------------------------
    df = real(a,dp) - real(b,dp)
    d  = df*df
  end function diff_int32

  
  !/ =====================================================================================
  function diff_real8( a, b ) result( d )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: a
    real(dp), intent(in) :: b
    real(dp)             :: d
    !/ -----------------------------------------------------------------------------------
    real(dp) :: df
    !/ -----------------------------------------------------------------------------------
    df = a - b
    d  = df*df
  end function diff_real8

  
  !/ =====================================================================================
  function diff_string( a, b ) result( d )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: a
    character(*), intent(in) :: b
    real(dp)                 :: d
    !/ -----------------------------------------------------------------------------------
    real(dp) :: df
    integer  :: na, nb, i, n
    !/ -----------------------------------------------------------------------------------
    na = len_trim(a)
    nb = len_trim(b)
    n  = min(na,nb)

    d = D_ZERO
    do i=1,n
       df = (real(ichar(a(i:i)),dp) - real(ichar(b(i:i)),dp)) / real(n,dp)
       d = d + df*df
    end do
  end function diff_string

  
  !/ =====================================================================================
  subroutine test01
    !/ -----------------------------------------------------------------------------------
    implicit none
    
    character(512) :: read_buffer
    integer :: fh, n
    type(string_splitter) :: SS
    type(TCard) :: tc
    real(dp) :: mse

    fh = ReadUnit( FILE='/data/simdata/full_test.mtf' )

    mse = D_ZERO

100 continue
    call zero( read_buffer )
    read(fh,'(A512)', end=999) read_buffer
    call split( SS, read_buffer, '|', COUNT=n)

   ! write(*,"('[',A,']')") SS%get(22)
   ! write(*,"('[',A,']')") SS%get(20)
   ! write(*,"('[',A,']')") SS%get(21)
    
    call tc%set( CARD0=SS%get(22), CARD1=SS%get(20), CARD2=SS%get(21) )

!    print *, 'sat ', asInteger( SS%get(1) ),  tc%satNumber,  diff( asInteger( SS%get(1) ),  tc%satNumber )
!    print *, 'sec ',            SS%get(2),    tc%security,   diff(            SS%get(2),    tc%security ) 
!    print *, 'yr  ', asInteger( SS%get(3) ),  tc%idYear,     diff( asInteger( SS%get(3) ),  tc%idYear )   
!    print *, 'lun ', asInteger( SS%get(4) ),  tc%idLaunch,   diff( asInteger( SS%get(4) ),  tc%idLaunch ) 
!    print *, 'pce ',            SS%get(5),    tc%idPiece,    diff(            SS%get(5),    tc%idPiece )  
!    print *, 'epy ', asInteger( SS%get(6) ),  tc%epochYear,  diff( asInteger( SS%get(6) ),  tc%epochYear )
!    print *, 'epd ', asReal8(   SS%get(7) ),  tc%epochDay,   diff( asReal8(   SS%get(7) ),  tc%epochDay ) 
!    print *, 'N1  ', asReal8(   SS%get(8) ),  tc%N1,         diff( asReal8(   SS%get(8) ),  tc%N1 )       
!    print *, 'dN1 ', asReal8(   SS%get(9) ),  tc%dN1,        diff( asReal8(   SS%get(9) ),  tc%dN1 )      
!    print *, 'B*  ', asReal8(   SS%get(10) ), tc%bstar,      diff( asReal8(   SS%get(10) ), tc%bstar )    
!    print *, 'etp ', asInteger( SS%get(11) ), tc%eType,      diff( asInteger( SS%get(11) ), tc%eType )    
!    print *, 'eno ', asInteger( SS%get(12) ), tc%eNum,       diff( asInteger( SS%get(12) ), tc%eNum )     
                                                                                                          
!    print *, 'I0  ', asReal8(   SS%get(13) ), tc%I0,         diff( asReal8(   SS%get(13) ), tc%I0 )       
!    print *, 'O0  ', asReal8(   SS%get(14) ), tc%O0,         diff( asReal8(   SS%get(14) ), tc%O0 )       
!    print *, 'E0  ', asReal8(   SS%get(15) ), tc%E0,         diff( asReal8(   SS%get(15) ), tc%E0 )       
!    print *, 'W0  ', asReal8(   SS%get(16) ), tc%W0,         diff( asReal8(   SS%get(16) ), tc%W0 )       
!    print *, 'M0  ', asReal8(   SS%get(17) ), tc%M0,         diff( asReal8(   SS%get(17) ), tc%M0 )       
!    print *, 'N0  ', asReal8(   SS%get(18) ), tc%N0,         diff( asReal8(   SS%get(18) ), tc%N0 )       
!    print *, 'rev ', asInteger( SS%get(19) ), tc%revNumber,  diff( asInteger( SS%get(19) ), tc%revNumber )


    mse = mse + diff( asInteger( SS%get(1) ),  tc%satNumber )
    mse = mse + diff(            SS%get(2),    tc%security )
    mse = mse + diff( asInteger( SS%get(3) ),  tc%idYear )
    mse = mse + diff( asInteger( SS%get(4) ),  tc%idLaunch )
    mse = mse + diff(            SS%get(5),    tc%idPiece )
    mse = mse + diff( asInteger( SS%get(6) ),  tc%epochYear )
    mse = mse + diff( asReal8(   SS%get(7) ),  tc%epochDay )
    mse = mse + diff( asReal8(   SS%get(8) ),  tc%N1 )
    mse = mse + diff( asReal8(   SS%get(9) ),  tc%dN1 )
    mse = mse + diff( asReal8(   SS%get(10) ), tc%bstar )
    mse = mse + diff( asInteger( SS%get(11) ), tc%eType )
    !mse = mse + diff( asInteger( SS%get(12) ), tc%eNum )
    
    mse = mse + diff( asReal8(   SS%get(13) ), tc%I0 )
    mse = mse + diff( asReal8(   SS%get(14) ), tc%O0 )
    mse = mse + diff( asReal8(   SS%get(15) ), tc%E0 )
    mse = mse + diff( asReal8(   SS%get(16) ), tc%W0 )
    mse = mse + diff( asReal8(   SS%get(17) ), tc%M0 )
    mse = mse + diff( asReal8(   SS%get(18) ), tc%N0 )
    !mse = mse + diff( asInteger( SS%get(19) ), tc%revNumber )

    goto 100

999 continue

    print *, 'MSE = ', mse








    
    close(fh)

  end subroutine test01

  end module ftest_tle


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_tle
  implicit none
  !/ -------------------------------------------------------------------------------------

  call test01



end program main

!/ =======================================================================================
!/ **                                F T E S T _ T I M E                                **
!/ =========================================================================== END FILE ==
