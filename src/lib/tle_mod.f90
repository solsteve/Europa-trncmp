!/ ====================================================================== BEGIN FILE =====
!/ **                                   T L E _ M O D                                   **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2017,20, Stephen W. Soliday                                        **
!/ **                         stephen.soliday@trncmp.org                                **
!/ **                         http://research.trncmp.org                                **
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
module tle_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2017-04-11
  !! license: GPL
  !!
  !!  Provides the interface and procedures for Storing SDC/SCC Two and Three Card
  !!  Element Set in Trasmission Format.
  !
  !/ -------------------------------------------------------------------------------------

  use trncmp_env
  use tlogger
  use time_tools_mod
  implicit none

  real(dp), parameter :: XPdotP_C1 = 1.44D3 / D_2PI
  real(dp), parameter :: XPdotP_C2 = 1.44D3 * XPdotP_C1
  real(dp), parameter :: XPdotP_C3 = 1.44D3 * XPdotP_C2

  
  !/ =====================================================================================
  type TCard
     !/ ----------------------------------------------------------------------------------

     character(len=80) :: saved_card_0  !! save a copy of card 0 before parsing
     character(len=80) :: saved_card_1  !! save a copy of card 1 before parsing
     character(len=80) :: saved_card_2  !! save a copy of card 2 before parsing

     !/ ----- card 0 ---------------------------------------------------------------------

     !/ 00000000011111111112222222222333333333344444444445555555555666666666677777777778
     !/ 12345678901234567890123456789012345678901234567890123456789012345678901234567890
     !/ AAAAAAAAAA

     character(69) :: name      !< 01-10  A10    Satellite Name


     !/ ----- card 1 ---------------------------------------------------------------------

     !/ 00000000011111111112222222222333333333344444444445555555555666666666677777777778
     !/ 12345678901234567890123456789012345678901234567890123456789012345678901234567890
     !/ 1 NNNNNU NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN
     !/ 1 AAAAAU 00  0  0 BBBBB.BBBBBBBB +.CCCCCCCC +00000-0 +00000-0 0  DDDZ

     integer      :: lineno1    !< 01-01  I1     Card Number of Element Data
     !/ ------------ SPACE ------- 02-02  ------------------------------------------------
     integer      :: satNumber  !< 03-07  I5     Satellite Number
     character(1) :: security   !< 08-08  A1     Security Classification (U,C,S,T,B)
     !/ ------------ SPACE  ------ 09-09  ------------------------------------------------
     integer      :: idYear     !< 10-11  I2     International Designator
     !!                                          (Last two digits of launch year)
     integer      :: idLaunch   !< 12-14  I3     International Designator
     !!                                          (Launch number of the year)
     character(3) :: idPiece    !< 15-17  A3     International Designator (Piece of launch)
     !/ ------------ SPACE ------- 18-18  ------------------------------------------------
     integer      :: epochYear  !< 19-20  I2     Epoch Year (Last two digits of year)
     real(dp)     :: epochDay   !< 21-32  F12.8  Epoch (Day number and fractional portion
     !!                                          of the day) 000Z Jan 1 == 1.0  (not 0.0)
     !/ ------------ SPACE ------- 33-33  ------------------------------------------------
     real(dp)     :: N1         !< 34-43  F10.8  First Time Derivative of the Mean Motion
     !!                                          divided by 2. or Ballistic Coefficient
     !!                                          (Depending on ephemeris type)
     !!                                          TLE:[rev/day^2] Internal:{rad/min^2}
     !/ ------------ SPACE ------- 44-44 -------------------------------------------------
     real(dp)     :: dN1        !< 45-52  E8.5   Second Time Derivative of Mean Motion
     !!                                          divided by 6. (Blank if N/A)
     !!                                          TLE:[rev/day^3] Internal:{rad/min^3}
     !/ ------------ SPACE ------- 53-53 -------------------------------------------------
     real(dp)     :: bstar      !< 54-61  E8.5   BSTAR drag term if SGP4 general
     !!                                          perturbation theory was used. Otherwise,
     !!                                          radiation pressure coefficient.
     !!                                          [Rearth^-1]
     !/ ------------ SPACE ------- 62-62 -------------------------------------------------
     integer      :: eType      !< 63-63  I1     Ephemeris type
     !/ ------------ SPACE ------- 64-64 -------------------------------------------------
     integer      :: eNum       !< 65-68  I4     Element number
     integer      :: cksum1     !< 69-69  I1     Check Sum (Modulo 10)



     !/ ----- card 2 ---------------------------------------------------------------------

     !/ 00000000011111111112222222222333333333344444444445555555555666666666677777777778
     !/ 12345678901234567890123456789012345678901234567890123456789012345678901234567890
     !/ 2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN
     !/ 2 AAAAA EEE.EEEE FFF.FFFF GGGGGGG HHH.HHHH III.IIII JJ.JJJJJJJJKKKKKZ

     !
     integer      :: lineno2    !< 01-01  I1     Line Number of Element Data
     !/ ------------ SPACE ------- 02-02 -------------------------------------------------
     integer      :: satNumber2 !< 03-07  I5     Satellite Number
     !/ ------------ SPACE ------- 08-08 -------------------------------------------------
     real(dp)     :: I0         !< 09-16  F8.4   Orbital Inclination
     !                                           TLE:[Degrees] Internal:{radian}
     !/ ------------ SPACE ------- 17-17 -------------------------------------------------
     real(dp)     :: O0         !< 18-25  F8.4   Rt. Asc. of the Ascending Node
     !                                           TLE:[Degrees] Internal:{radian}
     !/ ------------ SPACE ------- 26-26 -------------------------------------------------
     real(dp)     :: E0         !< 27-33  F7.7   Eccentricity (decimal point assumed)
     !/ ------------ SPACE ------- 34-34 -------------------------------------------------
     real(dp)     :: W0         !< 35-42  F8.4   Argument of Perigee
     !                                           TLE:[Degrees] Internal:{radian}
     !/ ------------ SPACE ------- 43-43 -------------------------------------------------
     real(dp)     :: M0         !< 44-51  F8.4   Mean Anomaly
     !                                           TLE:[Degrees] Internal:{radian}
     !/ ------------ SPACE ------- 52-52 -------------------------------------------------
     real(dp)     :: N0         !< 53-63  F11.8  Mean Motion
     !                                           TLE:[Revs per day]  Internal:[rad/min]
     integer      :: revNumber  !< 64-68  I5     Revolution number at epoch         [Revs]
     integer      :: cksum2     !< 69-69  I1     Check Sum (Modulo 10)

   contains

     procedure, private :: tle_parse_card_0
     procedure, private :: tle_parse_card_1
     procedure, private :: tle_parse_card_2

     procedure, private :: tle_format_card_0
     procedure, private :: tle_format_card_1
     procedure, private :: tle_format_card_2

     procedure :: clear   => tle_clear_cards
     procedure :: copy    => tle_copy_cards
     procedure :: save    => tle_save_cards
     procedure :: restore => tle_restore_cards

     procedure :: set     => tle_set
     procedure :: get     => tle_get

  end type TCard



  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


  

  !/ =====================================================================================
  function TLE_Checksum( card ) result( cs )
    !/ -----------------------------------------------------------------------------------
  !! Compute the modulo 10 check sum for TLE
  !/ -------------------------------------------------------------------------------------
    implicit none
    character(80), intent(in) :: card !! data card to check.
    integer                   :: cs   !! TLE checksum.
    !/ -----------------------------------------------------------------------------------
    character(1) :: test
    integer      :: i, sm
    !/ -----------------------------------------------------------------------------------

    sm = 0
    do i=1,68
       test = card(i:i)
       if ( '1'.eq.test ) sm = sm + 1
       if ( '2'.eq.test ) sm = sm + 2
       if ( '3'.eq.test ) sm = sm + 3
       if ( '4'.eq.test ) sm = sm + 4
       if ( '5'.eq.test ) sm = sm + 5
       if ( '6'.eq.test ) sm = sm + 6
       if ( '7'.eq.test ) sm = sm + 7
       if ( '8'.eq.test ) sm = sm + 8
       if ( '9'.eq.test ) sm = sm + 9
       if ( '-'.eq.test ) sm = sm + 1
       if ( '+'.eq.test ) sm = sm + 2
    end do

    cs = mod( sm, 10 )
    
  end function TLE_Checksum


  !/ =====================================================================================
  subroutine tle_clear_cards( dts, clear_buffers )
    !/ -----------------------------------------------------------------------------------
    !! Zero out all fields, and optionaly the three save buffers.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),      intent(inout) :: dts           !! reference to this TCard.
    logical, optional, intent(in)    :: clear_buffers !! flag to also clear the save buffers.
    !/ -----------------------------------------------------------------------------------
    integer :: i
    !/ -----------------------------------------------------------------------------------
    
    do i=1,69
       dts%name(i:i) = ' '
    end do

    dts%lineno1    = 1
    dts%satNumber  = 0
    dts%security   = 'U'
    dts%idYear     = 0
    dts%idLaunch   = 0
    dts%idPiece    = '   '
    dts%epochYear  = 0
    dts%epochDay   = D_ZERO
    dts%N1         = D_ZERO
    dts%dN1        = D_ZERO
    dts%bstar      = D_ZERO
    dts%eType      = 0
    dts%eNum       = 0
    dts%cksum1     = 0

    dts%lineno2    = 2
    dts%satNumber2 = 0
    dts%I0         = D_ZERO
    dts%O0         = D_ZERO
    dts%E0         = D_ZERO
    dts%W0         = D_ZERO
    dts%M0         = D_ZERO
    dts%N0         = D_ZERO
    dts%revNumber  = 0
    dts%cksum2     = 0

    if ( present( clear_buffers ) ) then
       if ( clear_buffers ) then
          do i=1,80
             dts%saved_card_0(i:i) = ' '
             dts%saved_card_1(i:i) = ' '
             dts%saved_card_2(i:i) = ' '
          end do
       end if
    end if


  end subroutine tle_clear_cards

  
  !/ =====================================================================================
  subroutine tle_copy_cards( dts, src, copy_buffers )
    !/ -----------------------------------------------------------------------------------
    !! Copy all of the fields, and optionally the three save buffers
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),      intent(inout) :: dts          !! reference to this TCard.
    type(TCard),       intent(in)    :: src          !! reference to a source TCard.
    logical, optional, intent(in)    :: copy_buffers !! flag to also clear the save buffers.
    !/ -----------------------------------------------------------------------------------

    dts%name       = src%name

    dts%lineno1    = src%lineno1
    dts%satNumber  = src%satNumber
    dts%security   = src%security
    dts%idYear     = src%idYear
    dts%idLaunch   = src%idLaunch
    dts%idPiece    = src%idPiece
    dts%epochYear  = src%epochYear
    dts%epochDay   = src%epochDay
    dts%N1         = src%N1
    dts%dN1        = src%dN1
    dts%bstar      = src%bstar
    dts%eType      = src%eType
    dts%eNum       = src%eNum
    dts%cksum1     = src%cksum1

    dts%lineno2    = src%lineno2
    dts%satNumber2 = src%satNumber2
    dts%I0         = src%I0
    dts%O0         = src%O0
    dts%E0         = src%E0
    dts%W0         = src%W0
    dts%M0         = src%M0
    dts%N0         = src%N0
    dts%revNumber  = src%revNumber
    dts%cksum2     = src%cksum2

    if ( present( copy_buffers ) ) then
       if ( copy_buffers ) then
          dts%saved_card_0 = src%saved_card_0
          dts%saved_card_1 = src%saved_card_1
          dts%saved_card_2 = src%saved_card_2
       end if
    end if

  end subroutine tle_copy_cards

  !/ =====================================================================================
  subroutine tle_save_cards( dts )
    !/ -----------------------------------------------------------------------------------
    !! Save the fields of this TCard into three internal 80 character cards
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard), intent(inout) :: dts  !! reference to this TCard.
    !/ -----------------------------------------------------------------------------------

    call dts%tle_parse_card_0( dts%saved_card_0 )
    call dts%tle_parse_card_1( dts%saved_card_1 )
    call dts%tle_parse_card_2( dts%saved_card_2 )

  end subroutine tle_save_cards

  !/ =====================================================================================
  subroutine tle_restore_cards( dts, CHECK )
    !/ -----------------------------------------------------------------------------------
    !! Restore the fields from the three internal 80 character cards. If check is present
    !! return a .true. if the checksum passes
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),      intent(inout) :: dts   !! reference to this TCard.
    logical, optional, intent(out)   :: check !! result of the checksum.
    !/ -----------------------------------------------------------------------------------
    logical :: t1, t2
    !/ -----------------------------------------------------------------------------------

    call dts%tle_parse_card_0( dts%saved_card_0 )

    if ( present( CHECK ) ) then
       call dts%tle_parse_card_1( dts%saved_card_1, t1 )
       call dts%tle_parse_card_2( dts%saved_card_2, t2 )
       CHECK = t1.and.t2
    else
       call dts%tle_parse_card_1( dts%saved_card_1 )
       call dts%tle_parse_card_2( dts%saved_card_2 )
    end if

  end subroutine tle_restore_cards

  !/ =====================================================================================
  subroutine tle_set( dts, EPC, CARD0, CARD1, CARD2 )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),            intent(inout) :: dts  !! reference to this TCard.
    type(Epoch),   optional, intent(in)    :: EPC
    character(*), optional, intent(in)    :: CARD0
    character(*), optional, intent(in)    :: CARD1
    character(*), optional, intent(in)    :: CARD2
    !/ -----------------------------------------------------------------------------------

    if ( present( CARD0 ) ) then
       call dts%tle_parse_card_0( CARD0 )
    end if

    if ( present( CARD1 ) ) then
       call dts%tle_parse_card_1( CARD1 )
    end if

    if ( present( CARD2 ) ) then
       call dts%tle_parse_card_2( CARD2 )
    end if

  end subroutine tle_set

  !/ =====================================================================================
  subroutine tle_get( dts, EPC, CARD0, CARD1, CARD2 )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),            intent(inout) :: dts  !! reference to this TCard.
    type(Epoch),   optional, intent(inout) :: EPC
    character(80), optional, intent(out)   :: CARD0
    character(80), optional, intent(out)   :: CARD1
    character(80), optional, intent(out)   :: CARD2
    !/ -----------------------------------------------------------------------------------

    if ( present( CARD0 ) ) then
       call dts%tle_format_card_0( CARD0 )
    end if

    if ( present( CARD1 ) ) then
       call dts%tle_format_card_1( CARD1 )
    end if

    if ( present( CARD2 ) ) then
       call dts%tle_format_card_2( CARD2 )
    end if

  end subroutine tle_get

  !/ =====================================================================================
  subroutine tle_parse_card_0( dts, card )
    !/ -----------------------------------------------------------------------------------
    !! Parse the supplied card into the appropriate fields for card 0.
    !/ -----------------------------------------------------------------------------------
    !  00000000011111111112222222222333333333344444444445555555555666666666677777777778
    !  12345678901234567890123456789012345678901234567890123456789012345678901234567890
    !  AAAAAAAAAA
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard), intent(inout) :: dts  !! reference to this TCard.
    character(*), intent(in)    :: card !! card number zero.
    !/ -----------------------------------------------------------------------------------
    integer :: start
    
    if ( ( '0'.eq.card(1:1) ).and.( ' '.eq.card(2:2) ) ) then
       start = 3
    else
       start = 1
    end if
    
    call zero( dts%name )
    dts%name = card(start:)
    
  end subroutine tle_parse_card_0

  !/ =====================================================================================
  subroutine tle_parse_card_1( dts, card, CHECK, IERR )
    !/ -----------------------------------------------------------------------------------
    !! Parse the supplied card into the appropriate fields for card 1.
    !/ ----------------------------------------------------------------------------------- 
    !  00000000011111111112222222222333333333344444444445555555555666666666677777777778
    !  12345678901234567890123456789012345678901234567890123456789012345678901234567890
    !  1 NNNNNU NNNNNAAA NNNNN.NNNNNNNN +.NNNNNNNN +NNNNN-N +NNNNN-N N NNNNN
    !  1 AAAAAU 00  0  0 BBBBB.BBBBBBBB +.CCCCCCCC +00000-0 +00000-0 0  DDDZ
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),      intent(inout) :: dts   !! reference to this TCard.
    character(*),      intent(in)    :: card  !! card number one.
    logical, optional, intent(out)   :: CHECK !! result of the checksum.
    integer, optional, intent(out)   :: IERR  !! error return status.
    !/ -----------------------------------------------------------------------------------
    real(dp)     :: epd, nd, ndd, bstar
    integer      :: cdno, satno, epy, nexp, bexp, ept, eno, cs, idy, idl, ios
    character(3) :: idp
    character(1) :: sec
    !/ -----------------------------------------------------------------------------------
    read(card,100,IOSTAT=ios) cdno, satno, sec, idy, idl, idp, epy, epd, nd, &
         &                     ndd, nexp, bstar, bexp, ept, eno, cs
    
    if ( 0.ne.ios ) then
       if ( .not.present( IERR ) ) then
          call log_error( 'parse_card_1: line did not parse. IOS', I4=ios )
          call log_error( card )
       end if
       goto 999
    end if

    if ( present( CHECK ) ) then
       if ( cs.eq.TLE_checksum( card ) ) then
          CHECK = .true.
       else
          check = .false.
          ios = 99
          goto 999
       end if
    end if

    !/ -----------------------------------------------------------------------------------

    dts%lineno1    = cdno
    dts%satNumber  = satno
    dts%security   = sec
    dts%idYear     = idy
    dts%idLaunch   = idl
    dts%idPiece    = idp
    dts%epochYear  = epy
    dts%epochDay   = epd
    dts%N1         = nd                  / XPdotP_C2
    dts%dN1        = ndd   * D_TEN**nexp / XPdotP_C3
    dts%bstar      = bstar * D_TEN**bexp
    dts%eType      = ept
    dts%eNum       = eno
    dts%cksum1     = cs
    
    !/ -----------------------------------------------------------------------------------

999 continue

    if ( present( IERR ) ) IERR = ios

    !/ -----------------------------------------------------------------------------------
100 FORMAT( I1,1X,I5,A1,1X,I2,I3,A3,1X,I2,D12.0,1X,D10.0,1X,F6.5,I2,1X, &
         &  F6.5,I2,1X,I1,1X,I4,I1 )

    
  end subroutine tle_parse_card_1

  
  !/ =====================================================================================
  subroutine tle_parse_card_2( dts, card, CHECK, IERR )
    !/ -----------------------------------------------------------------------------------
    !!
    !/ -----------------------------------------------------------------------------------
    !  00000000011111111112222222222333333333344444444445555555555666666666677777777778
    !  12345678901234567890123456789012345678901234567890123456789012345678901234567890
    !  2 NNNNN NNN.NNNN NNN.NNNN NNNNNNN NNN.NNNN NNN.NNNN NN.NNNNNNNNNNNNNN
    !  2 AAAAA EEE.EEEE FFF.FFFF GGGGGGG HHH.HHHH III.IIII JJ.JJJJJJJJKKKKKZ
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),      intent(inout) :: dts   !! reference to this TCard.
    character(*),     intent(in)    :: card  !! card number two.
    logical, optional, intent(out)   :: CHECK !! result of the checksum.
    integer, optional, intent(out)   :: IERR  !! error return status.
    !/ -----------------------------------------------------------------------------------
    integer  :: cdno, satno, revi, cs, ios
    real(dp) :: incli, node, ecc, argp, Mo, no_kozai
    !/ -----------------------------------------------------------------------------------

    read(card,200,IOSTAT=ios) cdno, satno, incli, node, ecc, argp, Mo, no_kozai, revi, cs
    

    if ( 0.ne.ios ) then
       if ( .not.present( IERR ) ) then
          call log_error( 'parse_card_1: line did not parse. IOS', I4=ios )
          call log_error( card )
       end if
       goto 999
    end if

    if ( present( CHECK ) ) then
       if ( cs.eq.TLE_checksum( card ) ) then
          CHECK = .true.
       else
          check = .false.
          ios = 99
          goto 999
       end if
    end if

    !/ -----------------------------------------------------------------------------------

    dts%lineno2    = cdno
    dts%satNumber2 = satno
    dts%I0         = incli * DEG2RAD
    dts%O0         = node  * DEG2RAD
    dts%E0         = ecc
    dts%W0         = argp  * DEG2RAD
    dts%M0         = Mo    * DEG2RAD
    dts%N0         = no_kozai / XPdotP_C1
    dts%revNumber  = revi
    dts%cksum2     = cs

    !/ -----------------------------------------------------------------------------------

999 continue

    if ( present( IERR ) ) IERR = ios

    !/ -----------------------------------------------------------------------------------
200 FORMAT( I1,1X,I5,1X,D8.0,1X,D8.0,1X,F7.7,1X,D8.0,1X,D8.0,1X,D11.0,I5,I1)

  end subroutine tle_parse_card_2



  
  !/ =====================================================================================
  subroutine tle_format_card_0( dts, card )
    !/ -----------------------------------------------------------------------------------
    !! Return a formated representation for card zero.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),  intent(inout) :: dts  !! reference to this TCard.
    character(80), intent(out)   :: card !! reference to an 80 character card.
    !/ -----------------------------------------------------------------------------------

    card = '0 ' // dts%name

  end subroutine tle_format_card_0

  !/ =====================================================================================
  subroutine tle_format_card_1( dts, card, status )
    !/ -----------------------------------------------------------------------------------
    !! Return a formated representation for card one.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),  intent(inout) :: dts  !! reference to this TCard.
    character(80), intent(out)   :: card !! reference to an 80 character card.
    integer, optional, intent(out) :: status
    !/ -----------------------------------------------------------------------------------
    real(dp)      :: epd
    integer       :: cdno, satno, epy, ept, eno, cs, idy, idl, ios
    character(3)  :: idp
    character(1)  :: sec
    character(16) :: buffer
    character(8)  :: ndd, bst
    character(10) :: nd
    !/ -----------------------------------------------------------------------------------

    cdno  = 1
    satno = dts%satNumber
    sec   = dts%security
    idy   = dts%idYear
    idl   = dts%idLaunch
    idp   = dts%idPiece
    epy   = dts%epochYear
    epd   = dts%epochDay

    write(buffer,'(F11.8)') dts%N1 * XPdotP_C2
    if ( '-'.eq.buffer(1:1) ) then
       nd(1:1) = '-'
    else
       nd(1:1) = ' '
    end if

    nd(2:10) = buffer(3:12)

    write(buffer,'(E12.5)') dts%dN1 * XPdotP_C3
    if ( '-'.eq.buffer(1:1) ) then
       ndd(1:1) = '-'
    else
       ndd(1:1) = ' '
    end if
    ndd(2:6) = buffer(4:8)
    ndd(7:7) = buffer(10:10)
    ndd(8:8) = buffer(12:12)
    
    write(buffer,'(E12.5)') dts%bstar
    
    if ( '-'.eq.buffer(1:1) ) then
       bst(1:1) = '-'
    else
       bst(1:1) = ' '
    end if
    bst(2:6) = buffer(4:8)
    bst(7:7) = buffer(10:10)
    bst(8:8) = buffer(12:12)
    
    ept   = dts%eType
    eno   = dts%eNum
    cs    = 0

    write(card,100,IOSTAT=ios) satno, sec, idy, idl, idp, epy, &
         &                     LeadingZero( 'F12.8', epd ), nd, ndd, bst, ept, eno

    if ( present( status ) ) then
       status = ios
    end if

    if ( 0.eq.ios ) then
       cs = TLE_checksum( card )
       write(card(69:69),'(I1)') cs
    else
       write(ERROR_UNIT,*) 'format_card_1: line did not format'
    end if
    
    !/ -----------------------------------------------------------------------------------
100 FORMAT( '1 ',I5.5,A1,1X,I2.2,I3.3,A3,1X,I2.2,A,1X,A10,1X,A8,1X, &
         &  A8,1X,I1,1X,I4,12X )

  end subroutine tle_format_card_1

  !/ =====================================================================================
  subroutine tle_format_card_2( dts, card, status )
    !/ -----------------------------------------------------------------------------------
    !! Return a formated representation for card two.
    !/ -----------------------------------------------------------------------------------
    implicit none
    class(TCard),  intent(inout) :: dts  !! reference to this TCard.
    character(80), intent(out)   :: card !! reference to an 80 character card.
    integer, optional, intent(out) :: status
    !/ -----------------------------------------------------------------------------------
    integer       :: satno, revi, cs, ios
    real(dp)      :: incli, node, argp, Mo, no_kozai
    character(16) :: buffer
    character(7)  :: ecc
    !/ -----------------------------------------------------------------------------------

    satno    = dts%satNumber2
    incli    = dts%I0 * RAD2DEG
    node     = dts%O0 * RAD2DEG

    write(buffer,'(F10.7)') dts%E0
    ecc = buffer(4:10)

    argp     = dts%W0 * RAD2DEG
    Mo       = dts%M0 * RAD2DEG
    no_kozai = dts%N0 * XPdotP_C1
    revi     = dts%revNumber

    write(card,200,IOSTAT=ios) satno, incli, &
         &                     node, ecc, argp, Mo, no_kozai, revi

    if ( present( status ) ) then
       status = ios
    end if

    if ( 0.eq.ios ) then
       cs = TLE_checksum( card )
       write(card(69:69),'(I1)') cs
    else
       write(ERROR_UNIT,*) 'format_card_2: line did not format'
    end if
    
    !/ -----------------------------------------------------------------------------------
200 FORMAT( '2 ',I5.5,1X,F8.4,1X,F8.4,1X,A7,1X,F8.4,1X,F8.4,1X,F11.8,I5.5,12X)

  end subroutine tle_format_card_2



end module tle_mod



!/ =======================================================================================
!/ **                                   T L E _ M O D                                   **
!/ =========================================================================== END FILE ==

