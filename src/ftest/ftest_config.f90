!/ ====================================================================== BEGIN FILE =====
!/ **                              F T E S T _ C O N F I G                              **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
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
module ftest_config_test
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2018-12-02
  !! license: GPL
  !!
  !!##Test of .
  !!
  !
  !/ -------------------------------------------------------------------------------------
  use trncmp_env
  use config_entry_mod
  use config_entry_list_mod
  use config_section_mod
  implicit none


  !/ -------------------------------------------------------------------------------------
  interface ExpectGot
     !/ ----------------------------------------------------------------------------------
     module procedure :: eg_real
     module procedure :: eg_integer
     module procedure :: eg_string
  end interface ExpectGot


  
   
  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================




  !/ =====================================================================================
  subroutine debug_entry( ent )
    !/ -----------------------------------------------------------------------------------
    !/ PLEASE REMOVE THIS TEMPORARY TESTING PROCEDURE
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_t), intent(inout) :: ent
    !/ -----------------------------------------------------------------------------------

    if ( allocated( ent%key_string) ) then
       write( *,10) ent%key_string
    else
       write( *,11)
    end if

    if ( allocated( ent%value_string) ) then
       write( *,20) ent%value_string
    else
       write( *,21)
    end if

    if ( allocated( ent%comment_string) ) then
       write( *,30) ent%comment_string
    else
       write( *,31)
    end if

10  format( 'Key     [',A,']')
11  format( 'Key     {NONE}' )
20  format( 'Value   [',A,']')
21  format( 'value   {NONE}' )
30  format( 'Comment [',A,']')
31  format( 'Comment {NONE}' )

  end subroutine debug_entry


  !/ =====================================================================================
  subroutine dump_list( list )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_list_t), intent(inout) :: list
    !/ -----------------------------------------------------------------------------------
    integer :: i, n, st
    type(config_entry_t) :: CE
    character(:), allocatable :: rep
    !/ -----------------------------------------------------------------------------------

    n = size( list )

    do i=1,n
       call list%get( CE, i, STATUS=st )
       if ( 0.eq.st ) then
          if ( CE%isComment() ) then
             rep = CE%getComment() 
             write (*,100) i, rep
          else
             rep = CE%toString() 
             if ( CE%isKVPair() ) then
                write (*,110) i, rep
             else
                write (*,120) i,rep
             end if
          end if
       else
          write (*,130) i
       end if
    end do

100 format( '(',I2,') ; ',A )
110 format( '(',I2,')  ',A )
120 format( '(',I2,')  empty record [',A,']' )
130 format( '(',I2,')  null record' )

  end subroutine dump_list


  !/ =====================================================================================
  subroutine eg_real( expect, got )
    !/ -----------------------------------------------------------------------------------
    implicit none
    real(dp), intent(in) :: expect
    real(dp), intent(in) :: got

    if ( expect .lt. got ) then
       write( *, 110 ) expect, got
    else
       if ( expect .gt. got ) then
       write( *, 120 ) expect, got
       else
       write( *, 100 ) expect, got
       end if
    end if

100 format( 'PASS: Expected [',G0,'] got [',G0,']' );
110 format( 'FAIL: Expected [',G0,'] got [',G0,'] greater' );
120 format( 'FAIL: Expected [',G0,'] got [',G0,'] less' );

  end subroutine eg_real


  !/ =====================================================================================
  subroutine eg_integer( expect, got )
    !/ -----------------------------------------------------------------------------------
    implicit none
    integer, intent(in) :: expect
    integer, intent(in) :: got

    if ( expect .eq. got ) then
       write( *, 100 ) expect, got
    else
       write( *, 110 ) expect, got
    end if

100 format( 'PASS: Expected [',I0,'] got [',I0,']' );
110 format( 'FAIL: Expected [',I0,'] got [',I0,']' );

  end subroutine eg_integer


  !/ =====================================================================================
  subroutine eg_string( expect, got )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: expect
    character(*), intent(in) :: got

    if ( LEQ( expect, got ) ) then
       write( *, 100 ) expect, got
    else
       write( *, 110 ) expect, got
    end if

100 format( 'PASS: Expected [',A,'] got [',A,']' );
110 format( 'FAIL: Expected [',A,'] got [',A,']' );

  end subroutine eg_string


  !/ =====================================================================================
  subroutine append_item( list, str )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_list_t), intent(inout) :: list
    character(*),              intent(in)    :: str
    !/ -----------------------------------------------------------------------------------
    type(config_entry_t) :: temp
    call temp%set( LINE=str )
    call list%append( temp )
  end subroutine append_item


  !/ =====================================================================================
  subroutine set_item( list, idx, str )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_list_t), intent(inout) :: list
    integer,                   intent(in)    :: idx
    character(*),              intent(in)    :: str
    !/ -----------------------------------------------------------------------------------
    type(config_entry_t) :: temp
    call temp%set( LINE=str )
    call list%set( idx, temp )
  end subroutine set_item


  !/ =====================================================================================
  subroutine load_test_list( list )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_list_t), intent(inout) :: list
    !/ -----------------------------------------------------------------------------------

    call append_item( list,     'alpha = car ; position 1' )
    call append_item( list,     '; comment position 2' )
    call append_item( list,     '; comment position 3' )
    call append_item( list,     'bravo = horse ; position 4' )
    call append_item( list,     'charlie = jeep ; position 5' )
    call set_item(    list,  7, 'delta = truck ; position 7' )
    call set_item(    list,  8, 'echo = oxcart ; position 8' )
    call append_item( list,     '; comment position 9' )
    call append_item( list,     'foxtrot = caison ; position 10' )
    call append_item( list,     'golf = lorry ; position 11' )
    call set_item(    list, 14, 'hotel = train ; position 14' )
    call set_item(    list, 16, 'india = boat ; position 16' )
    call append_item( list,     'julet = skiff ; position 17' )

  end subroutine load_test_list








  !/ =====================================================================================
  subroutine test_config_entry
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_t) :: CE, CF
    character(:), allocatable :: res1, res2, res3

    !/ ----- set/clear ------------------------------------------------------------------

    call debug_entry( CE )
    write(*,*) ' -- set line [wife = heather ; lover] --'
    call CE%set( LINE='wife = heather ; lover' )
    call debug_entry( CE )
    write(*,*) ' -- clear --'
    call CE%clear
    call debug_entry( CE )

    write(*,*) ' -- overide COM=companion --'
    call CE%set( LINE='wife = heather ; lover', COM='companion' )
    call debug_entry( CE )

    !/ ----- copy ------------------------------------------------------------------------

    write(*,*) ' -- new entry --'
    call debug_entry( CF )
    write(*,*) ' -- copy previous entry --'
    call CF%copy(CE)
    call debug_entry( CF )

    !/ ----- isKVPair --------------------------------------------------------------------

    write(*,*) ' -- test type -- KV pair'
    call CF%clear
    call CF%set( KEY='test', VAL='pos', COM='kv pair' )
    call debug_entry( CF )
    if ( CF%isComment() ) then
       write(*,*) 'FAIL: is comment-only'
    else
       write(*,*) 'PASS: is not comment-only'
    end if

    if ( CF%isKVPair() ) then
       write(*,*) 'PASS: is KVpair'
    else
       write(*,*) 'FAIL: is not KVpair'
    end if

    !/ ----- isComment -------------------------------------------------------------------

    write(*,*) ' -- test type -- comment-only --'

    call CF%clear
    call CF%set( COM='comment only' )
    call debug_entry( CF )
    if ( CF%isComment() ) then
       write(*,*) 'PASS: is comment-only'
    else
       write(*,*) 'FAIL: is not comment-only'
    end if

    if ( CF%isKVpair() ) then
       write(*,*) 'FAIL: is KVpair'
    else
       write(*,*) 'PASS: is not KVpair'
    end if

    !/ ----- getKey/getValue/getComment --------------------------------------------------

    write(*,*) ' -- get(Key|Value|Comment) --'

    call CE%clear
    call debug_entry( CE )
    call CE%set( LINE='wife = heather ; lover', KEY='car', VAL='jeep', COM='cool' )
    call debug_entry( CE )
    res1 = CE%getKey()
    res2 = CE%getValue()
    res3 = CE%getComment()
    call ExpectGot( 'car',  res1 )
    call ExpectGot( 'jeep', res2 )
    call ExpectGot( 'cool', res3 )

    !/ ----- toString --------------------------------------------------------------------

    write(*,*) ' -- toString --'

    call CE%clear
    call CE%set( )
    call ExpectGot( '',  CE%toString() )

    call CE%clear
    call CE%set( KEY='key' )
    call ExpectGot( 'key',  CE%toString() )

    call CE%clear
    call CE%set( KEY='key', VAL='value' )
    call ExpectGot( 'key = value',  CE%toString() )

    call CE%clear
    call CE%set( COM='comment' )
    call ExpectGot( '; comment',  CE%toString() )

    call CE%clear
    call CE%set( KEY='key', COM='comment' )
    call ExpectGot( 'key ; comment',  CE%toString() )

    call CE%clear
    call CE%set( KEY='key', VAL='value', COM='comment' )
    call ExpectGot( 'key = value ; comment',  CE%toString() )

    !/ ----- fromString --------------------------------------------------------------------

    write(*,*) ' -- fromString --'

    call CE%clear
    call CE%fromString( 'key' )
    call ExpectGot( 'key',  CE%toString() )

    call CE%clear
    call CE%fromString( 'key = value' )
    call ExpectGot( 'key = value',  CE%toString() )

    call CE%clear
    call CE%fromString( '; comment' )
    call ExpectGot( '; comment',  CE%toString() )

    call CE%clear
    call CE%fromString( 'key ; comment' )
    call ExpectGot( 'key ; comment',  CE%toString() )

    call CE%clear
    call CE%fromString( 'key = value ; comment' )
    call ExpectGot( 'key = value ; comment',  CE%toString() )


    write(*,*) ' -- fromString via set --'

    call CE%clear
    call CE%set( LINE='key' )
    call ExpectGot( 'key',  CE%toString() )

    call CE%clear
    call CE%set( LINE='key = value' )
    call ExpectGot( 'key = value',  CE%toString() )

    call CE%clear
    call CE%set( LINE='; comment' )
    call ExpectGot( '; comment',  CE%toString() )

    call CE%clear
    call CE%set( LINE='key ; comment' )
    call ExpectGot( 'key ; comment',  CE%toString() )

    call CE%clear
    call CE%set( LINE='key = value ; comment' )
    call ExpectGot( 'key = value ; comment',  CE%toString() )

  end subroutine test_config_entry


  !/ =====================================================================================
  subroutine test_config_entry_list
    !/ -----------------------------------------------------------------------------------
    use tlogger
    implicit none

    type(config_entry_list_t) :: L1
    type(config_entry_t)      :: CE
    character(:), allocatable :: text
    integer                   :: st

    call tlogger_set( CONSOLE=tlogger_debug )

    !/ ----- clear/delete -------------------------------------

    call load_test_list( L1 )
    call dump_list( L1 )

    write(*,*) ' -- clear 7 --'
    call L1%clear(7)
    call dump_list( L1 )

    write(*,*) ' -- clear all --'
    call L1%clear
    call dump_list( L1 )

    write(*,*) ' -- rebuild --'
    call load_test_list( L1 )
    call dump_list( L1 )

    write(*,*) ' -- clear all --'
    call L1%clear
    call dump_list( L1 )

    write(*,*) ' -- rebuild --'
    call load_test_list( L1 )
    call dump_list( L1 )

    !/ ----- rewind/hasNext/next --------------------------

    write(*,*) ' -- rewind --'
    call load_test_list( L1 )
    call dump_list( L1 )

    call L1%rewind

100 continue
    if ( L1%hasNext() ) then
       call L1%next(CE,STATUS=st)
       if ( 0.eq.st ) then
          write(*,*) CE%toString()
       end if
    else
       goto 200
    end if
    goto 100
200 continue

    write(*,*)
    write(*,*) ' -- selected --'
    write(*,*)

    text = L1%getKey(4)
    call ExpectGot( 'bravo', text )
    
    text = L1%getKey(8)
    call ExpectGot( 'echo', text )
    
    text = L1%getValue(14)
    call ExpectGot( 'train', text )
    
    text = L1%getValue(5)
    call ExpectGot( 'jeep', text )
    
    text = L1%getComment(3)
    call ExpectGot( 'comment position 3', text )
    
    text = L1%getComment(9)
    call ExpectGot( 'comment position 9', text )

  end subroutine test_config_entry_list


  !/ =====================================================================================
  subroutine test_config_section
    !/ -----------------------------------------------------------------------------------
    use tlogger
    implicit none
    !/ -----------------------------------------------------------------------------------
    character(:), allocatable :: name
    character(:), allocatable :: text
    integer                   :: idx, ier, itest, n
    real(dp)                  :: rtest
    
    type(config_section_t) :: sec

    write(*,*) ' -- set name --'
    call sec%setName( "TEST" )
    name = sec%getName()
    call ExpectGot( 'TEST', name )
    
    call sec%addComment( 'First comment' )
    call sec%addComment( 'Second line comment' )

    call sec%append( KEY='ship', VAL='enterprise', COM='Constitution class' )
    call sec%append( COM='Third line comment' )
    call sec%append( KEY='reg', VAL='NCC-1701' )

    call sec%append( LINE='class=heavy cruiser ; exploration and combat' )
    call sec%append( LINE='; Fourth line comment' )
    call sec%append( LINE='shuttles = 7' )

    call sec%addComment( 'Fifth line comment' )
    call sec%append( KEY='warp', VAL='9.8' )
    call sec%append( KEY='comms', VAL='2.45' )
    call sec%append( KEY='phasers', VAL='2' )

    name = sec%getComment(2)
    call ExpectGot( 'Second line comment', name )

    name = sec%getComment(1)
    call ExpectGot( 'First comment', name )

    name = sec%getComment(3)
    call ExpectGot( 'Third line comment', name )

    name = sec%getComment(5)
    call ExpectGot( 'Fifth line comment', name )

    name = sec%getComment(4)
    call ExpectGot( 'Fourth line comment', name )

    idx = sec%find('ship')
    call ExpectGot( 3, idx )

    idx = sec%find('reg')
    call ExpectGot( 5, idx )

    idx = sec%find('class')
    call ExpectGot( 6, idx )

    idx = sec%find('shuttles')
    call ExpectGot( 8, idx )

    idx = sec%find('boat')
    call ExpectGot( 0, idx )

    call sec%get( 'ship', VAL=text )
    call ExpectGot( 'enterprise', text )

    write(*,*) ' -- get string --'    

    text = sec%getString( 'class', STATUS=ier )
    call ExpectGot( 0, ier )
    call ExpectGot( 'heavy cruiser', text )

    text = sec%getString( 'plane', STATUS=ier )
    call ExpectGot( 1, ier )

    write(*,*) ' -- get integer --'    

    itest = sec%getInt32( 'shuttles', STATUS=ier )
    call ExpectGot( 0, ier )
    call ExpectGot( 7, itest )
    
    itest = sec%getInt32( 'farm', STATUS=ier )
    call ExpectGot( 1, ier )
    
    itest = sec%getInt32( 'ship', STATUS=ier )
    call ExpectGot( 2, ier )
    
    write(*,*) ' -- get real --'    

    rtest = sec%getReal8( 'warp', STATUS=ier )
    call ExpectGot( 0, ier )
    call ExpectGot( 9.8_dp, rtest )
    write(*,*) '     -- next two expected to fail --'    
    call ExpectGot( 9.7_dp, rtest )
    call ExpectGot( 9.9_dp, rtest )
    
    rtest = sec%getReal8( 'farm', STATUS=ier )
    call ExpectGot( 1, ier )
    
    rtest = sec%getReal8( 'ship', STATUS=ier )
    call ExpectGot( 2, ier )
    
    write(*,*) ' -- get w/messages (these are meant to produce error messages) --'

    text  = sec%getString( 'plane' )
    itest = sec%getInt32(  'plane' )
    rtest = sec%getReal8(  'plane' )
    itest = sec%getInt32(  'ship' )
    rtest = sec%getReal8(  'ship' )

    write(*,*) ' -- count --'
    
    n = size( sec, COUNT='NUMBER' )
    call ExpectGot( 12, n )

    n = size( sec, COUNT='COMMENT' )
    call ExpectGot( 5, n )

    n = size( sec, COUNT='KVPAIR' )
    call ExpectGot( 7, n )

    
  end subroutine test_config_section

  
  !/ =====================================================================================
  subroutine test_config_db
    !/ -----------------------------------------------------------------------------------
    use configdb_mod
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(configdb_t) :: cfg
    type(configdb_t) :: cfg2
    type(config_section_t), pointer :: sec1
    type(config_section_t), pointer :: sec2
    type(config_section_t), pointer :: sec3
    integer :: i, ns, nc
    character(:), allocatable :: text
    type(config_section_t), pointer :: SC
    type(config_section_t)          :: csec
    !/ -----------------------------------------------------------------------------------

    allocate( sec1 )
    allocate( sec2 )
    allocate( sec3 )
    
    call sec1%setName( 'BRAVO' )
    call sec2%setName( 'DELTA' )
    call sec3%setName( 'ECHO' )

    call sec1%append( LINE='; section comment' )
    call sec1%set( KEY='car', VAL='gti' )
    call sec1%append( LINE='engine = F134 ; old school' )
    

    call cfg%addComment( 'First comment' )
    call cfg%add( 'ALPHA' )
    call cfg%add( sec1 )
    call cfg%addComment( 'Second comment' )
    call cfg%add( 'CHARLIE' )
    call cfg%addComment( 'Third comment' )
    call cfg%add( sec2 )
    call cfg%add( sec3 )

    ns = size( cfg, COUNT='Section' )
    nc = size( cfg, COUNT='COMMENT' )
    
    do i=1,nc
       text = cfg%getComment(i)
       if ( 0.lt.LEN(text) ) then
          write(*,100) i, text
       end if
    end do
    
    do i=1,ns
       SC => cfg%get(i)
       text = SC%getName()
       if ( 0.lt.LEN(text) ) then
          write(*,200) i, text
       end if
    end do
    
100 format( 'C',I0,' [',A,']' )
200 format( 'S',I0,' [',A,']' )

    write (*,*)
    write (*,*) '===== TEST WRITE ======================================================='
    
    call cfg%writeINI( UNIT=OUTPUT_UNIT )
    
    write (*,*) '===== READ ============================================================='

    call cfg2%readINI( FILE='../config_test.ini' )

    write (*,*) '----- WRITE ------------------------------------------------------------'
    
    call cfg2%writeINI( UNIT=OUTPUT_UNIT )

    write (*,*) '========================================================================'
    write (*,*)

    call csec%clear

    call csec%setName( 'Alice' )
    call csec%set( KEY='girl', VAL='heather', COM='first and only' )
    
    call csec%fromCommandLine(PNAME='cli', FILE_PREFIX='comp', CLEAR=.false.)
    write(OUTPUT_UNIT,111) csec%getName()
    call csec%writeINI(UNIT=OUTPUT_UNIT)
111 format( '[',A,']' )
  end subroutine test_config_db


  subroutine test01
   implicit none
   character(*), parameter   :: test = '  [  section ]       '
   character(:), allocatable :: buffer

   buffer = TRIM( ADJUSTL( test ) )

   write(*,100) buffer
   write(*,100) TRIM(ADJUSTL(buffer(2:LEN_TRIM(buffer)-1)))

   100 format( '>xx>',A,'<<' )

  end subroutine test01

  
  !/ =====================================================================================
  subroutine test_section_merge
    !/ -----------------------------------------------------------------------------------
    use configdb_mod
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(config_section_t) :: sec1, sec2

    call sec1%append( LINE='; Section one' )
    call sec1%append( LINE='alpha = chevy ; my first car' )
    call sec1%append( LINE='beta = jeep' )
    call sec1%append( LINE='; another comment' )
    call sec1%append( LINE='delta = pontiac' )

    call sec2%append( LINE='; Section two' )
    call sec2%append( LINE='alpha = vw ; her first car' )
    call sec2%append( LINE='gamma = saturn' )
    call sec2%append( LINE='epsilon = GTI' )
    call sec2%append( LINE='; a last comment' )

    write(OUTPUT_UNIT,*) '===== merge test =================='
    call sec1%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '-----------------------------------'
    call sec2%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '==================================='
    call sec1%merge( sec2 )
    call sec1%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '==================================='

  end subroutine test_section_merge

    
  !/ =====================================================================================
  subroutine test_config_merge
    !/ -----------------------------------------------------------------------------------
    use configdb_mod
    implicit none
    !/ -----------------------------------------------------------------------------------

    type(configdb_t) :: db1, db2

    call db1%readINI( FILE='../config_test_1.ini' )
    call db2%readINI( FILE='../config_test_2.ini' )

    write(OUTPUT_UNIT,*) '===== merge test =================='
    call db1%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '-----------------------------------'
    call db2%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '==================================='
    call db1%merge( db2 )
    call db1%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '==================================='

  end subroutine test_config_merge

   !/ =====================================================================================
  subroutine test_config_merge1
    !/ -----------------------------------------------------------------------------------
       use configdb_mod
    implicit none
    !/ -----------------------------------------------------------------------------------

    type(configdb_t) :: db1, db2

    call db1%readINI( FILE='../config_test_1.ini' )
    call db2%readINI( FILE='../config_test_2.ini' )
    write(OUTPUT_UNIT,*) '========================================================'
    call db1%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '========================================================'
    call db2%writeINI( UNIT=OUTPUT_UNIT )
    call db1%merge( db2 )
    write(OUTPUT_UNIT,*) '========================================================'
    call db1%writeINI( UNIT=OUTPUT_UNIT )
    write(OUTPUT_UNIT,*) '========================================================'

  end subroutine test_config_merge1

  
end module ftest_config_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_config_test
  implicit none
  !/ -------------------------------------------------------------------------------------

  !write (*,*) '===== CONFIG ENTRY ============================================='
  !write (*,*)
  !call test_config_entry
  !write (*,*)
  
  !write (*,*)
  !write (*,*) '===== CONFIG LIST =============================================='
  !write (*,*)
  !call test_config_entry_list
  !write (*,*)
  
  !write (*,*)
  !write (*,*) '===== CONFIG SECTION ==========================================='
  !write (*,*)
  !call test_config_section
  !write (*,*)

  !write (*,*)
  !write (*,*) '===== CONFIG DB ================================================'
  !write (*,*)
  !call test_config_db
  !write (*,*)

  !write (*,*)
  !write (*,*) '===== SECTION MERGE ============================================'
  !write (*,*)
  !call test_section_merge
  !write (*,*)


  !write (*,*)
  !write (*,*) '===== CONFIG MERGE ============================================='
  !write (*,*)
  call test_config_merge1
  !write (*,*)



end program main

!/ =======================================================================================
!/ **                                 F T E S T _ V L A                                 **
!/ =========================================================================== END FILE ==
