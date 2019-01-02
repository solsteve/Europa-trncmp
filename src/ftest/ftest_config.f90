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




  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
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
    integer :: i, n
    class(config_entry_t), pointer :: CE
    character(:), allocatable :: rep
    !/ -----------------------------------------------------------------------------------

    n = size( list )

    do i=1,n
       CE => list%get( i )
       if ( associated( CE ) ) then
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
          nullify( CE )
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
  subroutine ExpectGot( expect, got )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in) :: expect
    character(*), intent(in) :: got

    if ( LEQ( expect, got ) ) then
       write( *, 100 ) expect, got
    else
       write( *, 110 ) expect, got
    end if
    
100 format( 'PASS: Expected [',A,'] got[',A,']' );
110 format( 'FAIL: Expected [',A,'] got[',A,']' );

  end subroutine ExpectGot

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
    call append_item( list,     'bravo = horse ; position 2' )
    call append_item( list,     'charlie = jeep ; position 3' )
    call set_item(    list,  6, 'delta = truck ; position 6' )
    call set_item(    list,  5, 'echo = oxcart ; position 5' )
    call append_item( list,     'foxtrot = caison ; position 7' )
    call append_item( list,     'golf = lorry ; position 8' )
    call set_item(    list, 10, 'hotel = train ; position 10' )
    call set_item(    list, 15, 'india = boat ; position 15' )
    call append_item( list,     'julet = skiff ; position 16' )

  end subroutine load_test_list
  
  
  !/ =====================================================================================
  subroutine test_config_entry_list
    !/ -----------------------------------------------------------------------------------
    use tlogger
    implicit none

    type(config_entry_list_t) :: L1
    type(config_entry_t), pointer      :: CE
    
    call tlogger_set( CONSOLE=tlogger_debug )

!/ ----- clear/delete -------------------------------------

    call load_test_list( L1 )
    call dump_list( L1 )

    write(*,*) ' -- clear 7 --'
    call L1%clear(7)
    call dump_list( L1 )

    write(*,*) ' -- delete 10 --'
    call L1%delete(10)
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

    write(*,*) ' -- delete all --'
    call L1%delete
    call dump_list( L1 )

    !/ ----- rewind/hasNext/next --------------------------

    write(*,*) ' -- rewind --'
    call load_test_list( L1 )
    call dump_list( L1 )

    call L1%rewind

100 continue
    if ( L1%hasNext() ) then
       CE => L1%next()
       if ( associated( CE ) ) then
          write(*,*) CE%toString()
       end if
    else
       goto 200
    end if
    goto 100
200 continue
          

  end subroutine test_config_entry_list

  
end module ftest_config_test


  !/ =======================================================================================
  program main
    !/ -------------------------------------------------------------------------------------
    use ftest_config_test
    implicit none
    !/ -------------------------------------------------------------------------------------

    write (*,*) '===== CONFIG ENTRY ============================================='
    write (*,*)
    call test_config_entry
    write (*,*)
    write (*,*)
    write (*,*) '===== CONFIG LIST =============================================='
    write (*,*)
    call test_config_entry_list
    !call test02
    write (*,*)

  end program main

  !/ =======================================================================================
  !/ **                                 F T E S T _ V L A                                 **
  !/ =========================================================================== END FILE ==
