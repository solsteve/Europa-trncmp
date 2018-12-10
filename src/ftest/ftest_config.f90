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
  use config_section_mod
  implicit none




  
  !/ =====================================================================================
contains !/**                   P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================



  

  !/ =====================================================================================
  subroutine check( ent )
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_t), intent(inout) :: ent
    
    if ( ent%isComment() ) then
       write(*,*) '   comment only'
    else
       write(*,*) '   NOT comment only'
    end if
    
    if ( ent%isKVPair() ) then
       write(*,*) '   key value pair'
    else
       write(*,*) '   NOT key value pair'
    end if

  end subroutine check
    
  !/ =====================================================================================
  subroutine test01( line )
    !/ -----------------------------------------------------------------------------------
    implicit none
    character(*), intent(in)  :: line
    !/ -----------------------------------------------------------------------------------
    type(config_entry_t) :: CE
    character(:), allocatable :: res

    write(*,*) 'line   :: [', line, ']'
    call CE%set( LINE=line )
    call CE%debug
    res = CE%toString()

    write(*,*) 'result :: [', res,  ']'

    call check( CE )

    write(*,*) ' -- clear --'
    call CE%clear
    call CE%debug

  end subroutine test01

  
  !/ =====================================================================================
  subroutine test02
    !/ -----------------------------------------------------------------------------------
    implicit none
    type(config_entry_t) :: CE, other
    character(:), allocatable :: res1, res2, res3, res4
    character(:), allocatable :: act1, act2, act3

    call CE%debug
    write(*,*) ' -- clear --'
    call CE%clear
    call CE%debug
    write(*,*) ' -- set line [wife = heather ; lover] --'
    call CE%set( LINE='wife = heather ; lover' )
    call CE%debug
    write(*,*) ' -- clear --'
    call CE%clear
    call CE%debug
    write(*,*) ' -- set line [wife = heather ; lover] -- then overide --'
    write(*,*) ' -- KEY=car VAL=jeep COM=cool --'
    call CE%set( LINE='wife = heather ; lover', KEY='car', VAL='jeep', COM='cool' )
    call CE%debug
    call CE%get( KEY=res1, VAL=res2, COM=res3, LINE=res4 )
    write(*,100) 'car', res1
    write(*,100) 'jeep', res2
    write(*,100) 'cool', res3
    write(*,100) 'car = jeep ; cool', res4
    write(*,*) ' -- clear --'
    call CE%clear
    call CE%debug
    write(*,*) ' -- set line [cat = astrid ; hunter]'
    call CE%set( LINE='cat = astrid ; hunter' )
    act1 = CE%getKey()
    act2 = CE%getValue()
    act3 = CE%getComment()
    write(*,100) 'cat',    act1
    write(*,100) 'astrid', act2
    write(*,100) 'hunter', act3
    write(*,*) ' -- clear other --'
    call other%clear
    call other%debug
    write(*,*) ' -- copy --'
    call other%copy(CE)
    call CE%get(    LINE=res1 )
    call other%get( LINE=act1 )
    write(*,100) res1, act1
100 format( 'Expected [',A,'] got[',A,']' );
    
  end subroutine test02

   !/ =====================================================================================
  subroutine test03
    !/ -----------------------------------------------------------------------------------
    implicit none
    !/ -----------------------------------------------------------------------------------
    type(config_section_t) :: sec
    integer :: n
    !/ -----------------------------------------------------------------------------------
    

    call sec%setName( 'Empty' )
    write(*,*) sec%getName()
    
    call sec%setName( 'BPNN' )
    write(*,*) sec%getName()

    call sec%addComment( 'First comment line' )
    call sec%append( COM='Second comment line' )
    call sec%append( LINE='input = 14 ; number of inputs' )
    call sec%append( LINE='; Third comment line' )
    call sec%append( KEY='output', VAL='3', COM='number of outputs' )

    n = size( sec )
    write(*,*) 'Number of records  =', n

    n = size( sec, COUNT='number' )
    write(*,*) 'Number of records  =', n
    
    write(*,*) 'Number of KV pairs =', size( sec, COUNT='K' )
    write(*,*) 'Number of Comments =', size( sec, COUNT='com' )

  end subroutine test03

  
end module ftest_config_test


!/ =======================================================================================
program main
  !/ -------------------------------------------------------------------------------------
  use ftest_config_test
  implicit none
  !/ -------------------------------------------------------------------------------------
  character(*), parameter :: testA = '   wife   '                      ! 1 or 2
  character(*), parameter :: testB = '   wife =  heather  '            ! 3
  character(*), parameter :: testC = '   wife = heather  ;  lover '    ! 7
  character(*), parameter :: testD = '   wife  ;    lover '            ! 5 or 6
  character(*), parameter :: testE = '   ;    lover '                  ! 4

  write (*,*)
  call test01( testA )
  write (*,*)
  write (*,*) '--------------------------------------------------'
  write (*,*)
  call test01( testB )
  write (*,*)
  write (*,*) '--------------------------------------------------'
  write (*,*)
  call test01( testC )
  write (*,*)
  write (*,*) '--------------------------------------------------'
  write (*,*)
  call test01( testD )
  write (*,*)
  write (*,*) '--------------------------------------------------'
  write (*,*)
  call test01( testE )
  write (*,*)
  write (*,*) '=================================================='
  write (*,*)
  call test02
  write (*,*)
  write (*,*)
  write (*,*) '=================================================='
  write (*,*)
  call test03
  write (*,*)

end program main

!/ =======================================================================================
!/ **                                 F T E S T _ V L A                                 **
!/ =========================================================================== END FILE ==
