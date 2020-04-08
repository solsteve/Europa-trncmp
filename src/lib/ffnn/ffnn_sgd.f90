!/ ====================================================================== BEGIN FILE =====
!/ **                              F F N N _ S G D _ M O D                              **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 1994-2020, Stephen W. Soliday                                      **
!/ **                           stephen.soliday@trncmp.org                              **
!/ **                           http://research.trncmp.org                              **
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
module FFNN_SGD_mod
  !/ -------------------------------------------------------------------------------------
  !! Provides a class performing stochastic gradient decent using the FFNN.
  !!
  !! author:  Stephen W. Soliday
  !! date:    2020-Apr-07
  !! license: GPL
  !! -------------------------------------------------------------------------------------
  use trncmp_env
  use dice_mod
  use tlogger
  use ffnn_network_mod
  use dice_mod
  implicit none

  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================

  
  !/ =====================================================================================
  subroutine SGD_TRAIN_WITHOUT_REPLACEMENT( input, desired, network, max_bat, score, verb )
    !! -----------------------------------------------------------------------------------
    !! Perform stochastic gradient decent using selection without replacement.
    !! Every epoch is guaranteed to try every exemplar pair.
    !! -----------------------------------------------------------------------------------
    implicit none
    real(dp),   intent(in)    :: input(:,:)
    real(dp),   intent(in)    :: desired(:,:)
    type(FFNN), intent(inout) :: network
    integer,    intent(in)    :: max_bat
    real(dp),   intent(out)   :: score
    integer,    intent(in)    :: verb
    !! -----------------------------------------------------------------------------------
    integer              :: num_samp, test, max_trial
    integer              :: bat, bat_size, i, j, k
    integer, allocatable :: bat_index(:)
    real(dp)             :: loss, mse
    type(Dice)           :: dd
    !! -----------------------------------------------------------------------------------

    num_samp = size(input, DIM=2)

    bat_size = num_samp / max_bat
    test = modulo( num_samp, max_bat )
    if ( 0.lt.test ) bat_size = bat_size + 1
    max_trial = bat_size * max_bat

    allocate( bat_index( max_trial ) )
    do i=1,num_samp
       bat_index(i) = i
    end do
    do i=num_samp+1,max_trial
       bat_index(i) = i - num_samp
    end do

    call dd%shuffle( bat_index )

    !! -----------------------------------------------------------------------------------

    j = 1
    score = D_ZERO
    do bat=1,max_bat
       call network%reset
       loss = D_ZERO
       do i=1,bat_size
          k = bat_index(j)
          j = j + 1
          call network%train( input(:,k), desired(:,k), MSE=mse )
          loss = loss + mse
       end do
       call network%update
       loss = loss / real(bat_size,dp)
       score = score + loss
       if ( 1.lt.verb) then
       print *, bat, max_bat, loss
    end if
    end do
    score = score / real(max_bat,dp)

    deallocate( bat_index )
    
  end subroutine SGD_TRAIN_WITHOUT_REPLACEMENT



  
  !/ =====================================================================================
  subroutine SGD_TRAIN_WITH_REPLACEMENT( input, desired, network, max_bat, score, verb )
    !! -----------------------------------------------------------------------------------
    !! Perform stochastic gradient decent using selection with replacement.
    !! There is no guaranteed to try every exemplar pair, each epoch.
    !! However, statistically every exemplar should eventually get tried.
    !! -----------------------------------------------------------------------------------
    implicit none
    real(dp),   intent(in)    :: input(:,:)
    real(dp),   intent(in)    :: desired(:,:)
    type(FFNN), intent(inout) :: network
    integer,    intent(in)    :: max_bat
    real(dp),   intent(out)   :: score
    integer,    intent(in)    :: verb
    !! -----------------------------------------------------------------------------------
    integer    :: num_samp, test, bat, bat_size, i, j
    real(dp)   :: loss, mse
    type(Dice) :: dd
    !! -----------------------------------------------------------------------------------

    call dd%seed_set
    
    num_samp = size(input, DIM=2)

    bat_size = num_samp / max_bat
    test = modulo( num_samp, max_bat )
    if ( 0.lt.test ) bat_size = bat_size + 1

    !! -----------------------------------------------------------------------------------

    score = D_ZERO
    do bat=1,max_bat
       call network%reset
       loss = D_ZERO
       do i=1,bat_size
          j = dd%index(num_samp)
          call network%train( input(:,j), desired(:,j), MSE=mse )
          loss = loss + mse
       end do
       call network%update
       loss = loss / real(bat_size,dp)
       score = score + loss
       if ( 1.lt.verb ) then
          print *, bat, max_bat, loss
       end if
    end do
    score = score / real(max_bat,dp)

  end subroutine SGD_TRAIN_WITH_REPLACEMENT




  !/ =====================================================================================
  subroutine SGD_TRAIN( input, desired, network, epochs, batch, replace, verbose )
    !! -----------------------------------------------------------------------------------
    implicit none
    real(dp),   optional, intent(in)    :: input(:,:)
    real(dp),   optional, intent(in)    :: desired(:,:)
    type(FFNN), optional, intent(inout) :: network
    integer,    optional, intent(in)    :: epochs
    integer,    optional, intent(in)    :: batch
    logical,    optional, intent(in)    :: replace
    integer,    optional, intent(in)    :: verbose
    !! -----------------------------------------------------------------------------------
    real(dp) :: score
    integer  :: epc, max_epc, max_bat, verb
    logical  :: rep
    !! -----------------------------------------------------------------------------------

    rep = .true.
    if ( present( REPLACE ) ) rep = REPLACE

    verb = 1
    if ( present( VERBOSE ) ) verb = verbose

    if ( present( input ) ) then
       if ( present( desired ) ) then
          if ( size(input, DIM=2) .eq. size(desired, DIM=2) ) then
             if ( present( network ) ) then

                if ( present( epochs ) ) then
                   max_epc = epochs
                else
                   max_epc = 10
                   call log_info( 'Using default number of epochs', I4=max_epc )
                end if

                if ( present( batch ) ) then
                   max_bat = batch
                else
                   max_bat = 10
                   call log_info( 'Using default number of batches', I4=max_bat )
                end if

                do epc=1,max_epc
                   if ( rep ) then
                      call SGD_TRAIN_WITH_REPLACEMENT(input,desired,network,max_bat,score,verb)
                   else
                      call SGD_TRAIN_WITHOUT_REPLACEMENT(input,desired,network,max_bat,score,verb)
                   end if
                   if ( 0.lt.verb ) then
                   print *, epc, max_epc, score
                end if
             end do

             if ( 0.eq.verb ) then
                print *, 'Final MSE=', score
             end if
             
             else
                call log_error('input and desired table are different lengths')
             end if
          else
             call log_error('No network was provided, use NETWORK=')
          end if
       else
          call log_error('No desired output was provided, use DESIRED=')
       end if
    else
       call log_error('No input was provided, use INPUT=')
    end if

  end subroutine SGD_TRAIN










    



  

end module FFNN_SGD_mod
