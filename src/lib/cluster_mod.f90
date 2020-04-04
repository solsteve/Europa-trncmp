!/ ====================================================================== BEGIN FILE =====
!/ **                                   P C A _ M O D                                   **
!/ =======================================================================================
!/ **                                                                                   **
!/ **  This file is part of the TRNCMP Research Library, `Europa' (Fortran 2018)        **
!/ **                                                                                   **
!/ **  Copyright (c) 2020, Stephen W. Soliday                                           **
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
module cluster_mod
  !/ -------------------------------------------------------------------------------------
  !! author:  Stephen W. Soliday
  !! date:    2020-03-09
  !! license: GPL
  !!
  !!## Cluster
  !!
  !! Collection of procedures for basic clustering.
  !
  !/ -------------------------------------------------------------------------------------
  use tc_lapack
  use tlogger
  implicit none


  !/ =====================================================================================
  type :: ClusterPoint
     !/ ----------------------------------------------------------------------------------
     real(dp), allocatable :: X(:)
     integer               :: n_var
   contains
  end type ClusterPoint

  
  !/ =====================================================================================
  type :: Cluster
     !/ ----------------------------------------------------------------------------------
     type(ClusterPoint), allocatable :: data(:)
     integer                         :: n_points
   contains
  end type Cluster




  !/ =====================================================================================
contains !/ **                  P R O C E D U R E   S E C T I O N                       **
  !/ =====================================================================================


END module cluster_mod
