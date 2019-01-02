#/ ====================================================================== BEGIN FILE =====
#/ **                           Top Level - C M A K E L I S T                           **
#/ =======================================================================================
#/ **                                                                                   **
#/ **  Copyright ( c) 2018, Stephen W. Soliday                                           **
#/ **                      stephen.soliday@trncmp.org                                   **
#/ **                      http://research.trncmp.org                                   **
#/ **                                                                                   **
#/ **  -------------------------------------------------------------------------------  **
#/ **                                                                                   **
#/ **  This program is free software: you can redistribute it and/or modify it under    **
#/ **  the terms of the GNU General Public License as published by the Free Software    **
#/ **  Foundation, either version 3 of the License, or ( at your option)                 **
#/ **  any later version.                                                               **
#/ **                                                                                   **
#/ **  This program is distributed in the hope that it will be useful, but WITHOUT      **
#/ **  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or FITNESS    **
#/ **  FOR A PARTICULAR PURPOSE. See the GNU General Public License for more details.   **
#/ **                                                                                   **
#/ **  You should have received a copy of the GNU General Public License along with     **
#/ **  this program. If not, see <http://www.gnu.org/licenses/>.                        **
#/ **                                                                                   **
#/ ----- Modification History ------------------------------------------------------------
#/
#/ @brief   Build environment.
#/
#/ @details Provides a common build environment.
#/
#/ @author  Stephen W. Soliday
#/ @date    2018-04-21
#/
#/ =======================================================================================

cmake_minimum_required ( VERSION 3.5 )

project ( Europa Fortran )
set ( PROJECT_DESCRIPTION "Europa Frotran Libraries" )
set ( PROJECT_URL "https://github.com/solsteve" )

enable_language ( Fortran )
include ( GNUInstallDirs )

set ( Europa_LIBNAME europa )
set ( Europa_MAJOR_VERSION 2 )
set ( Europa_MINOR_VERSION 0 )
set ( Europa_PATCH_VERSION 0.dev )
set ( Europa_VERSION "${Europa_MAJOR_VERSION}.${Europa_MINOR_VERSION}.${Europa_PATCH_VERSION}" )

set ( CMAKE_CONFIGURATION_TYPES "DEBUG" "RELEASE" )
set ( CMAKE_BUILD_TYPE "RELEASE" CACHE STRING "Select which configuration to build." )

#/ =======================================================================================
#/ Check whether the user has selected a BUILD_TYPE
#/ ---------------------------------------------------------------------------------------

set ( CMAKE_CC_COMPILER  "gcc" )
set ( CMAKE_CXX_COMPILER "g++" )

if ( "${CMAKE_BUILD_TYPE}" STREQUAL "" )

  message ( STATUS "Build type not explicitly set. Trying to guess..." )
  get_filename_component ( BINDIR ${PROJECT_BINARY_DIR} NAME )
  
  if ( ${BINDIR} MATCHES "RELEASE" )
    set ( CMAKE_BUILD_TYPE "RELEASE" )
    message ( STATUS "Set to ${CMAKE_BUILD_TYPE}" )
  elseif ( ${BINDIR} MATCHES "DEBUG" )
    set ( CMAKE_BUILD_TYPE "DEBUG" )
    message ( STATUS "Set to ${CMAKE_BUILD_TYPE}" )
  else ( )
    message ( WARNING "Unable to deduce build type. Use -DCMAKE_BUILD_TYPE=<RELEASE/DEBUG>" )
  endif ( ${BINDIR} MATCHES "RELEASE" )
  
else ( )
  message ( STATUS "Build Type is ${CMAKE_BUILD_TYPE}" )
endif ( "${CMAKE_BUILD_TYPE}" STREQUAL "" )

set ( GARCH "-fverbose-asm -march=corei7 -mtune=corei7 -ffast-math -ftree-vectorizer-verbose=0 -fopt-info-optimized -fopenmp -msse2 -mrdrnd" )
set ( GWARN "-Wall -pedantic-errors -Wconversion -Wimplicit-interface -Wunused-parameter -Wcharacter-truncation -Wunderflow -Wno-surprising" )
set ( GFORM "-std=f2008ts -ffree-form -fall-intrinsics -frecursive" )


set ( CMAKE_Fortran_FLAGS_RELEASE "-O3 ${GWARN} ${GARCH} ${GFORM}" )
set ( CMAKE_Fortran_FLAGS_DEBUG   "-O1 -g -fbacktrace -Warray-temporaries -fcheck=array-temps -fcheck=all ${GWARN} ${GARCH} ${GFORM}" )


#/ =======================================================================================
#/ /usr/include/hdf5/serial

set ( CMAKE_Fortran_MODULE_DIRECTORY ${PROJECT_BINARY_DIR}/europa/mods )

include_directories ( ${CMAKE_Fortran_MODULE_DIRECTORY} )
file ( MAKE_DIRECTORY ${CMAKE_Fortran_MODULE_DIRECTORY} )

set ( CMAKE_INSTALL_PREFIX ${PROJECT_BINARY_DIR}/europa )


#/ =======================================================================================
#/ if DEBUG requested, set the various debug options.
#/ ---------------------------------------------------------------------------------------

if ( ${CMAKE_BUILD_TYPE} MATCHES "DEBUG" )
  message ( STATUS "DEBUG Compile Options: ${CMAKE_Fortran_FLAGS_DEBUG}" )
else ( )
  if ( ${CMAKE_BUILD_TYPE} MATCHES "RELEASE" )
    message ( STATUS "RELEASE Compile Options: ${CMAKE_Fortran_FLAGS_RELEASE}" )
  else ( )
    message ( STATUS "Compiler option could not be set" )
  endif ( )
endif ( )

#/ =======================================================================================
#/ Build pFUnit test code.
#/ ---------------------------------------------------------------------------------------


set ( PFUNIT_DIR $ENV{PFUNIT} )

if ( IS_DIRECTORY ${PFUNIT_DIR} )
  message ( STATUS "pFUnit found in ${PFUNIT_DIR}" )
  
  if ( ${CMAKE_BUILD_TYPE} MATCHES "DEBUG" )
    message ( STATUS "Adding compile options -fprofile-arcs -ftest-coverage" )
    set ( CMAKE_Fortran_FLAGS_DEBUG "${CMAKE_Fortran_FLAGS_DEBUG} -fprofile-arcs -ftest-coverage" )
  endif ( )
  
endif ( IS_DIRECTORY ${PFUNIT_DIR} )


#/ =======================================================================================

find_package ( BLAS )
find_package ( LAPACK )
find_package ( HDF5 COMPONENTS Fortran )

add_subdirectory ( src )
add_subdirectory ( tools )
add_subdirectory ( apps )


#/ =======================================================================================


if ( IS_DIRECTORY ${PFUNIT_DIR} )
  add_subdirectory ( src/utest )
  enable_testing ( )
  
  # Add the tests below
  add_test ( NAME UnitTest1 COMMAND arrays  )
  add_test ( NAME UnitTest2 COMMAND container  )
  add_test ( NAME UnitTest3 COMMAND mathaux )
  
else ( )
  message ( WARNING "pFUnit not installed, testing disabled" )
endif ( )


#/ =======================================================================================
#/ **                           Top Level - C M A K E L I S T                           **
#/ =========================================================================== end FILE ==
