#/ ====================================================================== BEGIN FILE =====
#/ **                                  M A K E F I L E                                  **
#/ =======================================================================================
#/ **                                                                                   **
#/ **  Copyright (c) 2018, Stephen W. Soliday                                           **
#/ **                      stephen.soliday@trncmp.org                                   **
#/ **                      http://research.trncmp.org                                   **
#/ **                                                                                   **
#/ **  -------------------------------------------------------------------------------  **
#/ **                                                                                   **
#/ **  This program is free software: you can redistribute it and/or modify it under    **
#/ **  the terms of the GNU General Public License as published by the Free Software    **
#/ **  Foundation, either version 3 of the License, or (at your option)                 **
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
#/ @brief   Controler.
#/
#/ @details Provides a wrapper for cmake.
#/
#/ @author  Stephen W. Soliday
#/ @date    2018-04-21
#/
#/ =======================================================================================

.PHONY: all 

all: debug

COMPILERS=-DCMAKE_Fortran_COMPILER=gfortran
XCOMPILERS=-DCMAKE_Fortran_COMPILER=gfortran -DCMAKE_CXX_COMPILER=g++ -DCMAKE_CC_COMPILER=gcc

#/ ---------------------------------------------------------------------------------------

debug: DEBUG
	make -C $<

DEBUG:
	mkdir -p $@
	cd $@; cmake $(COMPILERS) -DCMAKE_BUILD_TYPE=$@ ..

#/ ---------------------------------------------------------------------------------------

release: RELEASE
	make -C $<

RELEASE:
	mkdir -p $@
	cd $@; cmake $(COMPILERS) -DCMAKE_BUILD_TYPE=$@ ..

#/ ---------------------------------------------------------------------------------------

install: release
	make -C RELEASE install

#/ ---------------------------------------------------------------------------------------

check: DEBUG
	make -C $< test

#/ ---------------------------------------------------------------------------------------

pages:
	make -C docs

#/ =======================================================================================

clean:
	make -C docs   $@
	make -C papers $@


fullclean: clean
	make -C docs $@
	rm -rf RELEASE DEBUG
	find . -name "*~" | xargs rm -f
	make -C papers $@
	make -C python $@


distclean: fullclean
	make -C python $@


#/ =======================================================================================
#/ **                                  M A K E F I L E                                  **
#/ =========================================================================== END FILE ==
