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
#/  @file Makefile
#/   Provides build environment
#/
#/  @author Stephen W. Soliday
#/  @date 2018-09-11
#/
#/ =======================================================================================

TEX2DVI = latex
DVI2PS  = dvips -D 600 -Z
PS2PDF  = ps2pdf
FIG2EPS = fig2dev -L eps
BIBTEX  = bibtex
TEX2RTF = latex2rtf -M12 -F

RM      = rm -f

#/ =======================================================================================

PAPER  = DerivationBackprop

%.dvi: %.tex
	$(TEX2DVI) $<
	$(BIBTEX)  $*
	$(TEX2DVI) $<
	$(TEX2DVI) $<

%.rtf: %.tex
	$(TEX2DVI) $<
	$(BIBTEX)  $*
	$(TEX2DVI) $<
	$(TEX2DVI) $<
	$(TEX2RTF) -o $@ $<

%.pdf: %.ps
	$(PS2PDF) $< $@

%.ps: %.dvi
	$(DVI2PS) -o $@ $<

#  ==========================================================================

all: $(PAPER).dvi
	xdvi $<

dvi: $(PAPER).dvi
ps:  $(PAPER).ps
pdf: $(PAPER).pdf
rtf: $(PAPER).rtf

FIGS:
	make -C figures

IMGS:
	make -C images

clean:
	$(RM) *.aux *.dvi *.log *.bbl *.blg
	$(RM) *.ps *.pdf *.rtf *.toc
	make -C figures $@
	make -C images  $@

fullclean: clean
	$(RM) *~
	make -C figures $@
	make -C images  $@

$(PAPER).dvi: $(PAPER).tex $(BIBLIO) FIGS IMGS
$(PAPER).rtf: $(PAPER).tex $(BIBLIO)
$(PAPER).ps:  $(PAPER).dvi
$(PAPER).pdf: $(PAPER).ps


#/ =======================================================================================
#/ **                                  M A K E F I L E                                  **
#/ ======================================================================== END FILE =====

