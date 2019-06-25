#/ ====================================================================== BEGIN FILE =====
#/ **                                  M A K E F I L E                                  **
#/ =======================================================================================
#/ **                                                                                   **
#/ **  Copyright (c) 2018, L3 Technologies Advanced Programs                            **
#/ **                      One Wall Street #1, Burlington, MA 01803                     **
#/ **                                                                                   **
#/ **  -------------------------------------------------------------------------------  **
#/ **                                                                                   **
#/ **  This file, and associated source code, is not free software; you may not         **
#/ **  redistribute it and/or modify it. This file is part of a research project        **
#/ **  that is in a development phase. No part of this research has been publicly       **
#/ **  distributed. Research and development for this project has been at the sole      **
#/ **  cost in both time and funding by L3 Technologies Advanced Programs.              **
#/ **                                                                                   **
#/ **  Any reproduction of computer software or portions thereof marked with this       **
#/ **  legend must also reproduce the markings.  Any person who has been provided       **
#/ **  access to such software must promptly notify L3 Technologies Advanced Programs.  **
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

PAPER  = Anomaly

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

