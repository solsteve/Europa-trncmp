
CFLAG=-march=corei7 -mtune=corei7 -ffast-math -ftree-vectorizer-verbose=0 \
      -fopt-info-optimized -Wall -pedantic-errors -Wconversion -Wunused-parameter

all: ftest runga
	chmod 750 *.py

clean:
	rm -f *.o ftest runga

fullclean: clean
	rm -f *.pyc

distclean: fullclean
	rm -rf __pycache__
	rm -f *~

%.o: %.cc
	gcc -xc++ -c -O3 $(CFLAG) -I. $< -o $@

ftest: ftest.o flib.o fga.o
	gcc -o $@ $^ -lstdc++ -lm

runga: runga.o flib.o fga.o
	gcc -o $@ $^ -lstdc++ -lm

ftest.o: ftest.cc fga.hh flib.hh
runga.o: runga.cc fga.hh flib.hh
flib.o:  flib.cc flib.hh
fga.o:   fga.cc  flib.hh
