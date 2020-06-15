%.eps: %.fig
	fig2dev -L eps $< $@

all: newton.eps LocalSetup.eps 

clean:
	rm -f newton.eps LocalSetup.eps 

fullclean: clean
	rm -f *.bak *~

newton.eps: newton.fig
LocalSetup.eps: LocalSetup.fig
