%.eps: %.fig
	fig2dev -L eps $< $@

all: AutoGen.eps 

clean:
	rm -f *.eps

fullclean: clean
	rm -f *.bak *~

AutoGen.eps: AutoGen.fig
