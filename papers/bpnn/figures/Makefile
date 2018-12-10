%.eps: %.fig
	fig2dev -L eps $< $@

all: threelayer.eps MiddleNet.eps

clean:
	rm -f *.eps

fullclean: clean
	rm -f *.bak *~

threelayer.eps: threelayer.fig
MiddleNet.eps:  MiddleNet.fig
