%.eps: %.png
	convert $< $@

IMAGES = Soliday.eps

all: $(IMAGES)

clean:
	rm -f $(IMAGES)

fullclean: clean
	rm -f *.bak *~

Soliday.eps: Soliday.png
