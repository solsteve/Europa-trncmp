%.eps: %.png
	convert $< $@

IMAGES = Soliday.eps ShipPCA.eps Chips.eps Rendered.eps Wireframe.eps

all: $(IMAGES)

clean:
	rm -f $(IMAGES)

fullclean: clean
	rm -f *.bak *~

Soliday.eps: Soliday.png
ShipPCA.eps: ShipPCA.png
