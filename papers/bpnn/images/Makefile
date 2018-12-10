%.eps: %.png
	convert $< $@

IMAGES = Soliday.eps ShipPCA.eps Wake.eps

all: $(IMAGES)

clean:
	rm -f $(IMAGES)

fullclean: clean
	rm -f *.bak *~

Soliday.eps: Soliday.png
Wake.eps: Wake.png
ShipPCA.eps: ShipPCA.png
