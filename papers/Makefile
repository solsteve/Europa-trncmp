all:
	ls -lpa

clean:
	make -C anomaly $@
	make -C bpnn    $@

fullclean: clean
	rm -f *~
	make -C anomaly $@
	make -C bpnn    $@

distclean: fullclean
	@echo "Done ... $@"

