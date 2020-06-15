all:
	ls -lpa

clean:
	make -C anomaly $@
	make -C bpnn    $@
	make -C nlls    $@

fullclean: clean
	rm -f *~
	make -C anomaly $@
	make -C bpnn    $@
	make -C nlls    $@

distclean: fullclean
	@echo "Done ... $@"

