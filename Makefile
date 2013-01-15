all:
	ghc --make -O2 train
	ghc --make -O2 modelstats

clean:
	rm -f train modelstats
	find . -name \*.hi -delete
	find . -name \*.o -delete

