all:
	ghc --make -O2 train
	ghc --make -O2 modelstats
	ghc --make -O2 docstats

clean:
	rm -f train modelstats docstats
	find . -name \*.hi -delete
	find . -name \*.o -delete

