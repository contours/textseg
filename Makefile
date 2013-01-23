all: train modelstats docstats

.PHONY: train modelstats docstats
train:
	ghc --make -O2 train

modelstats:
	ghc --make -O2 modelstats

docstats:
	ghc --make -O2 docstats

clean:
	rm -f train modelstats docstats
	find . -name \*.hi -delete
	find . -name \*.o -delete

