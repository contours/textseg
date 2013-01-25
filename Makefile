HSFLAGS = -O2 -threaded -rtsopts -funbox-strict-fields -optc-O3 -optc-march=native -optc-msse4

all: train modelstats docstats algstats

.PHONY: train modelstats docstats algstats
train:
	ghc --make $(HSFLAGS) train

modelstats:
	ghc --make $(HSFLAGS) modelstats

docstats:
	ghc --make $(HSFLAGS) docstats

algstats:
	ghc --make $(HSFLAGS) algstats

clean:
	rm -f train modelstats docstats algstats
	find . -name \*.hi -delete
	find . -name \*.o -delete

