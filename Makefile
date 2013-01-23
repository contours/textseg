HSFLAGS = -O2 -threaded -rtsopts

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

