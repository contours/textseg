HSFLAGS = -O2 -threaded -rtsopts -funbox-strict-fields -optc-O3 -optc-march=native -optc-msse4
TARGETS = train modelstats docstats algstats
HS_FILES = $(wildcard **.hs)

all: $(TARGETS)

%:: %.hs $(filter-out $(addsuffix .hs,$(TARGETS)),$(HS_FILES))
	ghc --make $(HSFLAGS) $<

clean:
	rm -f $(TARGETS)
	find . -name \*.hi -delete
	find . -name \*.o -delete

