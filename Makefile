HSFLAGS = -W -O2 -fllvm -threaded -rtsopts -funbox-strict-fields
TARGETS = train modelstats docstats algstats texttiling-experiment topictiling-experiment dp-experiment
HS_FILES := $(patsubst ./%,%,$(shell find . -name \*.hs))

all: $(TARGETS)

%:: %.hs $(filter-out $(addsuffix .hs,$(TARGETS)),$(HS_FILES))
	ghc --make $(HSFLAGS) $<

clean:
	rm -f $(TARGETS)
	find . -name \*.hi -delete
	find . -name \*.o -delete

