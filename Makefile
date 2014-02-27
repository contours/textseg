HSFLAGS = -W -O2 -threaded -rtsopts
TARGETS = run-texttiling
HS_FILES := $(patsubst ./%,%,$(shell find . -name \*.hs))

all: $(TARGETS)

%:: %.hs $(filter-out $(addsuffix .hs,$(TARGETS)),$(HS_FILES))
	ghc --make $(HSFLAGS) $<

clean:
	rm -f $(TARGETS)
	find . -name \*.hi -delete
	find . -name \*.o -delete

