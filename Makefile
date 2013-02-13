
GHC?=ghc
GHCFLAGS=-fglasgow-exts -hide-package haskell98 -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-deprecations

#GHCFLAGS+=-O -fexcess-precision -optc-ffast-math -optc-O3 
#crash: -optc-march=pentium4 -optc-mfpmath=sse

GHCCMD = $(GHC) $(GHCFLAGS)

C2HS?=c2hs
TRHSX?=trhsx

PREPROCESSED=$(patsubst %.fhs,%.hs,$(wildcard *.fhs)) \
             $(patsubst %.chs,%.hs,$(wildcard *.chs))
SOURCES=*.hs *.chs *.fhs $(PREPROCESSED)
TARGETS=functortest vobtest fenfire darcs2rdf

all: build

build:
	runghc Setup.hs build

profilable:
	rm -f $(TARGETS)
	$(MAKE) all
	rm -f $(TARGETS)
	$(MAKE) all "GHCFLAGS=-prof -auto-all -hisuf p_hi -osuf p_o $(GHCFLAGS)"
non-profilable:
	rm -f $(TARGETS)
	$(MAKE) all

functortest vobtest fenfire darcs2rdf: build

run-functortest: functortest
run-vobtest: vobtest
run-fenfire: ARGS=test.nt
run-fenfire: fenfire
run-darcs2rdf: darcs2rdf
run-%: %
	./dist/build/$</$< $(ARGS)

run-project: fenfire ../fenfire-project/project.nt darcs.nt
	./dist/build/fenfire/fenfire ../fenfire-project/project.nt darcs.nt $(ARGS)

darcs.nt: darcs2rdf _darcs/inventory
	darcs changes --xml | ./dist/build/darcs2rdf/darcs2rdf "http://antti-juhani.kaijanaho.fi/darcs/fenfire-hs/" > darcs.nt

clean:
	rm -f $(PREPROCESSED) *.p_hi *.hi *.i *.chi Raptor.h Raptor_stub.* *.p_o *.o $(TARGETS)

# __attribute__ needs to be a no-op until c2hs learns to parse them in raptor.h
%.hs: %.chs
	$(C2HS) --cppopts '-D"__attribute__(A)= "' $<

%.hs: %.fhs
	echo "-- GENERATED file. Edit the ORIGINAL $< instead." >$@
	$(TRHSX) $< >>$@ || (rm $@ && exit 1)

dump-patches: darcs2rdf
	darcs changes --xml | ./darcs2rdf "http://antti-juhani.kaijanaho.fi/darcs/fenfire-hs/" >> $(ARGS)
