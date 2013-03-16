
GHC?=ghc
GHCFLAGS=-fglasgow-exts -hide-package haskell98 -Wall -fno-warn-unused-imports -fno-warn-missing-signatures -fno-warn-orphans -fno-warn-deprecations

PREFIX=~/inst
CONFIGURE_OPTS=--user --prefix $(PREFIX)

#GHCFLAGS+=-O -fexcess-precision -optc-ffast-math -optc-O3 
#crash: -optc-march=pentium4 -optc-mfpmath=sse

GHCCMD = $(GHC) $(GHCFLAGS)

C2HS?=c2hs
TRHSX?=trhsx

PREPROCESSED=$(patsubst %.fhs,%.hs,$(wildcard *.fhs)) \
             $(patsubst %.chs,%.hs,$(wildcard *.chs))
SOURCES=*.hs *.chs *.fhs $(PREPROCESSED)
TARGETS=functortest vobtest fenfire darcs2rdf irc2rdf irc2notetaker latex2png

all: build

build: .setup-config
	runhaskell Setup.hs build

install: 
	runhaskell Setup.hs install

profilable:
	rm -f $(TARGETS)
	$(MAKE) all
	rm -f $(TARGETS)
	$(MAKE) all "GHCFLAGS=-prof -auto-all -hisuf p_hi -osuf p_o $(GHCFLAGS)"
non-profilable:
	rm -f $(TARGETS)
	$(MAKE) all

functortest vobtest fenfire darcs2rdf irc2rdf irc2notetaker latex2png: build

run-functortest: functortest
run-vobtest: vobtest
run-fenfire: ARGS=test.nt
run-fenfire: fenfire
run-darcs2rdf: darcs2rdf
run-irc2rdf: irc2rdf
run-irc2notetaker: irc2notetaker
run-latex2png: latex2png
run-%: %
	./dist/build/$</$< $(ARGS)

run-ghci: build install
	ghci -lraptor -fglasgow-exts Fenfire.hs

run-project: fenfire ../fenfire-project/project.turtle darcs.nt
	./dist/build/fenfire/fenfire ../fenfire-project/project.turtle darcs.nt $(ARGS)

darcs.nt: darcs2rdf _darcs/inventory
	darcs changes --xml | ./dist/build/darcs2rdf/darcs2rdf "http://antti-juhani.kaijanaho.fi/darcs/fenfire-hs/" > darcs.nt

.setup-config:
	runhaskell Setup.hs configure $(CONFIGURE_OPTS)

configure:
	runhaskell Setup.hs configure $(CONFIGURE_OPTS)

clean:
	runhaskell Setup.hs clean

# __attribute__ needs to be a no-op until c2hs learns to parse them in raptor.h
%.hs: %.chs
	$(C2HS) --cppopts '-D"__attribute__(A)= "' $<

%.hs: %.fhs
	echo "-- GENERATED file. Edit the ORIGINAL $< instead." >$@
	$(TRHSX) $< >>$@ || (rm $@ && exit 1)

dump-patches: darcs2rdf
	darcs changes --xml | ./darcs2rdf "http://antti-juhani.kaijanaho.fi/darcs/fenfire-hs/" >> $(ARGS)
