## ppl build file


VERSION := $(shell grep '^version' ppl.cabal | sed 's|^version:[ ]*||')

##
##help
##	this target

help	:
	@grep ^## Makefile | sed 's|##||'

##
##all
##	make pplc compiler

all	:
	stack install

##
##setup
##	initialize stack: install ghc and libraries

setup	:
	stack setup

##
##ex
##	run all examples in examples dir
##

ex	:
	$(MAKE) -C examples all

##
##dist
##	make all, ex, all pictures and tar archive

dist	:
	$(MAKE) -C src/PPL sdist
	stack sdist
	cp -v .stack-work/dist/*/Cabal-*/ppl-$(VERSION).tar.gz ppl.tar.gz


##
##clean
##	delete all generated files

clean	:
	stack clean
	$(MAKE) -C src/PPL  clean
	$(MAKE) -C examples clean
	rm -rf $(wildcard dist/*)

##
##version
##	set version # in pplc main prog

version	: src/PPL.hs

src/PPL.hs : ppl.cabal
	perl -i~ -p -e 's/version \d+\.\d+\.\d+\.\d+/version $(VERSION)/' $@

.PHONY	: all clean dist ex help version
