.PHONY: build test compare compile clean

PLATFORM := $(shell echo `uname`-`arch`)

build: Build/hsf-$(PLATFORM)

test: compare

Build/hsf-$(PLATFORM): hsf.hs
	@cd Build; \
	test -f hsf-$(PLATFORM).hs || ln -s ../hsf.hs hsf-$(PLATFORM).hs; \
	test -f runSfParser.sh || ln -s ../runSfParser.sh runSfParser.sh; \
	ghc -package parsec -o hsf-$(PLATFORM) hsf-$(PLATFORM).hs; \
	rm -f hsf; ln hsf-$(PLATFORM) hsf

compare: build
	@Build/hsf -c -o ../Scratch `pwd`/Test/*.sf

compile: build
	@Build/hsf -o ../Scratch `pwd`/Test/*.sf

clean:
	@rm -rf Build/*
	@rm -rf Scratch/*
