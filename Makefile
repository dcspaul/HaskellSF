.PHONY: build test compare compile

build: Build/hsf

test: compare

Build/hsf: hsf.hs
	@cd Build; \
	test -f hsf.hs || ln -s ../hsf.hs hsf.hs; \
	test -f runSfParser.sh || ln -s ../runSfParser.sh runSfParser.sh; \
	ghc -package parsec -o hsf hsf.hs

compare: build
	@Build/hsf -c -o ../Scratch `pwd`/Test/*.sf

compile: build
	@Build/hsf -o ../Scratch `pwd`/Test/*.sf

clean:
	@rm -rf Build/*
	@rm -rf Scratch/*
