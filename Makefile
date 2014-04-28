.PHONY: test build

build: Build/hsf

Build/hsf: hsf.hs
	@cd Build; \
	test -f hsf.hs || ln -s ../hsf.hs hsf.hs; \
	test -f runSfParser.sh || ln -s ../runSfParser.sh runSfParser.sh; \
	ghc -package parsec -o hsf hsf.hs

test: build
	@Build/hsf -c -o ../Out `pwd`/Test/*.sf

clean:
	@rm -rf Build/*
	@rm -rf Out/*
