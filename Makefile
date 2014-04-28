.PHONY: test build

build: Build/hsf

test: build
	@Build/hsf -c -o ../Out `pwd`/Test/*.sf

Build/hsf: hsf.hs
	@cd Build; \
	test -f hsf.hs || ln -s ../hsf.hs hsf.hs; \
	ghc -package parsec -o hsf hsf.hs
