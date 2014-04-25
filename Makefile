.PHONY: test build

build: hsfparser

test: build
	@./hsfparser `pwd`/Test/*.sf

hsfparser:	hsfparser.hs
	@ghc -package parsec -o hsfparser hsfparser.hs
