.PHONY: test build test2 build2

build: hsfparser

test: build
	@./hsfparser -o ../Out `pwd`/Test/*.sf

hsfparser: hsfparser.hs
	@ghc -package parsec -o hsfparser hsfparser.hs

test2: build2
	./hsfparser2 -vch some-dir hello world
	
build2:	hsfparser2.hs
	@ghc -package parsec -o hsfparser2 hsfparser2.hs || exit 1

