.PHONY: test

test: hsfparser
	@./hsfparser `pwd`/Test/*.sf

hsfparser:	hsfparser.hs
	@ghc -package parsec -o hsfparser hsfparser.hs
