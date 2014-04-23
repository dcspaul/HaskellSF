.PHONY: test

test: hsfparser
	@./hsfparser Test/*.sf

hsfparser:	hsfparser.hs
	@ghc -package parsec -o hsfparser hsfparser.hs
