.PHONY: test

test: hsfparser
	@./hsfparser `pwd`/Test/*.sf >Test/RESULTS+.txt
	
	
	
	
	
	@if cmp -s Test/RESULTS+.txt Test/RESULTS.txt ; then \
		echo "** Test OK" ; \
		rm Test/Results+.txt ; \
	else \
		echo "** Test Failed" ; \
		diff Test/RESULTS.txt Test/RESULTS+.txt ; \
	fi

hsfparser:	hsfparser.hs
	@ghc -package parsec -o hsfparser hsfparser.hs
