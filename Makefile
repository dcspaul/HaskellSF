.PHONY: build remote test compile-tests clean

PLATFORM := $(shell echo `uname`-`arch`)
VERSION := 76
REMOTE_VERSION := 78

# this target builds the binary for the current platform
# using the given VERSION of the Haskell compiler

build: Build$(VERSION)/hsf-$(PLATFORM)

Build$(VERSION)/hsf-$(PLATFORM): \
		Build$(VERSION)/hsf-$(PLATFORM).hs \
		Build$(VERSION)/runSfParser.sh  \
		Makefile
	@cd Build$(VERSION) || exit 1; \
	export PATH=/opt/ghc$(VERSION)/bin:$$PATH || exit 1 ;\
	ghc --version || exit 1 ;\
	ghc -o hsf-$(PLATFORM) hsf-$(PLATFORM).hs || exit 1; \
	rm -f hsf || exit 1; ln hsf-$(PLATFORM) hsf || exit 1

Build$(VERSION)/hsf-$(PLATFORM).hs: Src/hsf.hs Makefile
	@mkdir -p Build$(VERSION) || exit 1
	@if test "$(VERSION)" = "78" ; then \
		grep -v 'import Data.List.Split' Src/hsf.hs >Build$(VERSION)/hsf-$(PLATFORM).hs || exit 1 ;\
	else \
		cp Src/hsf.hs Build$(VERSION)/hsf-$(PLATFORM).hs || exit 1 ;\
	fi

Build$(VERSION)/runSfParser.sh: Src/runSfParser.sh Makefile
	@mkdir -p Build$(VERSION) || exit 1
	@cp Src/runSfParser.sh Build$(VERSION)/runSfParser.sh || exit 1

# this target compiles all of the test files

compile-tests: build
	@echo compiling all tests ...
	@mkdir -p Scratch || exit 1
	@Build$(VERSION)/hsf-$(PLATFORM) -o ../Scratch `pwd`/Test/*.sf

# this target runs all of the tests, comparing the output with sfParser
# you need to define: SFPARSER=location-of-sfparser (unless it is in your PATH)

test: build
	@echo comparing output ...
	@mkdir -p Scratch || exit 1
	@Build$(VERSION)/hsf-$(PLATFORM) -c -o ../Scratch `pwd`/Test/*.sf

# this target does a build on a remote machine
# using the given REMOTE_VERSION of the Haskell compiler
# eg. for testing a different architecture or compiler version
# you need to define: HSF_REMOTE=USER@HOST:PATH

remote:
	@export UPLOAD_DIR=`echo $(HSF_REMOTE) | sed 's/.*://'` ;\
	export SERVER_ACCOUNT=`echo $(HSF_REMOTE) | sed 's/:.*//'` ;\
	export REMOTE_PLATFORM=$$(ssh $$SERVER_ACCOUNT "echo \`uname\`-\`arch\`") || exit 1 ;\
	echo building version $(REMOTE_VERSION) for $$REMOTE_PLATFORM on $$SERVER_ACCOUNT ;\
	ssh $$SERVER_ACCOUNT "mkdir -p $$UPLOAD_DIR" || exit 1 ;\
	echo uploading ... ;\
	rsync -rlptSxzC -e ssh --delete \
		--exclude '.DS*' \
		--exclude '..DS*' \
		../../HaskellSF/Git/ $(HSF_REMOTE) || exit 1 ;\
	echo building ... ;\
	ssh $$SERVER_ACCOUNT "make -C $$UPLOAD_DIR VERSION=$(REMOTE_VERSION)" || exit 1 ;\
	echo downloading result ... ;\
	rsync -rlptSxzC -e ssh \
		--exclude '.DS*' \
		--exclude '..DS*' \
		$(HSF_REMOTE)/Build$(REMOTE_VERSION)/* ../../HaskellSF/Git/Build$(REMOTE_VERSION) || exit 1 ;\
	cd Build$(REMOTE_VERSION) || exit 1; \
	rm -f hsf || exit 1; \
	test -f hsf-$(PLATFORM) && ln hsf-$(PLATFORM) hsf || exit 0

# clean out the binaries & the test results

clean:
	@rm -rf Build??/*
	@rm -rf Scratch/*
