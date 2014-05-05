.PHONY: build build-76 build-78 test compile clean remote

PLATFORM := $(shell echo `uname`-`arch`)

build: build-76

test: build
	@echo comparing output ...
	@Build/hsf-$(PLATFORM) -c -o ../Scratch `pwd`/Test/*.sf

compile: build
	@echo compiling all tests ...
	@Build/hsf-$(PLATFORM) -o ../Scratch `pwd`/Test/*.sf

clean:
	@rm -rf Build??/*
	@rm -rf Scratch/*

# this target does a build using the old version of the Haskell compiler (7.6)

build-76: Build76/hsf-$(PLATFORM)

Build76/hsf-$(PLATFORM): hsf.hs Makefile
	@cd Build76 || exit 1; \
	rm -f hsf-$(PLATFORM).hs || exit 1 ;\
	cp ../hsf.hs hsf-$(PLATFORM).hs || exit 1 ;\
	rm -f runSfParser.sh || exit 1 ;\
	cp ../runSfParser.sh runSfParser.sh || exit 1 ;\
	export PATH=/opt/ghc76/bin:$$PATH || exit 1 ;\
	ghc --version || exit 1 ;\
	ghc -o hsf-$(PLATFORM) hsf-$(PLATFORM).hs || exit 1; \
	rm -f hsf || exit 1; ln hsf-$(PLATFORM) hsf || exit 1

# this target does a build using the new version of the Haskell compiler (7.8)

build-78: Build78/hsf-$(PLATFORM)

Build78/hsf-$(PLATFORM): hsf.hs Makefile
	@cd Build78 || exit 1; \
	rm -f hsf-$(PLATFORM).hs || exit 1 ;\
	grep -v 'import Data.List.Split' ../hsf.hs >hsf-$(PLATFORM).hs || exit 1 ;\
	rm -f runSfParser.sh || exit 1 ;\
	cp ../runSfParser.sh runSfParser.sh || exit 1 ;\
	export PATH=/opt/ghc78/bin:$$PATH ;\
	ghc --version || exit 1 ;\
	ghc -o hsf-$(PLATFORM) hsf-$(PLATFORM).hs || exit 1; \
	rm -f hsf || exit 1; ln hsf-$(PLATFORM) hsf || exit 1

# this target does a build on a remote machine
# eg. for testing a different architecture or compiler version
# you need to define: HSF_REMOTE=USER@HOST:PATH

REMOTE_VERSION=78

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
	ssh $$SERVER_ACCOUNT "make -C $$UPLOAD_DIR build-$(REMOTE_VERSION)" || exit 1 ;\
	echo downloading result ... ;\
	rsync -rlptSxzC -e ssh \
		--exclude '.DS*' \
		--exclude '..DS*' \
		$(HSF_REMOTE)/Build$(REMOTE_VERSION)/* ../../HaskellSF/Git/Build$(REMOTE_VERSION) || exit 1 ;\
	cd Build$(REMOTE_VERSION) || exit 1; \
	rm -f hsf || exit 1; test -f hsf-$(PLATFORM) && ln hsf-$(PLATFORM) hsf
