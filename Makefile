.PHONY: build test compare compile clean remote

PLATFORM := $(shell echo `uname`-`arch`)

build: Build/hsf-$(PLATFORM)

test: compare

Build/hsf-$(PLATFORM): hsf.hs Makefile
	@cd Build || exit 1; \
	test -f hsf-$(PLATFORM).hs || ln -s ../hsf.hs hsf-$(PLATFORM).hs || exit 1; \
	test -f runSfParser.sh || ln -s ../runSfParser.sh runSfParser.sh || exit 1; \
	ghc --version || exit 1 ;\
	ghc -package parsec -o hsf-$(PLATFORM) hsf-$(PLATFORM).hs || exit 1; \
	rm -f hsf || exit 1; ln hsf-$(PLATFORM) hsf || exit 1

compare: build
	@echo comparing output ...
	@Build/hsf-$(PLATFORM) -c -o ../Scratch `pwd`/Test/*.sf

compile: build
	@Build/hsf-$(PLATFORM) -o ../Scratch `pwd`/Test/*.sf

clean:
	@rm -rf Build/*
	@rm -rf Scratch/*

# this target does a build on a remote machine
# eg. for testing a different architecture or compiler version
# you need to define: HSF_REMOTE=USER@HOST:PATH

remote:
	@export UPLOAD_DIR=`echo $(HSF_REMOTE) | sed 's/.*://'` ;\
	export SERVER_ACCOUNT=`echo $(HSF_REMOTE) | sed 's/:.*//'` ;\
	export REMOTE_PLATFORM=$$(ssh $$SERVER_ACCOUNT "echo \`uname\`-\`arch\`") || exit 1 ;\
	echo building for: $$REMOTE_PLATFORM on $$SERVER_ACCOUNT ;\
	ssh $$SERVER_ACCOUNT "mkdir -p $$UPLOAD_DIR" || exit 1 ;\
	rsync -rlptSxzC -e ssh --delete \
		--exclude '.DS*' \
		--exclude '..DS*' \
		../../HaskellSF/Git/ $(HSF_REMOTE) || exit 1 ;\
	ssh $$SERVER_ACCOUNT "make -C $$UPLOAD_DIR build" || exit 1 ;\
	rsync -rlptSxzC -e ssh \
		--exclude '.DS*' \
		--exclude '..DS*' \
		$(HSF_REMOTE)/Build/*$$REMOTE_PLATFORM* ../../HaskellSF/Git/Build || exit 1 ;\
	
