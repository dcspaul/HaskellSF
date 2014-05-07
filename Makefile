.PHONY: install remote build test compile-tests clean

PLATFORM := $(shell echo `uname`-`arch`)
VERSION := 76
REMOTE_VERSION := 78

TOP_DIR := $(shell pwd)
SRC_DIR := $(TOP_DIR)/Src
BUILD_DIR := $(TOP_DIR)/Build$(VERSION)/$(PLATFORM)
SCRATCH_DIR := $(TOP_DIR)/Scratch
TEST_DIR := $(TOP_DIR)/Test
BIN_DIR := $(TOP_DIR)/Bin

# build a binary for the current platform
# put it in the bin directory with a platform/version specific name

install: $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM)
	
$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(BIN_DIR)/hsf: $(BUILD_DIR)/hsf-$(PLATFORM)
	@echo installing hsf$(VERSION)-$(PLATFORM)
	@mkdir -p $(BIN_DIR)
	@install $(BUILD_DIR)/hsf-$(PLATFORM) $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM)
	@rm -f $(BIN_DIR)/hsf
	@ln $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(BIN_DIR)/hsf

# build a binary for the current platform
# using the given VERSION of the Haskell compiler

build: $(BUILD_DIR)/hsf-$(PLATFORM)

$(BUILD_DIR)/hsf-$(PLATFORM): \
		$(BUILD_DIR)/hsf.hs \
		$(BUILD_DIR)/HSF/Include.hs \
		$(BUILD_DIR)/runSfParser.sh \
		Makefile
	@cd $(BUILD_DIR) || exit 1; \
	export PATH=/opt/ghc$(VERSION)/bin:$$PATH || exit 1 ;\
	ghc --version || exit 1 ;\
	ghc -o hsf-$(PLATFORM) --make hsf.hs || exit 1

$(BUILD_DIR)/hsf.hs: $(SRC_DIR)/hsf.hs Makefile
	@mkdir -p $(BUILD_DIR) || exit
	@if test "$(VERSION)" = "78" ; then \
		grep -v 'import Data.List.Split' $(SRC_DIR)/hsf.hs >$(BUILD_DIR)/hsf.hs || exit 1 ;\
	else \
		cp $(SRC_DIR)/hsf.hs $(BUILD_DIR)/hsf.hs || exit 1 ;\
	fi

$(BUILD_DIR)/HSF/%.hs: $(SRC_DIR)/HSF/%.hs Makefile
	@mkdir -p $(BUILD_DIR)/HSF || exit 1
	@rm -f $@ || exit 1
	@cp $< $@ 

$(BUILD_DIR)/runSfParser.sh: $(SRC_DIR)/runSfParser.sh Makefile
	@mkdir -p $(BUILD_DIR) || exit
	@cp $(SRC_DIR)/runSfParser.sh $(BUILD_DIR)/runSfParser.sh || exit 1

# this target compiles all of the test files

compile-tests: build
	@echo compiling all tests ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -o $(SCRATCH_DIR) $(TEST_DIR)/*.sf

# this target runs all of the tests, comparing the output with sfParser
# you need to define: SFPARSER=location-of-sfparser (unless it is in your PATH)

test: build
	@echo comparing output ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -c -o $(SCRATCH_DIR) $(TEST_DIR)/*.sf

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
		$(TOP_DIR)/ $(HSF_REMOTE) || exit 1 ;\
	echo building ... ;\
	ssh $$SERVER_ACCOUNT "make -C $$UPLOAD_DIR VERSION=$(REMOTE_VERSION) build" || exit 1 ;\
	echo downloading result ... ;\
	rsync -rlptSxzC -e ssh \
		--exclude '.DS*' \
		--exclude '..DS*' \
		$(HSF_REMOTE)/Build$(REMOTE_VERSION)/* $(TOP_DIR)/Build$(REMOTE_VERSION) || exit 1 ;\
	echo installing hsf$(REMOTE_VERSION)-$$REMOTE_PLATFORM ;\
	mkdir -p $(BIN_DIR) ;\
	install $(TOP_DIR)/Build$(REMOTE_VERSION)/$$REMOTE_PLATFORM/hsf-$$REMOTE_PLATFORM \
		$(BIN_DIR)/hsf$(REMOTE_VERSION)-$$REMOTE_PLATFORM

# clean out the binaries & the test results

clean:
	@echo cleaning ...
	@rm -rf $(TOP_DIR)/Build?? $(SCRATCH_DIR)

# where is everything

where:
	@echo TOP = $(TOP_DIR)
	@echo SRC = $(SRC_DIR)
	@echo BIN = $(BIN_DIR)
	@echo SCRATCH = $(SCRATCH_DIR)
	@echo BUILD = $(BUILD_DIR)
	@echo TEST = $(TEST_DIR)
