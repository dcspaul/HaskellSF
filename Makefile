.PHONY: install remote build test quickcheck compile-tests clean where

PLATFORM := $(shell echo `uname`-`arch`)
VERSION := 76
REMOTE_VERSION := 78
TEST_PREFIX := t1-error
QCFLAGS := -d

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
	@mkdir -p $(BIN_DIR) || exit 1
	@install -C $(BUILD_DIR)/hsf-$(PLATFORM) $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) || exit 1
	@install -C $(BUILD_DIR)/hsf-$(PLATFORM) $(BIN_DIR)/hsf || exit 1
	@install -C $(SRC_DIR)/runSF.sh $(BIN_DIR)/runSF.sh || exit 1

# build a binary for the current platform
# using the given VERSION of the Haskell compiler

build: $(BUILD_DIR)/hsf-$(PLATFORM)

$(BUILD_DIR)/hsf-$(PLATFORM): \
		$(BUILD_DIR)/hsf.hs \
		$(BUILD_DIR)/HSF/Parser.hs \
		$(BUILD_DIR)/HSF/Store.hs \
		$(BUILD_DIR)/HSF/Eval.hs \
		$(BUILD_DIR)/HSF/Utils.hs \
		$(BUILD_DIR)/HSF/Errors.hs \
		$(BUILD_DIR)/HSF/Options.hs \
		$(BUILD_DIR)/HSF/Test/Invent.hs \
		$(BUILD_DIR)/HSF/Test/QuickCheck.hs \
		$(BUILD_DIR)/HSF/Test/Frequencies.hs \
		$(BUILD_DIR)/HSF/Test/RunScalaVersion.hs \
		$(BUILD_DIR)/HSF/Test/RunOCamlVersion.hs \
		$(BUILD_DIR)/HSF/Test/RunHPVersion.hs \
		Makefile
	@cd $(BUILD_DIR) || exit 1; \
	export PATH=/opt/ghc$(VERSION)/bin:$$PATH || exit 1 ;\
	ghc --version || exit 1 ;\
	ghc -o hsf-$(PLATFORM) --make hsf.hs || exit 1

$(BUILD_DIR)/hsf.hs: $(SRC_DIR)/hsf.hs Makefile
	@mkdir -p $(BUILD_DIR) || exit
	@cp $(SRC_DIR)/hsf.hs $(BUILD_DIR)/hsf.hs || exit 1

$(BUILD_DIR)/HSF/%.hs: $(SRC_DIR)/HSF/%.hs Makefile
	@mkdir -p $(BUILD_DIR)/HSF $(BUILD_DIR)/HSF/Test || exit 1
	@rm -f $@ || exit 1
	@cp $< $@ 

# this target compiles all of the test files

compile-tests: install
	@echo compiling all tests ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf

# this target runs all of the tests, comparing the output with the external compilers
# you need to define SFPARSER & CSF (unless the external compilers are in your PATH)

test: install
	@echo comparing output ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	#@echo ">>>>> comparing with scala compiler"
	#@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -c scala -o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf
	#@echo ">>>>> comparing with ocaml compiler"
	#@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -c ocaml -o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf
	@echo ">>>>> comparing with hp compiler"
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -c hp -o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf

# this target runs quickcheck comparing the output with sfParser
# you need to define SFPARSER & CSF (unless the external compilers are in your PATH)

quickcheck: install
	@echo quickcheck ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@echo ">>>>> quickcheck with scala compiler"
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(QCFLAGS) -q scala -o $(SCRATCH_DIR)
	@echo ">>>>> quickcheck with ocaml compiler"
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(QCFLAGS) -q ocaml -o $(SCRATCH_DIR)
	@echo ">>>>> quickcheck with hp compiler"
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(QCFLAGS) -q hp -o $(SCRATCH_DIR)

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
	install -C $(TOP_DIR)/Build$(REMOTE_VERSION)/$$REMOTE_PLATFORM/hsf-$$REMOTE_PLATFORM \
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
