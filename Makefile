# ------------------------------------------------------------------------------
#  HSF - SmartFrog Core Language Compiler: Makefile
#  Paul Anderson <dcspaul@ed.ac.uk>
# ------------------------------------------------------------------------------}

.PHONY: install remote build test quickcheck compile-tests clean where

# to change these options without editing this file,
# create an "options.mk" or $(HSF_OPTIONS_MK) file 

# versions of Haskell compiler to use for local and remote builds:
VERSION := 76
REMOTE_VERSION := 78

# the "test" target compiles all of the files in the Test
# directory which start with this prefix:
TEST_PREFIX := t1

# extra options for quickcheck & testing
QCFLAGS := -d
TESTFLAGS := -v

# run tests/quickcheck using these external compilers:
TEST_HP := false
TEST_SCALA := true
TEST_OCAML := true

# set these to define explicit pathnames to external compilers
SF_HP_COMPILER := $(SF_HP_COMPILER)
SF_SCALA_COMPILER := $(SF_SCALA_COMPILER)
SF_OCAML_COMPILER := $(SF_OCAML_COMPILER)

# set this to specify the user/host/path for remote builds
# Eg: HSF_REMOTE := USER@HOST:PATH
HSF_REMOTE := 

# put stuff in one of these files to override the above options without editing this file
-include options.mk
-include $(HSF_OPTIONS_MK)

# ------------------------------------------------------------------------------
#  Directories
# ------------------------------------------------------------------------------}

PLATFORM := $(shell echo `uname`-`arch`)
TOP_DIR := $(shell pwd)
SRC_DIR := $(TOP_DIR)/Src
BUILD_DIR := $(TOP_DIR)/Build$(VERSION)/$(PLATFORM)
SCRATCH_DIR := $(TOP_DIR)/Scratch
TEST_DIR := $(TOP_DIR)/Test
BIN_DIR := $(TOP_DIR)/Bin

# ------------------------------------------------------------------------------
# build a binary for the current platform
# install it in the bin directory with a platform/version specific name
# ------------------------------------------------------------------------------

install: $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(BIN_DIR)/runSF.sh
	
$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(BIN_DIR)/hsf: $(BUILD_DIR)/hsf-$(PLATFORM)
	@echo installing hsf$(VERSION)-$(PLATFORM)
	@mkdir -p $(BIN_DIR) || exit 1
	@install -C $(BUILD_DIR)/hsf-$(PLATFORM) $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) || exit 1
	@install -C $(BUILD_DIR)/hsf-$(PLATFORM) $(BIN_DIR)/hsf || exit 1

$(BIN_DIR)/runSF.sh: $(SRC_DIR)/runSF.sh
	@echo installing runSF.sh
	@mkdir -p $(BIN_DIR) || exit 1
	@install -C $(SRC_DIR)/runSF.sh $(BIN_DIR)/runSF.sh || exit 1

# ------------------------------------------------------------------------------
# build a binary for the current platform
# using the given VERSION of the Haskell compiler
# ------------------------------------------------------------------------------

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

# ------------------------------------------------------------------------------
# compile all of the test files
# ------------------------------------------------------------------------------

compile-tests: install
	@echo compiling all tests ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@$(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) -o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf

# ------------------------------------------------------------------------------
# run all of the tests, comparing the output with the external compilers
# ------------------------------------------------------------------------------

test: install
	@echo comparing output ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@$(TEST_SCALA) && echo ">>>>> comparing with scala compiler" || true
	@test -z "$(SF_SCALA_COMPILER)" || export SF_SCALA_COMPILER=$(SF_SCALA_COMPILER) ;\
		$(TEST_SCALA) && $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(TESTFLAGS) -c scala \
		-o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf || true
	@$(TEST_OCAML) && echo ">>>>> comparing with ocaml compiler" || true
	@test -z "$(SF_OCAML_COMPILER)" || export SF_OCAML_COMPILER=$(SF_OCAML_COMPILER) ;\
		$(TEST_OCAML) && $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(TESTFLAGS) -c ocaml \
		-o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf || true
	@$(TEST_HP) && echo ">>>>> comparing with hp compiler" || true
	@test -z "$(SF_HP_COMPILER)" || export SF_HP_COMPILER=$(SF_HP_COMPILER) ;\
		$(TEST_HP) && $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(TESTFLAGS) -c hp \
		-o $(SCRATCH_DIR) $(TEST_DIR)/$(TEST_PREFIX)*.sf || true

# ------------------------------------------------------------------------------
# run quickcheck comparing the output with sfParser
# ------------------------------------------------------------------------------

quickcheck: install
	@echo quickcheck ...
	@mkdir -p $(SCRATCH_DIR) || exit 1
	@$(TEST_SCALA) && echo ">>>>> quickcheck with scala compiler" || true
	@test -z "$(SF_SCALA_COMPILER)" || export SF_SCALA_COMPILER=$(SF_SCALA_COMPILER) ;\
		$(TEST_SCALA) && $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(QCFLAGS) -q scala -o $(SCRATCH_DIR) || true
	@$(TEST_OCAML) && echo ">>>>> quickcheck with ocaml compiler" || true
	@test -z "$(SF_OCAML_COMPILER)" || export SF_OCAML_COMPILER=$(SF_OCAML_COMPILER) ;\
		$(TEST_OCAML) && $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(QCFLAGS) -q ocaml -o $(SCRATCH_DIR) || true
	@$(TEST_HP) && echo ">>>>> quickcheck with hp compiler" || true
	@test -z "$(SF_HP_COMPILER)" || export SF_HP_COMPILER=$(SF_HP_COMPILER) ;\
		$(TEST_HP) && $(BIN_DIR)/hsf$(VERSION)-$(PLATFORM) $(QCFLAGS) -q hp -o $(SCRATCH_DIR) || true

# ------------------------------------------------------------------------------
# build on a remote machine
# eg. for testing a different architecture or compiler version
# use REMOTE_VERSION of the Haskell compiler
# you need to define: HSF_REMOTE=USER@HOST:PATH somewhere before calling this
# ------------------------------------------------------------------------------

remote:
	@test -z "$(HSF_REMOTE)" && \
		echo 'You need to define $$(HSF_REMOTE) to set the user/host/path for remote builds' >&2 && \
		echo 'Eg: create an "options.mk" file containing the following:' >&2 && \
		echo 'HSF_REMOTE := USER@HOST:PATH' >&2 && \
		exit 1 ;\
	export UPLOAD_DIR=`echo $(HSF_REMOTE) | sed 's/.*://'` ;\
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

# ------------------------------------------------------------------------------
# clean out the binaries & the test results
# ------------------------------------------------------------------------------

clean:
	@echo cleaning ...
	@rm -rf $(TOP_DIR)/Build?? $(SCRATCH_DIR)

# ------------------------------------------------------------------------------
# where is everything
# ------------------------------------------------------------------------------

where:
	@echo TOP = $(TOP_DIR)
	@echo SRC = $(SRC_DIR)
	@echo BIN = $(BIN_DIR)
	@echo SCRATCH = $(SCRATCH_DIR)
	@echo BUILD = $(BUILD_DIR)
	@echo TEST = $(TEST_DIR)
	@echo REMOTE = $(HSF_REMOTE)
