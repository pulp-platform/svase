# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Philippe Sauter <phsauter@student.ethz.ch>

BUILD_DIR ?= build
BUILD_TYPE ?= Debug
DEPS := deps/fmt/build deps/slang/build deps/cxxopts/build

## build svase in debug mode (default)
all: debug

## calls build in release mode
release: $(DEPS)
	@$(MAKE) build BUILD_TYPE=Release

## calls build with debug mode
debug: $(DEPS)
	@$(MAKE) build BUILD_TYPE=Debug

build: $(DEPS)
	@rm -rf $(BUILD_DIR)
	@mkdir -p $(BUILD_DIR)
	cmake -S . -B $(BUILD_DIR) -DCMAKE_BUILD_TYPE=$(BUILD_TYPE)
	cmake --build $(BUILD_DIR)

## install each dependency
deps/%/build: .git
	git submodule update --init deps/$*
	@echo "Installing $*..."
	@$(MAKE) -C deps/ install_$*

# if downloaded as zip, the submodules need to be restored
.git:
	@if [ ! -d ".git" ]; then \
		git init; \
		.github/restore_submodules.sh; \
	fi

## format code to match linter
format:
	clang-format -style=LLVM -i src/*.cpp include/svase/*.h

## remove svase-build and dependencies
clean:
	@rm -rf build
	@rm -rf $(DEPS)
	#rm -rf deps/install


.PHONY: all release debug build format clean help
help: Makefile
	@printf "Available targets:\n\n"
	@awk '/^[a-zA-Z\-_0-9]+:/ { \
		helpMessage = match(lastLine, /^## (.*)/); \
		if (helpMessage) { \
			helpCommand = substr($$1, 0, index($$1, ":")-1); \
			helpMessage = substr(lastLine, RSTART + 3, RLENGTH); \
			printf "%-15s %s\n", helpCommand, helpMessage; \
		} \
	} \
	{ lastLine = $$0 }' $(MAKEFILE_LIST)
