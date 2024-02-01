# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>

all: install_fmt
all: install_slang
all: install_cxxopts


install:
	mkdir -p $@

install_fmt: | install
	git submodule update --init --recursive fmt
	cd fmt && cmake -B build
	cmake --build fmt/build -j$(shell nproc)
	cmake --install fmt/build --prefix install

install_slang: | install
	git submodule update --init --recursive slang
	cd slang && cmake -B build
	cmake --build slang/build -j$(shell nproc) -DSLANG_INCLUDE_TOOLS=OFF -DSLANG_INCLUDE_TESTS=OFF
	cmake --install slang/build --prefix install

install_cxxopts: | install
	git submodule update --init --recursive cxxopts
	cd cxxopts && cmake -B build
	cmake --build cxxopts/build -j$(shell nproc)
	cmake --install cxxopts/build --prefix install
