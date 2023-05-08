# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>

build_dir ?= build

all: pickles

pickles: test_deps

test_deps:
	cargo install bender morty
	pip install mako hjson jsonref jsonschema

$(build_dir):
	mkdir -p $@

# include snitch.mk
# include idma.mk
# include cva6.mk
include cheshire.mk