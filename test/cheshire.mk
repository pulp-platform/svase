# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Jannis Schönleber <janniss@iis.ee.ethz.ch>

cheshire_build_dir = $(build_dir)/cheshire
cheshire_rev = 9b6c99a749cbba857aabe41fec10830ef0c07b56

$(cheshire_build_dir): | $(build_dir)
	rm -rf $@
	git clone https://github.com/pulp-platform/cheshire.git $@
	cd $@ && git checkout $(cheshire_rev)
	cd $@ && git submodule update --init --recursive
	cd $@ && make all
	cd $@ && make slang-check-cheshire

$(build_dir)/cheshire_top.pickle.sv: | $(cheshire_build_dir)
	cp $(cheshire_build_dir)/build/cheshire_top.pickle.sv $@

pickles: $(build_dir)/cheshire_top.pickle.sv