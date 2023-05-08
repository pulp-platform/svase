# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Jannis Schönleber <janniss@iis.ee.ethz.ch>


cva6_build_dir = $(build_dir)/cva6
cva6_rev = fbfc417b9285ab9ec40470ce5269802b1dcc96a6

$(cva6_build_dir): | $(build_dir)
	rm -rf $@
	git clone https://github.com/pulp-platform/cva6.git $@
	cd $@ && git checkout $(cva6_rev)
	cd $@ && git submodule update --init --recursive

$(build_dir)/cva6.pickle.sv: | $(cva6_build_dir)
	bender sources -f -d $(cva6_build_dir) -t cv64a6_imafdc_sv39 -t synthesis | morty -f /dev/stdin -D VERILATOR=1 -o $@

$(build_dir)/cva6_top.pickle.sv: | $(cva6_build_dir)
	bender sources -f -d $(cva6_build_dir) -t cv64a6_imafdc_sv39 -t synthesis | morty -f /dev/stdin -D VERILATOR=1 -o $@ --top cva6

pickles: $(build_dir)/cva6.pickle.sv $(build_dir)/cva6_top.pickle.sv
