# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Paul Scheffler <paulsc@iis.ee.ethz.ch>


# Repository

idma_build_dir = $(build_dir)/idma
idma_rev = 591f9fcf5ec9b5d8bcf1137c44b9f704e059050e

$(idma_build_dir): | $(build_dir)
	rm -rf $@
	git clone https://github.com/pulp-platform/iDMA.git $@
	cd $@ && git checkout $(idma_rev)

# iDMA synthesis wrapper

$(build_dir)/idma_backend_synth.pickle.sv: | $(idma_build_dir)
	bender sources -t synthesis -f -d $(idma_build_dir) | morty -f /dev/stdin -o $@

pickles: $(build_dir)/idma_backend_synth.pickle.sv

$(build_dir)/idma_backend_synth.top.pickle.sv: | $(idma_build_dir)
	bender sources -t synthesis -f -d $(idma_build_dir) | morty -f /dev/stdin --top idma_backend_synth -o $@

bla: $(build_dir)/idma_backend_synth.top.pickle.sv

# iDMA nD synthesis wrapper

$(build_dir)/idma_nd_backend_synth.pickle.sv: | $(idma_build_dir)
	bender sources -t synthesis -f -d $(idma_build_dir) | morty -f /dev/stdin -o $@

pickles: $(build_dir)/idma_nd_backend_synth.pickle.sv
