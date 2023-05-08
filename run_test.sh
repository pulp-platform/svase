# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Jannis Schönleber <janniss@iis.ee.ethz.ch>

cd build && make && cd ..
svase cheshire_top cheshire_svase.sv test/build/cheshire_top.pickle.sv --split
slang cheshire_svase.sv  -Wrange-oob --allow-use-before-declare -Wrange-width-oob -error-limit=4419 -top cheshire_top__6142509188972423790

