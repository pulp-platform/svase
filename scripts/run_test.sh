# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Jannis Schönleber <janniss@iis.ee.ethz.ch>

cd build && make && cd ..
svase iguana_padframe_fixture iguana_svase.sv iguana_padframe_fixture.pickle.sv --split
slang iguana_svase.sv  -Wrange-oob --allow-use-before-declare -Wrange-width-oob -error-limit=4419 -top iguana_padframe__15538910711671852192
