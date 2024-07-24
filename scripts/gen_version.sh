#!/bin/bash
# Copyright (c) 2024 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:
# - Philippe Sauter <phsauter@iis.ee.ethz.ch>

# latest tag
TAG=$(git describe --tags --abbrev=0)

# current commit hash
COMMIT=$(git rev-parse --short HEAD)

printf "#pragma once\n#define VERSION \"SVase: ${TAG} - ${COMMIT}\"\n"
