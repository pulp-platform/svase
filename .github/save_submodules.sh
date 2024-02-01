#!/bin/bash
# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Philippe Sauter <phsauter@ethz.ch>

# This exists to make Githubs zip-download actually somewhat usable
# The hashes of the used submodule commits are not stored in .gitmodule
# but rather in the git-tree itself.
# This appends the commit to each submodule in .gitmodule

target=".github/.submodules_generated"
cat /dev/null > $target

regexp="[[:space:]]*path[[:space:]]*=[[:space:]]*(.*)"

# Read .gitmodules lines
cat .gitmodules | while IFS= read -r line; do
	echo "$line" >> $target
    # search for the 'path' keys (each submodule must have one)
    if [[ $line =~ $regexp ]]; then
        submodule_path=${BASH_REMATCH[1]}
        submodule_hash=$(git ls-tree HEAD "$submodule_path" | awk '{print $3}')
        # Update the submodule section with the commit hash
        echo -e "\thash = $submodule_hash" >> $target
    fi
done
