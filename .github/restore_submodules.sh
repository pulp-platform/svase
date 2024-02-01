#!/bin/bash
# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0

# Author:  Philippe Sauter <phsauter@ethz.ch>

# This exists to make Githubs zip-download actually somewhat usable
# It restores the submodules in a fresh git-tree

input=".github/.submodules_generated"

path=""
hash=""
url=""

cat $input | while IFS= read -r line; do
  if [[ "$line" =~ ^\[submodule\ (.*)\] ]]; then
    # new submodule section, clear info from previous
    submodule_name="${BASH_REMATCH[1]}"
    path=""
    hash=""
    url=""
  elif [[ "$line" =~ "path ="\ (.*) ]]; then
    path="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ "hash ="\ (.*) ]]; then
    hash="${BASH_REMATCH[1]}"
  elif [[ "$line" =~ "url ="\ (.*) ]]; then
    url="${BASH_REMATCH[1]}"
  fi

  # Do we have all details?
  if [[ -n "$path" && -n "$hash" && -n "$url" ]]; then
    echo "Restoring submodule $submodule_name"
    rm -rf "$path"
    git submodule add "$url" "$path"
    git -C "$path" checkout "$hash"
    path=""
    hash=""
    url=""
  fi
done
