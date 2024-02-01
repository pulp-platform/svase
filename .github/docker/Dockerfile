# Copyright (c) 2022 ETH Zurich and University of Bologna.
# Licensed under the Apache License, Version 2.0, see LICENSE for details.
# SPDX-License-Identifier: Apache-2.0
#
# Authors:
# - Philippe Sauter <phsauter@ethz.ch>
#
# syntax=docker/dockerfile:1
# slang requires very recent libboost and clang versions so doing it on alpine is fast
# also we want to link against musl, see https://build-your-own.org/blog/20221229_alpine/
ARG BASE_IMG=alpine:3.19

FROM ${BASE_IMG} AS builder

# install packages
COPY packages.txt /packages.txt
RUN apk add --no-cache $(cat /packages.txt)

RUN ln -sf /usr/bin/clang++-16 /usr/bin/c++ \
    && c++ --version

# should not be necessary for github-workflows build but is for local
RUN git config --global --add safe.directory /svase

WORKDIR /svase
