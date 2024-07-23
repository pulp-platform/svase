#!/bin/bash

# latest tag
TAG=$(git describe --tags --abbrev=0)

# current commit hash
COMMIT=$(git rev-parse --short HEAD)

printf "#pragma once\n#define VERSION \"SVase: ${TAG} - ${COMMIT}\"\n"
