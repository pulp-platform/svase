build_dir ?= build

all: pickles

pickles: test_deps

test_deps:
	cargo install bender morty
	pip install mako hjson jsonref jsonschema

$(build_dir):
	mkdir -p $@

include snitch.mk
include idma.mk
