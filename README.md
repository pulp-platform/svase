# SVase

_SVase_ is a source-to-source pre-elaborator for SystemVerilog (IEEE 1800-2017) single-unit designs.

It is developed as part of the PULP project, a joint effort between ETH Zurich and the University of Bologna.

It leverages the best-in-class FOSS tool [Slang](https://github.com/MikePopoloski/slang) to parse and compile a design and then rewrite constructs unsupported by fundamental open EDA tools using the provided compile-time information. Most notably, SVase

* Creates unique module variants for each parameter combination and hardcoding their parameters.
* Unrolls generate constructs and replaces instance types to use unique modules.

This completely uncouples instances and modules from each other, fully qualifying hierarchical parameters and types: any resulting unique module can be processed and synthesized out of context. Unlike Slang itself, SVase outputs a fully-compliant 1800-2017 source that may be fed into any next tool with limited SV support as-is.

## Disclaimer
This project is still considered to be in early development; some parts may not yet be functional, and existing interfaces and conventions may be broken without prior notice. We target a formal release in the very near future.

## Current TODOs

### Preprocessing

* [ ] Clean up endmodule-trailing stuff

### Rewrite support

* [ ] Uniquify and replace interfaces
* [ ] Rewrite assignment pattern expressions using `default:`
* [ ] Rewrite starred port expressions (planned in Morty?)
* [ ] Handle instance arrays
* [ ] Add optional SV attributes on rewritten syntax
* [ ] Preserve comments in a systematic way

### Postprocessing

* [ ] Add support for library output
* [ ] (Optional) Formatting and cleanup using verible if installed
* [ ] (Optional) output validation using slang and verible if installed

### Cleanup

* [ ] Use better command line library (`cxxopts` is buggy and lacking)
* [ ] Revise cloning and usev `std::move` where appropriate
* [ ] Switch to next available Slang release

### Hardening

* [ ] Set up CI with linting, compiling, test cases (FOSS equivalence checking?)
* [ ] Integrate Snitch Cluster as test
* [ ] Investigate Occamy Top as test

## Install & Build

Tested with GCC 11.2.0 and cmake 3.20.2

```bash
git clone https://github.com/paulsc96/svase.git
# install dependencies
source scripts/svase.env
cd deps
make
# build svase
source scripts/svase.env
mkdir build && cd build
cmake ..
make
```

## Usage

```bash
svase snitch test2.v test/build/snitch_cluster_wrapper.pickle.sv
```

## Build current tests

```bash
cd test
make
cd ..
./scripts/run_test.sh
```

## Format inplace offline to match linter

```bash
clang-format -style=LLVM -i src/*.cpp include/svase/*.h
```

## License

Unless specified otherwise in the respective file headers, all code checked into this repository is made available under a permissive license. All software sources are licensed under Apache 2.0.