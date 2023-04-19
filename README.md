# SVase

_SVase_ is a source-to-source pre-elaborator for SystemVerilog (IEEE 1800-2017) single-unit designs.

It leverages the best-in-class FOSS tool [Slang](https://github.com/MikePopoloski/slang) to parse and compile a design and then rewrite constructs unsupported by fundamental open EDA tools using the provided compile-time information. Most notably, SVase

* Creates unique module variants for each parameter combination and hardcoding their parameters.
* Unrolls generate constructs and replaces instance types to use unique modules.

This completely uncouples instances and modules from each other, fully qualifying hierarchical parameters and types: any resulting unique module can be processed and synthesized out of context. Unlike Slang itself, SVase outputs a fully-compliant 1800-2017 source that may be fed into any next tool with limited SV support as-is.

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
* [ ] Integrate Cheshire SoC as test
* [ ] Investigate Occamy Top as test

## Install & Build

```bash
git clone https://github.com/paulsc96/svase.git
# install dependencies
source svase.env
cd deps
make
# build svase
source svase.env
mkdir build && cd build
cmake ..
make
```

## Usage

```bash
svase snitch test2.v test/build/snitch_cluster_wrapper.pickle.sv
```


## Format inplace offline to match linter

```bash
clang-format -style=LLVM -i src/*.cpp include/svase/*.h
```