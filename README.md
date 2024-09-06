# SVase

_SVase_ is a source-to-source pre-elaborator for SystemVerilog (IEEE 1800-2017) single-unit designs.

It is developed as part of the PULP project, a joint effort between ETH Zurich and the University of Bologna.

It leverages the [best-in-class](https://chipsalliance.github.io/sv-tests-results/history) FOSS tool [Slang](https://github.com/MikePopoloski/slang) to parse and compile a design and then rewrite constructs unsupported by fundamental open EDA tools using the provided compile-time information. Most notably, _SVase_

* Creates unique module variants for each parameter combination and then hardcodes their parameters.
* Unrolls generate constructs and replaces instance types to use unique modules.

This completely uncouples instances and modules from each other, fully qualifying hierarchical parameters and types: any resulting unique module can be processed and synthesized out of context. Unlike Slang itself, _SVase_ outputs a fully-compliant 1800-2017 source that may be fed into any next tool with limited SV support as-is.

## Disclaimer
This project is still considered to be in early development; some parts may not yet be functional, and existing interfaces and conventions may be broken without prior notice. We target a formal release in the near future.



## Using _SVase_

Currently there is no way to run select passes or give multiple input files.
The only way to use _SVase_ is shown below:

```bash
# svase top_module output.sv input.sv
svase test out.sv test/assign/assign.sv
```

## 

### Example: SystemVerilog to Verilog flow


A popular tool to convert from SystemVerilog to Verilog is [sv2v](https://github.com/zachjs/sv2v) but it does not support all SystemVerilog constructs as can be seen on the [sv-tests website](https://chipsalliance.github.io/sv-tests-results/).

By adding _SVase_ into the flow, it is possible to get a wider coverage of SystemVerilog constructs.

```sh
svase top_module intermediate.sv input.sv
sv2v --verbose --write output.sv intermediate.sv
```



## Getting _SVase_

### Releases

You can get _SVase_ binaries from the Releases page on Github.
It should work properly on most Linux distros. If it does not, please open an Issue.

### Build it yourself

#### Requirements

The build requirements for _SVase_ are mostly dicated by the [Slang build requirements](https://sv-lang.com/building.html#build-requirements).
The following requirements should be sufficient:

- CMake 3.15
- C++20 compatible compiler (GCC 10, Clang 16, XCode 14.3)
- Python 3

The tested configurations are:
- GCC 11.2, CMake 3.20 and Python 3.6.
- GCC 13.1, CMake 3.30 and Python 3.8.
Depending on where/how you installed the compiler, CMake may not find the correct one. In this case you can manually specify a compiler, `scripts/svase.env` provides and example for this.

#### Build

One-liner:

```bash
make build
```

Step-by-step:
```bash
# edit and source svase.env if necessary
# source scripts/svase.env

# build dependencies
cd deps
make
cd ..

# build SVase
mkdir build && cd build
cmake ..
make
```

If everything went well, you can find _SVase_ at `build/svase`.

#### Running Tests

```bash
make run-tests
```

## 

## Development

### Format to match linter

```bash
make format
```



### V1.0.0 ToDo list

#### User Interface

* [ ] Help message
* [ ] Multiple input files (ie from json)
* [ ] (Optional) Run select passes only

#### Preprocessing

* [ ] Clean up endmodule-trailing

#### Rewrite support

* [ ] Uniquify and replace interfaces
* [ ] Rewrite assignment pattern expressions using `default:`
* [ ] Rewrite starred port expressions
* [ ] Handle instance arrays
* [ ] (Optional) Add optional SV attributes on rewritten syntax
* [ ] (Optional) Preserve/remove comments in a systematic way

#### Postprocessing

* [ ] Add support for library output
* [ ] (Optional) Formatting and cleanup using verible if installed
* [ ] (Optional) output validation using slang and verible if installed

#### Cleanup

* [ ] Switch to available Slang release
* [ ] Use better command line library (`cxxopts` is buggy and lacking)
* [ ] Revise cloning and use `std::move` where appropriate

#### Hardening

* [x] Set up CI with linting and compiling
* [ ] Add feature-oriented test cases (FOSS equivalence checking?)
* [ ] Integrate Cheshire as test
* [ ] (Optional) Integrate Snitch Cluster as test
* [ ] (Optional) Investigate Occamy Top as test



## License

Unless specified otherwise in the respective file headers, all code checked into this repository is made available under a permissive license. All software sources are licensed under Apache 2.0.
