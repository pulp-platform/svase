cd build && make && cd ..
svase cheshire_top cheshire_svase.sv test/build/cheshire_top.pickle.sv
slang cheshire_svase.sv  -Wrange-oob --allow-use-before-declare -Wrange-width-oob -error-limit=4419 -top cheshire_top__6142509188972423790

