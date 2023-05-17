cd build && make && cd ..
svase iguana_padframe_fixture iguana_svase.sv iguana_padframe_fixture.pickle.sv --split
slang iguana_svase.sv  -Wrange-oob --allow-use-before-declare -Wrange-width-oob -error-limit=4419 -top iguana_padframe__15538910711671852192
