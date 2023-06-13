# Testing

## Possible Approach
Include the unprocessed source `test.sv` and the reference-output `test_ref.sv`.

1. run `svase test test_out.sv test.sv`
2. either `diff` the `*.sv` files or `diff` their ast-json representation

For ast-json:
```sh
slang test_ref.sv --ast-json test_ref.json
jq 'walk(if type == "object" then del(.["addr", "symbol"]) else . end)' test_ref.json  > test_ref2.json

slang test_out.sv --ast-json test_out.json
jq 'walk(if type == "object" then del(.["addr", "symbol"]) else . end)' test_out.json  > test_out2.json

diff test_ref.json test_out.json 
```