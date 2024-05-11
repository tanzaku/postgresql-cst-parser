#!/bin/bash

set -eux

cd crates/postgresql-lst-parser-wasm
rm -rf ./pkg
wasm-pack build --release --target web
cp pkg/*.js pkg/*.ts pkg/*.wasm ../../docs/js
cd ../../docs
python3 -m http.server 8000
