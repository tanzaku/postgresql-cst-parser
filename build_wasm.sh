#!/bin/bash

set -eux

cd crates/postgresql-cst-parser-wasm
wasm-pack build --release --target web
cp pkg/*.js pkg/*.ts pkg/*.wasm dist/js
cd dist
python3 -m http.server 8000
