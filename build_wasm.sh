#!/bin/bash

set -eux

cd demo
npm run copy-wasm
npm run build
cd ../docs
python3 -m http.server 8000
