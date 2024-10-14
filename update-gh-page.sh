#!/bin/bash

set -eux

cd demo
npm run copy-wasm
npm run update-gh-page
