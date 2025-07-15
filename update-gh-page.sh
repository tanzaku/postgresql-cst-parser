#!/bin/bash

set -eux

cd demo
npm ci
npm run copy-wasm
npm run update-gh-page
