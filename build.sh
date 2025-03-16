#!/bin/bash

set -eux

cargo run -p lexer-generator --release
cargo run -p parser-generator --release
# cargo test --package postgresql-cst-parser --test test -- test_all --exact --show-output
cargo test --package postgresql-cst-parser
