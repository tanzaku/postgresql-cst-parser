#!/bin/bash

set -eux

cargo run -p lexer-generator --release
cargo run -p parser-generator --release
cargo test --package postgresql-cst-parser --lib -- tests::test_parse --exact --nocapture
