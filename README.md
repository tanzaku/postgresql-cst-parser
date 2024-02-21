# postgresql-cst-parser

## Overview

The `postgresql-cst-parser` is a PostgreSQL-specific Concrete Syntax Tree (CST) parser developed in Pure Rust. This document describes the parser's features, motivation, usage, and details of its implementation.

## Key Features

- **Automatically Generated CST Parser**: Automatically generated from PostgreSQL grammar, allowing it to support a wide range of syntaxes.
- **Partial Limitations**: Due to some incomplete implementations in the scanner, it does not support all grammatical structures.

## Motivation for Development

1. There is a lack of PostgreSQL CST (Concrete Syntax Tree) parsers that can be utilized from Rust and support a wide range of syntax.
1. [pg_query.rs](https://github.com/pganalyze/pg_query.rs) is an excellent library, however, it does not construct CSTs and cannot be built for WebAssembly (wasm).

## Usage

Use it as shown in the following code examples.

```rust
let resolved_root = postgresql_cst_parser::parse("SELECT 1;");
dbg!(resolved_root);
```

If you would like to experience this parser in action, you can try it out directly online [here](https://tanzaku.github.io/postgresql-cst-parser/). Enter your own code to see how the parser operates in real-time.


## Implementation

The implementation uses a modified version of PostgreSQL's [scan.l](https://github.com/postgres/postgres/blob/REL_16_STABLE/src/backend/parser/scan.l) and [gram.y](https://github.com/postgres/postgres/blob/REL_16_STABLE/src/backend/parser/gram.y) based on patches from [libpg_query](https://github.com/pganalyze/libpg_query/tree/16-latest/patches). `scan.l` has been rewritten for Rust, and a syntax parsing table has been created based on `scan.l` and `gram.y` to construct the parser.

## License

`kwlist.h`, `scan.l`, `gram.y` are under the PostgreSQL License.
Other files are published under the MIT License.
