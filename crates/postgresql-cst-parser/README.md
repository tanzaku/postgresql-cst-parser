# postgresql-cst-parser

[![Crates.io](https://img.shields.io/crates/v/postgresql-cst-parser.svg)](https://crates.io/crates/postgresql-cst-parser)

**Note: This parser is not an official PostgreSQL project but an independent, unofficial tool.**

## Overview

`postgresql-cst-parser` is a PostgreSQL-specific Concrete Syntax Tree (CST) parser developed in Pure Rust. This document describes the parser's features, development motivation, usage, and implementation details.

## Key Features

- **PostgreSQL 17 Support**: Supports the latest PostgreSQL 17 syntax.
- **Structured CST Output**: The generated CST strictly follows the structure defined in PostgreSQL's [gram.y](https://github.com/postgres/postgres/blob/REL_17_0/src/backend/parser/gram.y) file.
- **Utilizing `cstree`**: Uses the `cstree` crate for building syntax trees.
- **PL/pgSQL**: Currently not supported.

## Development Motivation

This project was developed because we needed a library that can be used from Rust, supports all syntax, and (being written in Pure Rust) can be used with wasm-bindgen.

## Usage

You can use it as follows:

```rust
use postgresql_cst_parser::{parse, syntax_kind::SyntaxKind};

fn main() {
    // Parse SQL query and get the syntax tree
    let sql = "SELECT tbl.a as a, tbl.b from TBL tbl WHERE tbl.a > 0;";
    let root = parse(sql).unwrap();

    // Example 1: Extract all column references from the query
    let column_refs: Vec<String> = root
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::columnref)
        .map(|node| node.text().to_string())
        .collect();

    println!("Column references: {:?}", column_refs); // ["tbl.a", "tbl.b", "tbl.a"]

    // Example 2: Find the WHERE condition
    if let Some(where_clause) = root
        .descendants()
        .find(|node| node.kind() == SyntaxKind::where_clause)
    {
        println!("WHERE condition: {}", where_clause.text());
    }

    // Example 3: Get the selected table name
    if let Some(relation_expr) = root
        .descendants()
        .find(|node| node.kind() == SyntaxKind::relation_expr)
    {
        if let Some(name_node) = relation_expr
            .descendants()
            .find(|node| node.kind() == SyntaxKind::ColId)
        {
            println!("Table name: {}", name_node.text());
        }
    }

    // Example 4: Parse complex SQL and extract specific nodes
    let complex_sql = "WITH data AS (SELECT id, value FROM source WHERE value > 10) 
                       SELECT d.id, d.value, COUNT(*) OVER (PARTITION BY d.id) 
                       FROM data d JOIN other o ON d.id = o.id 
                       ORDER BY d.value DESC LIMIT 10;";

    let complex_root = parse(complex_sql).unwrap();

    // Extract CTEs (Common Table Expressions)
    let ctes: Vec<_> = complex_root
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::common_table_expr)
        .collect();

    // Extract window functions
    let window_funcs: Vec<_> = complex_root
        .descendants()
        .filter(|node| node.kind() == SyntaxKind::over_clause)
        .collect();

    println!("Number of CTEs: {}", ctes.len());
    println!("Number of window functions: {}", window_funcs.len());
}
```

Example of the generated syntax tree:

```sql
SELECT tbl.a as a from TBL tbl;
```

```
Root@0..31
  parse_toplevel@0..31
    stmtmulti@0..31
      stmtmulti@0..30
        toplevel_stmt@0..30
          stmt@0..30
            SelectStmt@0..30
              select_no_parens@0..30
                simple_select@0..30
                  SELECT@0..6 "SELECT"
                  Whitespace@6..7 " "
                  opt_target_list@7..17
                    target_list@7..17
                      target_el@7..17
                        a_expr@7..12
                          c_expr@7..12
                            columnref@7..12
                              ColId@7..10
                                IDENT@7..10 "tbl"
                              indirection@10..12
                                indirection_el@10..12
                                  Dot@10..11 "."
                                  attr_name@11..12
                                    ColLabel@11..12
                                      IDENT@11..12 "a"
                        Whitespace@12..13 " "
                        AS@13..15 "as"
                        Whitespace@15..16 " "
                        ColLabel@16..17
                          IDENT@16..17 "a"
                  Whitespace@17..18 " "
                  from_clause@18..30
                    FROM@18..22 "from"
                    Whitespace@22..23 " "
                    from_list@23..30
                      table_ref@23..30
                        relation_expr@23..26
                          qualified_name@23..26
                            ColId@23..26
                              IDENT@23..26 "TBL"
                        Whitespace@26..27 " "
                        opt_alias_clause@27..30
                          alias_clause@27..30
                            ColId@27..30
                              IDENT@27..30 "tbl"
      Semicolon@30..31 ";"
```

If you'd like to try this parser directly, you can experience it online [here](https://tanzaku.github.io/postgresql-cst-parser/).

## Implementation

This implementation uses PostgreSQL's [scan.l](https://github.com/postgres/postgres/blob/REL_17_0/src/backend/parser/scan.l) and [gram.y](https://github.com/postgres/postgres/blob/REL_17_0/src/backend/parser/gram.y) with patches from [libpg_query](https://github.com/pganalyze/libpg_query/tree/17-6.0.0/patches) applied. `scan.l` has been further rewritten for Rust, and based on `scan.l` and `gram.y`, a syntax parsing table has been created to build the parser.

## License

- `kwlist.h`, `parser.c`, `scan.l`, `gram.y` are under the PostgreSQL License.
- `lexer_ported.rs` and `generated.rs` contain code ported from PostgreSQL, so the ported parts are under the PostgreSQL License.
- This project applies patches from [libpg_query](https://github.com/pganalyze/libpg_query) to `scan.l` and `gram.y`, but the patches themselves are not included in this repository.
- Other files are published under the MIT License.