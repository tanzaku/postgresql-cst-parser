# postgresql-cst-parser

## 概要

`postgresql-cst-parser`は、Pure Rustで開発されたPostgreSQL専用の具象構文木（CST）パーサーです。このドキュメントでは、パーサーの機能、開発のモチベーション、使用方法、および実装の詳細について説明します。

## 主な特徴

- **PostgreSQL 17対応**: 最新のPostgreSQL 17の構文をサポートしています。
- **構造化されたCST出力**: 生成されるCSTは、PostgreSQLの[gram.y](https://github.com/postgres/postgres/blob/REL_17_0/src/backend/parser/gram.y)ファイルで定義された構造に厳密に従います。
- **`cstree`の利用**: 構文木の構築に`cstree`クレートを使用しています。
- **PL/pgSQL**: 現在はサポートされていません。

## 開発のモチベーション

Rustから使用可能で、すべての構文をサポートし、(Pure Rust で書かれており) wasm-bindgen が利用可能なライブラリが必要だったため開発しました。  

## 使用方法

以下のように使用することができます：

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

生成される構文木の例:

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

このパーサーを実際に体験してみたい場合は、[こちら](https://tanzaku.github.io/postgresql-cst-parser/)で直接試すことができます。

## 実装

この実装は、PostgreSQLの[scan.l](https://github.com/postgres/postgres/blob/REL_17_0/src/backend/parser/scan.l)と[gram.y](https://github.com/postgres/postgres/blob/REL_17_0/src/backend/parser/gram.y)に対して[libpg_query](https://github.com/pganalyze/libpg_query/tree/17-6.0.0/patches)のパッチを適用したファイルを使用しています。`scan.l`はさらに Rust 用に書き直したうえで、`scan.l`と`gram.y`に基づいて構文解析テーブルを作成し、パーサーを構築しています。

## ライセンス

- `kwlist.h`、`parser.c`、`scan.l`、`gram.y`はPostgreSQLライセンスの下にあります。
- `lexer_ported.rs`と`generated.rs`はPostgreSQLから移植されたコードを含むため、移植部分はPostgreSQLライセンスの下にあります。
- このプロジェクトでは、`scan.l`、`gram.y`に対して[libpg_query](https://github.com/pganalyze/libpg_query)のパッチを当てていますが、パッチそのものはこのリポジトリには含まれていません。
- その他のファイルはMITライセンスの下で公開されています。
