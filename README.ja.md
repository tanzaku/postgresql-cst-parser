# postgresql-cst-parser

## 概要

`postgresql-cst-parser`は、Pure Rust で開発された PostgreSQL 専用の Concrete Syntax Tree（CST）パーサーです。この文書では、パーサーの特徴、動機、使用方法、および実装の詳細について説明します。

## 主な特徴

- **自動生成された CST パーサー**: PostgreSQL の文法から自動的に生成されるため、幅広い文法に対応。
- **部分的な制限**: スキャナーの一部の実装が不完全であるため、全ての文法に対応しているわけではありません。

## 開発の動機

1. Rust から利用でき、広範な文法をサポートする PostgreSQL の CST パーサーが不足している。
2. [pg_query.rs](https://github.com/pganalyze/pg_query.rs)はとても素晴らしいライブラリだが、CST は構築できず WebAssembly（wasm）でビルドできない。

## 使用方法

以下のコード例のようにして使用します。

```rust
let resolved_root = postgresql_cst_parser::parse("SELECT 1;");
dbg!(resolved_root);
```

## 実装方法

実装には、PostgreSQL の [scan.l](https://github.com/postgres/postgres/blob/REL_16_STABLE/src/backend/parser/scan.l) と [gram.y](https://github.com/postgres/postgres/blob/REL_16_STABLE/src/backend/parser/gram.y) に対する [libpg_query の patch](https://github.com/pganalyze/libpg_query/tree/16-latest/patches)を適用したものを使用しています。`scan.l` は Rust 用に書き換えられ、`scan.l` と `gram.y` を基にして構文解析表を作成し、パーサーを構築しています。

## ライセンス

`kwlist.h`, `scan.l`, `gram.y` は PostgreSQL License です。
その他のファイルは MIT License の下で公開されています。
