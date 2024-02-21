#![allow(non_camel_case_types)]

mod lexer;

mod parser;

mod cst;
pub mod syntax_kind;

pub use cst::NodeOrToken;
pub use cst::ParseError;
pub use cst::PostgreSQLSyntax;
pub use cst::ResolvedNode;
pub use cst::ResolvedToken;
pub use cst::SyntaxElement;
pub use cst::SyntaxElementRef;
pub use cst::SyntaxNode;
pub use cst::SyntaxToken;

pub fn parse(input: &str) -> Result<ResolvedNode, ParseError> {
    cst::parse(input)
}

#[cfg(test)]
mod tests {
    use crate::lexer::lex;

    use super::*;

    #[test]
    fn test_lex_select() {
        let input = r#"
        WITH regional_sales AS (
            SELECT region, SUM(amount) AS total_sales
            FROM orders
            GROUP BY region
        ), top_regions AS (
            SELECT region
            FROM regional_sales
            WHERE total_sales > (SELECT SUM(total_sales)*1.1/10 FROM regional_sales)
        )
        SELECT region,
               product,
               SUM(quantity) AS product_units,
               SUM(amount) AS product_sales
        FROM orders
        WHERE region IN (SELECT region FROM top_regions)
        GROUP BY region, product;
    "#;
        let tokens = lex(input);
        dbg!(tokens);
    }

    #[test]
    fn test_lex_insert() {
        let input = r#"
        INSERT INTO employees (id, name, position, salary, department_id)
        VALUES
        (1, 'John Doe', 'Software Engineer', 70000, 4),
        (2, 'Jane Smith', 'Project Manager', 85000, 3),
        (3, 'Carlos Gomez', 'Data Analyst', 65000, 2);
        "#;
        let tokens = lex(input);
        dbg!(tokens);
    }

    #[test]
    fn test_lex_delete() {
        let input = r#"
        DELETE FROM employees
        WHERE position = 'Data Analyst' AND salary < 70000;
        "#;
        let tokens = lex(input);
        dbg!(tokens);
    }

    #[test]
    fn test_lex_update() {
        let input = r#"
        UPDATE employees
        SET salary = salary * 1.05
        WHERE department_id = 3;
        "#;
        let tokens = lex(input);
        dbg!(tokens);
    }

    #[test]
    fn test_lex_merge() {
        let input = r#"
        MERGE INTO products p USING stock_movements s ON p.id = s.product_id
        WHEN MATCHED THEN
          UPDATE SET
            p.stock_quantity = p.stock_quantity + s.quantity
        WHEN NOT MATCHED THEN
          INSERT (p.id, p.name, p.price, p.stock_quantity)
          VALUES (s.product_id, s.product_name, s.price, s.quantity);
        "#;
        let tokens = lex(input);
        dbg!(tokens);
    }

    #[test]
    fn test_cst_parser() {
        let input = r#"
            WITH regional_sales AS (
                SELECT region, SUM(amount) AS total_sales
                FROM orders
                GROUP BY region
            ), top_regions AS (
                SELECT region
                FROM regional_sales
                WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
            )
            SELECT region,
                   product,
                   SUM(quantity) AS product_units,
                   SUM(amount) AS product_sales,
                   1*2-3/4 as X
            FROM orders
            WHERE region IN (SELECT region FROM top_regions)
            GROUP BY region, product;
        "#;

        cst::parse(input).unwrap();
    }

    #[test]
    fn test_parse() {
        let input = r#"
            WITH regional_sales AS (
                SELECT region, SUM(amount) AS total_sales
                FROM orders
                GROUP BY region
            ), top_regions AS (
                SELECT region
                FROM regional_sales
                WHERE total_sales > (SELECT SUM(total_sales)/10 FROM regional_sales)
            )
            -- test
            /* test */
            SELECT region,
                   product,
                   SUM(quantity) AS product_units,
                   SUM(amount) AS product_sales,
                   1*2-3/4 as X
            FROM orders
            WHERE region IN (SELECT region FROM top_regions)
            GROUP BY region, product;
        "#;
        let actual = format!("{:#?}", parse(input).unwrap());
        eprintln!("{actual}");
        let expected = r#"Root@0..714
  Whitespace@0..13 "\n            "
  parse_toplevel@13..705
    stmtmulti@13..705
      stmtmulti@13..704
        toplevel_stmt@13..704
          stmt@13..704
            SelectStmt@13..704
              select_no_parens@13..704
                with_clause@13..352
                  WITH@13..17 "WITH"
                  Whitespace@17..18 " "
                  cte_list@18..352
                    cte_list@18..169
                      common_table_expr@18..169
                        name@18..32
                          ColId@18..32
                            IDENT@18..32 "regional_sales"
                        opt_name_list@32..32
                        Whitespace@32..33 " "
                        AS@33..35 "AS"
                        opt_materialized@35..35
                        Whitespace@35..36 " "
                        LParen@36..37 "("
                        Whitespace@37..54 "\n                "
                        PreparableStmt@54..155
                          SelectStmt@54..155
                            select_no_parens@54..155
                              simple_select@54..155
                                SELECT@54..60 "SELECT"
                                opt_all_clause@60..60
                                Whitespace@60..61 " "
                                opt_target_list@61..95
                                  target_list@61..95
                                    target_list@61..67
                                      target_el@61..67
                                        a_expr@61..67
                                          c_expr@61..67
                                            columnref@61..67
                                              ColId@61..67
                                                IDENT@61..67 "region"
                                    Comma@67..68 ","
                                    Whitespace@68..69 " "
                                    target_el@69..95
                                      a_expr@69..80
                                        c_expr@69..80
                                          func_expr@69..80
                                            func_application@69..80
                                              func_name@69..72
                                                type_function_name@69..72
                                                  IDENT@69..72 "SUM"
                                              LParen@72..73 "("
                                              func_arg_list@73..79
                                                func_arg_expr@73..79
                                                  a_expr@73..79
                                                    c_expr@73..79
                                                      columnref@73..79
                                                        ColId@73..79
                                                          IDENT@73..79 "amount"
                                              opt_sort_clause@79..79
                                              RParen@79..80 ")"
                                            within_group_clause@80..80
                                            filter_clause@80..80
                                            over_clause@80..80
                                      Whitespace@80..81 " "
                                      AS@81..83 "AS"
                                      Whitespace@83..84 " "
                                      ColLabel@84..95
                                        IDENT@84..95 "total_sales"
                                into_clause@95..95
                                Whitespace@95..112 "\n                "
                                from_clause@112..123
                                  FROM@112..116 "FROM"
                                  Whitespace@116..117 " "
                                  from_list@117..123
                                    table_ref@117..123
                                      relation_expr@117..123
                                        qualified_name@117..123
                                          ColId@117..123
                                            IDENT@117..123 "orders"
                                      opt_alias_clause@123..123
                                where_clause@123..123
                                Whitespace@123..140 "\n                "
                                group_clause@140..155
                                  GROUP_P@140..145 "GROUP"
                                  Whitespace@145..146 " "
                                  BY@146..148 "BY"
                                  set_quantifier@148..148
                                  Whitespace@148..149 " "
                                  group_by_list@149..155
                                    group_by_item@149..155
                                      a_expr@149..155
                                        c_expr@149..155
                                          columnref@149..155
                                            ColId@149..155
                                              IDENT@149..155 "region"
                                having_clause@155..155
                                window_clause@155..155
                        Whitespace@155..168 "\n            "
                        RParen@168..169 ")"
                        opt_search_clause@169..169
                        opt_cycle_clause@169..169
                    Comma@169..170 ","
                    Whitespace@170..171 " "
                    common_table_expr@171..352
                      name@171..182
                        ColId@171..182
                          IDENT@171..182 "top_regions"
                      opt_name_list@182..182
                      Whitespace@182..183 " "
                      AS@183..185 "AS"
                      opt_materialized@185..185
                      Whitespace@185..186 " "
                      LParen@186..187 "("
                      Whitespace@187..204 "\n                "
                      PreparableStmt@204..338
                        SelectStmt@204..338
                          select_no_parens@204..338
                            simple_select@204..338
                              SELECT@204..210 "SELECT"
                              opt_all_clause@210..210
                              Whitespace@210..211 " "
                              opt_target_list@211..217
                                target_list@211..217
                                  target_el@211..217
                                    a_expr@211..217
                                      c_expr@211..217
                                        columnref@211..217
                                          ColId@211..217
                                            IDENT@211..217 "region"
                              into_clause@217..217
                              Whitespace@217..234 "\n                "
                              from_clause@234..253
                                FROM@234..238 "FROM"
                                Whitespace@238..239 " "
                                from_list@239..253
                                  table_ref@239..253
                                    relation_expr@239..253
                                      qualified_name@239..253
                                        ColId@239..253
                                          IDENT@239..253 "regional_sales"
                                    opt_alias_clause@253..253
                              Whitespace@253..270 "\n                "
                              where_clause@270..338
                                WHERE@270..275 "WHERE"
                                Whitespace@275..276 " "
                                a_expr@276..338
                                  a_expr@276..287
                                    c_expr@276..287
                                      columnref@276..287
                                        ColId@276..287
                                          IDENT@276..287 "total_sales"
                                  Whitespace@287..288 " "
                                  Greater@288..289 ">"
                                  Whitespace@289..290 " "
                                  a_expr@290..338
                                    c_expr@290..338
                                      select_with_parens@290..338
                                        LParen@290..291 "("
                                        select_no_parens@291..337
                                          simple_select@291..337
                                            SELECT@291..297 "SELECT"
                                            opt_all_clause@297..297
                                            Whitespace@297..298 " "
                                            opt_target_list@298..317
                                              target_list@298..317
                                                target_el@298..317
                                                  a_expr@298..317
                                                    a_expr@298..314
                                                      c_expr@298..314
                                                        func_expr@298..314
                                                          func_application@298..314
                                                            func_name@298..301
                                                              type_function_name@298..301
                                                                IDENT@298..301 "SUM"
                                                            LParen@301..302 "("
                                                            func_arg_list@302..313
                                                              func_arg_expr@302..313
                                                                a_expr@302..313
                                                                  c_expr@302..313
                                                                    columnref@302..313
                                                                      ColId@302..313
                                                                        IDENT@302..313 "total_sales"
                                                            opt_sort_clause@313..313
                                                            RParen@313..314 ")"
                                                          within_group_clause@314..314
                                                          filter_clause@314..314
                                                          over_clause@314..314
                                                    Slash@314..315 "/"
                                                    a_expr@315..317
                                                      c_expr@315..317
                                                        AexprConst@315..317
                                                          Iconst@315..317
                                                            ICONST@315..317 "10"
                                            into_clause@317..317
                                            Whitespace@317..318 " "
                                            from_clause@318..337
                                              FROM@318..322 "FROM"
                                              Whitespace@322..323 " "
                                              from_list@323..337
                                                table_ref@323..337
                                                  relation_expr@323..337
                                                    qualified_name@323..337
                                                      ColId@323..337
                                                        IDENT@323..337 "regional_sales"
                                                  opt_alias_clause@337..337
                                            where_clause@337..337
                                            group_clause@337..337
                                            having_clause@337..337
                                            window_clause@337..337
                                        RParen@337..338 ")"
                              group_clause@338..338
                              having_clause@338..338
                              window_clause@338..338
                      Whitespace@338..351 "\n            "
                      RParen@351..352 ")"
                      opt_search_clause@352..352
                      opt_cycle_clause@352..352
                Whitespace@352..365 "\n            "
                SQL_COMMENT@365..372 "-- test"
                Whitespace@372..385 "\n            "
                C_COMMENT@385..395 "/* test */"
                Whitespace@395..408 "\n            "
                select_clause@408..704
                  simple_select@408..704
                    SELECT@408..414 "SELECT"
                    opt_all_clause@414..414
                    Whitespace@414..415 " "
                    opt_target_list@415..582
                      target_list@415..582
                        target_list@415..549
                          target_list@415..500
                            target_list@415..449
                              target_list@415..421
                                target_el@415..421
                                  a_expr@415..421
                                    c_expr@415..421
                                      columnref@415..421
                                        ColId@415..421
                                          IDENT@415..421 "region"
                              Comma@421..422 ","
                              Whitespace@422..442 "\n                   "
                              target_el@442..449
                                a_expr@442..449
                                  c_expr@442..449
                                    columnref@442..449
                                      ColId@442..449
                                        IDENT@442..449 "product"
                            Comma@449..450 ","
                            Whitespace@450..470 "\n                   "
                            target_el@470..500
                              a_expr@470..483
                                c_expr@470..483
                                  func_expr@470..483
                                    func_application@470..483
                                      func_name@470..473
                                        type_function_name@470..473
                                          IDENT@470..473 "SUM"
                                      LParen@473..474 "("
                                      func_arg_list@474..482
                                        func_arg_expr@474..482
                                          a_expr@474..482
                                            c_expr@474..482
                                              columnref@474..482
                                                ColId@474..482
                                                  IDENT@474..482 "quantity"
                                      opt_sort_clause@482..482
                                      RParen@482..483 ")"
                                    within_group_clause@483..483
                                    filter_clause@483..483
                                    over_clause@483..483
                              Whitespace@483..484 " "
                              AS@484..486 "AS"
                              Whitespace@486..487 " "
                              ColLabel@487..500
                                IDENT@487..500 "product_units"
                          Comma@500..501 ","
                          Whitespace@501..521 "\n                   "
                          target_el@521..549
                            a_expr@521..532
                              c_expr@521..532
                                func_expr@521..532
                                  func_application@521..532
                                    func_name@521..524
                                      type_function_name@521..524
                                        IDENT@521..524 "SUM"
                                    LParen@524..525 "("
                                    func_arg_list@525..531
                                      func_arg_expr@525..531
                                        a_expr@525..531
                                          c_expr@525..531
                                            columnref@525..531
                                              ColId@525..531
                                                IDENT@525..531 "amount"
                                    opt_sort_clause@531..531
                                    RParen@531..532 ")"
                                  within_group_clause@532..532
                                  filter_clause@532..532
                                  over_clause@532..532
                            Whitespace@532..533 " "
                            AS@533..535 "AS"
                            Whitespace@535..536 " "
                            ColLabel@536..549
                              IDENT@536..549 "product_sales"
                        Comma@549..550 ","
                        Whitespace@550..570 "\n                   "
                        target_el@570..582
                          a_expr@570..577
                            a_expr@570..573
                              a_expr@570..571
                                c_expr@570..571
                                  AexprConst@570..571
                                    Iconst@570..571
                                      ICONST@570..571 "1"
                              Star@571..572 "*"
                              a_expr@572..573
                                c_expr@572..573
                                  AexprConst@572..573
                                    Iconst@572..573
                                      ICONST@572..573 "2"
                            Minus@573..574 "-"
                            a_expr@574..577
                              a_expr@574..575
                                c_expr@574..575
                                  AexprConst@574..575
                                    Iconst@574..575
                                      ICONST@574..575 "3"
                              Slash@575..576 "/"
                              a_expr@576..577
                                c_expr@576..577
                                  AexprConst@576..577
                                    Iconst@576..577
                                      ICONST@576..577 "4"
                          Whitespace@577..578 " "
                          AS@578..580 "as"
                          Whitespace@580..581 " "
                          ColLabel@581..582
                            IDENT@581..582 "X"
                    into_clause@582..582
                    Whitespace@582..595 "\n            "
                    from_clause@595..606
                      FROM@595..599 "FROM"
                      Whitespace@599..600 " "
                      from_list@600..606
                        table_ref@600..606
                          relation_expr@600..606
                            qualified_name@600..606
                              ColId@600..606
                                IDENT@600..606 "orders"
                          opt_alias_clause@606..606
                    Whitespace@606..619 "\n            "
                    where_clause@619..667
                      WHERE@619..624 "WHERE"
                      Whitespace@624..625 " "
                      a_expr@625..667
                        a_expr@625..631
                          c_expr@625..631
                            columnref@625..631
                              ColId@625..631
                                IDENT@625..631 "region"
                        Whitespace@631..632 " "
                        IN_P@632..634 "IN"
                        Whitespace@634..635 " "
                        in_expr@635..667
                          select_with_parens@635..667
                            LParen@635..636 "("
                            select_no_parens@636..666
                              simple_select@636..666
                                SELECT@636..642 "SELECT"
                                opt_all_clause@642..642
                                Whitespace@642..643 " "
                                opt_target_list@643..649
                                  target_list@643..649
                                    target_el@643..649
                                      a_expr@643..649
                                        c_expr@643..649
                                          columnref@643..649
                                            ColId@643..649
                                              IDENT@643..649 "region"
                                into_clause@649..649
                                Whitespace@649..650 " "
                                from_clause@650..666
                                  FROM@650..654 "FROM"
                                  Whitespace@654..655 " "
                                  from_list@655..666
                                    table_ref@655..666
                                      relation_expr@655..666
                                        qualified_name@655..666
                                          ColId@655..666
                                            IDENT@655..666 "top_regions"
                                      opt_alias_clause@666..666
                                where_clause@666..666
                                group_clause@666..666
                                having_clause@666..666
                                window_clause@666..666
                            RParen@666..667 ")"
                    Whitespace@667..680 "\n            "
                    group_clause@680..704
                      GROUP_P@680..685 "GROUP"
                      Whitespace@685..686 " "
                      BY@686..688 "BY"
                      set_quantifier@688..688
                      Whitespace@688..689 " "
                      group_by_list@689..704
                        group_by_list@689..695
                          group_by_item@689..695
                            a_expr@689..695
                              c_expr@689..695
                                columnref@689..695
                                  ColId@689..695
                                    IDENT@689..695 "region"
                        Comma@695..696 ","
                        Whitespace@696..697 " "
                        group_by_item@697..704
                          a_expr@697..704
                            c_expr@697..704
                              columnref@697..704
                                ColId@697..704
                                  IDENT@697..704 "product"
                    having_clause@704..704
                    window_clause@704..704
      Semicolon@704..705 ";"
      toplevel_stmt@705..705
        stmt@705..705
  Whitespace@705..714 "\n        "
"#;

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_comment_parse() {
        let input = r#"-- a
/* b */
SELECT /*$c*/0 as a, -- d
-- e
b b -- f
,       c -- g
; -- h"#;
        let actual = format!("{:#?}", parse(input).unwrap());
        eprintln!("{actual}");
        let expected = r#"Root@0..74
  SQL_COMMENT@0..4 "-- a"
  Whitespace@4..5 "\n"
  C_COMMENT@5..12 "/* b */"
  Whitespace@12..13 "\n"
  parse_toplevel@13..69
    stmtmulti@13..69
      stmtmulti@13..62
        toplevel_stmt@13..62
          stmt@13..62
            SelectStmt@13..62
              select_no_parens@13..62
                simple_select@13..62
                  SELECT@13..19 "SELECT"
                  opt_all_clause@19..19
                  Whitespace@19..20 " "
                  C_COMMENT@20..26 "/*$c*/"
                  opt_target_list@26..62
                    target_list@26..62
                      target_list@26..47
                        target_list@26..32
                          target_el@26..32
                            a_expr@26..27
                              c_expr@26..27
                                AexprConst@26..27
                                  Iconst@26..27
                                    ICONST@26..27 "0"
                            Whitespace@27..28 " "
                            AS@28..30 "as"
                            Whitespace@30..31 " "
                            ColLabel@31..32
                              IDENT@31..32 "a"
                        Comma@32..33 ","
                        Whitespace@33..34 " "
                        SQL_COMMENT@34..38 "-- d"
                        Whitespace@38..39 "\n"
                        SQL_COMMENT@39..43 "-- e"
                        Whitespace@43..44 "\n"
                        target_el@44..47
                          a_expr@44..45
                            c_expr@44..45
                              columnref@44..45
                                ColId@44..45
                                  IDENT@44..45 "b"
                          Whitespace@45..46 " "
                          BareColLabel@46..47
                            IDENT@46..47 "b"
                      Whitespace@47..48 " "
                      SQL_COMMENT@48..52 "-- f"
                      Whitespace@52..53 "\n"
                      Comma@53..54 ","
                      Whitespace@54..61 "       "
                      target_el@61..62
                        a_expr@61..62
                          c_expr@61..62
                            columnref@61..62
                              ColId@61..62
                                IDENT@61..62 "c"
                  into_clause@62..62
                  from_clause@62..62
                  where_clause@62..62
                  group_clause@62..62
                  having_clause@62..62
                  window_clause@62..62
      Whitespace@62..63 " "
      SQL_COMMENT@63..67 "-- g"
      Whitespace@67..68 "\n"
      Semicolon@68..69 ";"
      toplevel_stmt@69..69
        stmt@69..69
  Whitespace@69..70 " "
  SQL_COMMENT@70..74 "-- h"
"#;

        assert_eq!(actual, expected);
    }

    #[test]
    fn test_comment_without_semicolon_parse() {
        let input = r#"-- a
/* b */
SELECT /*$c*/0 as a, -- d
-- e
b b -- f
,       c -- g
 -- h"#;
        let actual = format!("{:#?}", parse(input).unwrap());
        eprintln!("{actual}");
        let expected = r#"Root@0..73
  SQL_COMMENT@0..4 "-- a"
  Whitespace@4..5 "\n"
  C_COMMENT@5..12 "/* b */"
  Whitespace@12..13 "\n"
  parse_toplevel@13..62
    stmtmulti@13..62
      toplevel_stmt@13..62
        stmt@13..62
          SelectStmt@13..62
            select_no_parens@13..62
              simple_select@13..62
                SELECT@13..19 "SELECT"
                opt_all_clause@19..19
                Whitespace@19..20 " "
                C_COMMENT@20..26 "/*$c*/"
                opt_target_list@26..62
                  target_list@26..62
                    target_list@26..47
                      target_list@26..32
                        target_el@26..32
                          a_expr@26..27
                            c_expr@26..27
                              AexprConst@26..27
                                Iconst@26..27
                                  ICONST@26..27 "0"
                          Whitespace@27..28 " "
                          AS@28..30 "as"
                          Whitespace@30..31 " "
                          ColLabel@31..32
                            IDENT@31..32 "a"
                      Comma@32..33 ","
                      Whitespace@33..34 " "
                      SQL_COMMENT@34..38 "-- d"
                      Whitespace@38..39 "\n"
                      SQL_COMMENT@39..43 "-- e"
                      Whitespace@43..44 "\n"
                      target_el@44..47
                        a_expr@44..45
                          c_expr@44..45
                            columnref@44..45
                              ColId@44..45
                                IDENT@44..45 "b"
                        Whitespace@45..46 " "
                        BareColLabel@46..47
                          IDENT@46..47 "b"
                    Whitespace@47..48 " "
                    SQL_COMMENT@48..52 "-- f"
                    Whitespace@52..53 "\n"
                    Comma@53..54 ","
                    Whitespace@54..61 "       "
                    target_el@61..62
                      a_expr@61..62
                        c_expr@61..62
                          columnref@61..62
                            ColId@61..62
                              IDENT@61..62 "c"
                into_clause@62..62
                from_clause@62..62
                where_clause@62..62
                group_clause@62..62
                having_clause@62..62
                window_clause@62..62
  Whitespace@62..63 " "
  SQL_COMMENT@63..67 "-- g"
  Whitespace@67..69 "\n "
  SQL_COMMENT@69..73 "-- h"
"#;

        assert_eq!(actual, expected);
    }
}
