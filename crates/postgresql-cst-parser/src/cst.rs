use std::collections::{HashMap, HashSet};

use cstree::{
    build::GreenNodeBuilder, green::GreenNode, interning::Resolver, RawSyntaxKind, Syntax,
};
use miniz_oxide::inflate::decompress_to_vec;

use crate::{
    lexer::{lex, TokenKind},
    parser::{
        end_rule_id, end_rule_kind, num_non_terminal_symbol, num_terminal_symbol,
        rule_name_to_component_id, token_kind_to_component_id, Action, ACTION_TABLE, GOTO_TABLE,
        RULES,
    },
};

use super::{lexer::Token, syntax_kind::SyntaxKind};

struct Node {
    token: Option<Token>,
    component_id: u32,
    children: Vec<Node>,
    start_byte_pos: usize,
    end_byte_pos: usize,
}

pub type PostgreSQLSyntax = SyntaxKind;

impl From<SyntaxKind> for cstree::RawSyntaxKind {
    fn from(kind: SyntaxKind) -> Self {
        Self(kind as u32)
    }
}

pub type SyntaxNode = cstree::syntax::SyntaxNode<PostgreSQLSyntax>;
pub type ResolvedNode = cstree::syntax::ResolvedNode<PostgreSQLSyntax>;
pub type ResolvedToken = cstree::syntax::ResolvedToken<PostgreSQLSyntax>;
#[allow(unused)]
pub type SyntaxToken = cstree::syntax::SyntaxToken<PostgreSQLSyntax>;
#[allow(unused)]
pub type SyntaxElement = cstree::util::NodeOrToken<SyntaxNode, SyntaxToken>;
pub type SyntaxElementRef<'a> = cstree::util::NodeOrToken<&'a SyntaxNode, &'a SyntaxToken>;
pub type NodeOrToken<'a> = cstree::util::NodeOrToken<&'a ResolvedNode, &'a ResolvedToken>;

struct Parser {
    builder: GreenNodeBuilder<'static, 'static, PostgreSQLSyntax>,
}

#[derive(Debug)]
pub struct ParseError {
    pub message: String,
    pub start_byte_pos: usize,
    pub end_byte_pos: usize,
}

impl Parser {
    fn parse_rec(
        &mut self,
        node: &Node,
        peekable: &mut std::iter::Peekable<std::vec::IntoIter<(SyntaxKind, usize, usize, &str)>>,
        complement_node_or_token: &HashSet<usize>,
    ) {
        if cfg!(feature = "remove-empty-node") {
            if node.start_byte_pos == node.end_byte_pos
                && !complement_node_or_token.contains(&node.start_byte_pos)
            {
                return;
            }
        }

        while let Some((kind, start, _, text)) = peekable.peek() {
            // TODO: Consider whether the presence or absence of an equals sign changes the position of comments. Determine which option is preferable
            if *start >= node.start_byte_pos {
                // if *start > node.start_byte_pos {
                break;
            }
            self.builder.token(*kind, text);
            peekable.next();
        }

        let kind: SyntaxKind = SyntaxKind::from_raw(RawSyntaxKind(node.component_id));
        if let Some(token) = &node.token {
            self.builder.token(kind, &token.value);
        } else {
            self.builder.start_node(kind);
            node.children
                .iter()
                .for_each(|c| self.parse_rec(c, peekable, complement_node_or_token));
            self.builder.finish_node();
        }
    }

    fn parse(
        mut self,
        nodes: &Vec<&Node>,
        extras: Vec<(SyntaxKind, usize, usize, &str)>,
        complement_node_or_token: &HashSet<usize>,
    ) -> (GreenNode, impl Resolver) {
        let mut peekable = extras.into_iter().peekable();

        self.builder.start_node(SyntaxKind::Root);

        for node in nodes {
            self.parse_rec(node, &mut peekable, complement_node_or_token);
        }

        while let Some((kind, _, _, text)) = peekable.peek() {
            self.builder.token(*kind, text);
            peekable.next();
        }

        self.builder.finish_node();

        let (tree, cache) = self.builder.finish();
        (tree, cache.unwrap().into_interner().unwrap())
    }
}

/// The logic for converting tokens in PostgreSQL's parser.c
/// ref: https://github.com/postgres/postgres/blob/REL_16_STABLE/src/backend/parser/parser.c#L195
fn init_tokens(tokens: &mut [Token]) {
    fn next_token_index(tokens: &[Token], i: usize) -> Option<usize> {
        for (j, token) in tokens.iter().enumerate().skip(i + 1) {
            match token.kind {
                TokenKind::C_COMMENT | TokenKind::SQL_COMMENT => continue,
                _ => return Some(j),
            }
        }
        None
    }

    for i in 0..tokens.len() - 1 {
        match &tokens[i].kind {
            TokenKind::KEYWORD(k) if k == "FORMAT" => {
                if let Some(j) = next_token_index(tokens, i) {
                    if tokens[j].kind == TokenKind::KEYWORD("JSON".to_string()) {
                        tokens[i].kind = TokenKind::KEYWORD("FORMAT_LA".to_string());
                    }
                }
            }
            TokenKind::KEYWORD(k) if k == "NOT" => {
                if let Some(j) = next_token_index(tokens, i) {
                    match &tokens[j].kind {
                        TokenKind::KEYWORD(k)
                            if matches!(
                                k.as_str(),
                                "BETWEEN" | "IN_P" | "LIKE" | "ILIKE" | "SIMILAR"
                            ) =>
                        {
                            tokens[i].kind = TokenKind::KEYWORD("NOT_LA".to_string());
                        }
                        _ => {}
                    }
                }
            }
            TokenKind::KEYWORD(k) if k == "NULLS_P" => {
                if let Some(j) = next_token_index(tokens, i) {
                    match &tokens[j].kind {
                        TokenKind::KEYWORD(k) if matches!(k.as_str(), "FIRST_P" | "LAST_P") => {
                            tokens[i].kind = TokenKind::KEYWORD("NULLS_LA".to_string());
                        }
                        _ => {}
                    }
                }
            }
            TokenKind::KEYWORD(k) if k == "WITH" => {
                if let Some(j) = next_token_index(tokens, i) {
                    match &tokens[j].kind {
                        TokenKind::KEYWORD(k) if matches!(k.as_str(), "TIME" | "ORDINALITY") => {
                            tokens[i].kind = TokenKind::KEYWORD("WITH_LA".to_string());
                        }
                        _ => {}
                    }
                }
            }
            TokenKind::KEYWORD(k) if k == "WITHOUT" => {
                if let Some(j) = next_token_index(tokens, i) {
                    match &tokens[j].kind {
                        TokenKind::KEYWORD(k) if matches!(k.as_str(), "TIME") => {
                            tokens[i].kind = TokenKind::KEYWORD("WITHOUT_LA".to_string());
                        }
                        _ => {}
                    }
                }
            }
            _ => (),
        }
    }
}

#[inline]
fn is_replacement_value_comment(s: impl AsRef<str>) -> bool {
    let s = s.as_ref();
    s.starts_with("/*#") && s.ends_with("*/") && !s.contains('\n')
}

#[inline]
fn is_missing_from_replacement_value(
    stack: &[(u32, Node)],
    extras: &[(SyntaxKind, usize, usize, &str)],
    action_table: &[i16],
    state: u32,
) -> bool {
    match extras.last() {
        Some((_, _, _, s))
            if is_replacement_value_comment(s)
                && stack.last().unwrap().1.component_id == SyntaxKind::FROM as u32 =>
        {
            let action_index =
                (state * num_terminal_symbol()) as usize + SyntaxKind::IDENT as usize;

            let a = action_table[action_index];
            a != 0x7FFF
        }
        _ => false,
    }
}

#[inline]
fn is_bind_variable_comment(s: impl AsRef<str>) -> bool {
    let s = s.as_ref();
    s.starts_with("/*")
        && s.ends_with("*/")
        && !s.contains('\n')
        && !matches!(s.chars().nth(2).unwrap(), '$' | '#')
}

#[inline]
fn is_missing_bind_variable(
    extras: &[(SyntaxKind, usize, usize, &str)],
    action_table: &[i16],
    state: u32,
) -> bool {
    match extras.last() {
        Some((_, _, _, s)) if is_bind_variable_comment(s) => {
            let action_index =
                (state * num_terminal_symbol()) as usize + SyntaxKind::SCONST as usize;

            let a = action_table[action_index];
            a != 0x7FFF
        }
        _ => false,
    }
}

// #[inline]
// fn decide_action(
//     extras: &[(SyntaxKind, usize, usize, &str)],
//     action_table: &[i16],
//     state: u32,
//     cid: u32,
// ) -> Action {
//     let action = match action_table[(state * num_terminal_symbol() + cid) as usize] {
//         0x7FFF => Action::Error,
//         v if v > 0 => Action::Shift((v - 1) as usize),
//         v if v < 0 => Action::Reduce((-v - 1) as usize),
//         _ => Action::Accept,
//     };

//     if action == Action::Error {
//         if is_missing_bind_variable(extras, action_table, state) {
//             let next_state = action_table
//                 [(state * num_terminal_symbol()) as usize + SyntaxKind::SCONST as usize];
//             return Action::Shift(next_state as usize - 1);
//         }
//     }

//     action
// }

/// Parsing a string as PostgreSQL syntax and converting it into a ResolvedNode
pub fn parse(input: &str) -> Result<ResolvedNode, ParseError> {
    let mut tokens = lex(input);

    init_tokens(&mut tokens);

    tokens.push(Token {
        kind: end_rule_kind(),
        value: "".to_string(),
        start_byte_pos: input.len(),
        end_byte_pos: input.len(),
    });

    let action_table_u8 = decompress_to_vec(ACTION_TABLE.as_ref()).unwrap();
    let goto_table_u8 = decompress_to_vec(GOTO_TABLE.as_ref()).unwrap();
    let action_table = unsafe { action_table_u8.align_to::<i16>().1 };
    let goto_table = unsafe { goto_table_u8.align_to::<i16>().1 };

    let mut stack: Vec<(u32, Node)> = Vec::new();
    let mut tokens: std::iter::Peekable<std::vec::IntoIter<Token>> = tokens.into_iter().peekable();

    stack.push((
        0,
        Node {
            token: None,
            component_id: end_rule_id(),
            children: Vec::new(),
            start_byte_pos: 0,
            end_byte_pos: 0,
        },
    ));

    let mut last_pos = 0;
    let mut extras: Vec<(SyntaxKind, usize, usize, &str)> = Vec::new();
    let mut complement_node_or_token = HashSet::new();

    loop {
        let state = stack.last().unwrap().0;
        let mut token = match tokens.peek() {
            Some(token) => token.clone(),
            None => {
                return Err(ParseError {
                    message: "unexpected end of input".to_string(),
                    start_byte_pos: input.len(),
                    end_byte_pos: input.len(),
                });
            }
        };

        let mut cid = token_kind_to_component_id(&token.kind);

        if matches!(token.kind, TokenKind::C_COMMENT | TokenKind::SQL_COMMENT) {
            if last_pos < token.start_byte_pos {
                extras.push((
                    SyntaxKind::Whitespace,
                    last_pos,
                    token.start_byte_pos,
                    &input[last_pos..token.start_byte_pos],
                ));
            }

            last_pos = token.end_byte_pos;

            let kind = SyntaxKind::from_raw(RawSyntaxKind(cid));
            extras.push((
                kind,
                token.start_byte_pos,
                token.end_byte_pos,
                &input[token.start_byte_pos..token.end_byte_pos],
            ));
            tokens.next();

            continue;
        }

        let mut insert_dummy_token = false;
        let mut action = match action_table[(state * num_terminal_symbol() + cid) as usize] {
            0x7FFF => Action::Error,
            v if v > 0 => Action::Shift((v - 1) as usize),
            v if v < 0 => Action::Reduce((-v - 1) as usize),
            _ => Action::Accept,
        };

        if Some(token.start_byte_pos) != extras.last().map(|e| e.2) {
            if is_missing_from_replacement_value(&stack, &extras, action_table, state) {
                let last_extra = extras.last().unwrap();
                token = Token {
                    start_byte_pos: last_extra.2,
                    end_byte_pos: last_extra.2,
                    kind: TokenKind::IDENT, // とりあえず識別子としておく
                    value: String::new(),
                };
                cid = SyntaxKind::IDENT as u32;
                complement_node_or_token.insert(token.start_byte_pos);

                action = match action_table[(state * num_terminal_symbol() + cid) as usize] {
                    0x7FFF => Action::Error,
                    v if v > 0 => Action::Shift((v - 1) as usize),
                    v if v < 0 => Action::Reduce((-v - 1) as usize),
                    _ => Action::Accept,
                };
                insert_dummy_token = true;
            }

            if is_missing_bind_variable(&extras, action_table, state) {
                let last_extra = extras.last().unwrap();
                token = Token {
                    start_byte_pos: last_extra.2,
                    end_byte_pos: last_extra.2,
                    kind: TokenKind::SCONST, // とりあえず文字列としておく
                    value: String::new(),
                };
                cid = SyntaxKind::SCONST as u32;
                complement_node_or_token.insert(token.start_byte_pos);

                action = match action_table[(state * num_terminal_symbol() + cid) as usize] {
                    0x7FFF => Action::Error,
                    v if v > 0 => Action::Shift((v - 1) as usize),
                    v if v < 0 => Action::Reduce((-v - 1) as usize),
                    _ => Action::Accept,
                };
                insert_dummy_token = true;
            }
        }

        match action {
            Action::Shift(next_state) => {
                let node = Node {
                    token: Some(token.clone()),
                    component_id: cid,
                    children: Vec::new(),
                    start_byte_pos: token.start_byte_pos,
                    end_byte_pos: token.end_byte_pos,
                };

                if last_pos < token.start_byte_pos {
                    extras.push((
                        SyntaxKind::Whitespace,
                        last_pos,
                        token.start_byte_pos,
                        &input[last_pos..token.start_byte_pos],
                    ));
                }

                last_pos = token.end_byte_pos;

                stack.push((next_state as u32, node));
                if !insert_dummy_token {
                    tokens.next();
                }
            }
            Action::Reduce(rule_index) => {
                let rule = &RULES[rule_index];

                let mut children = Vec::new();
                for _ in 0..rule.len {
                    children.push(stack.pop().unwrap().1);
                }
                children.reverse();

                let reduced_component_id = rule_name_to_component_id(rule.name);

                let start_byte_pos =
                    children
                        .first()
                        .map(|t| t.start_byte_pos)
                        .unwrap_or_else(|| {
                            // Adopt the larger of the end position of the previous token or the end of the space.
                            extras
                                .last()
                                .map(|e| e.2)
                                .unwrap_or_default()
                                .max(stack.last().unwrap().1.end_byte_pos)
                        });

                let end_byte_pos = children
                    .last()
                    .map(|t| t.end_byte_pos)
                    .unwrap_or(start_byte_pos);

                let node = Node {
                    token: None,
                    component_id: reduced_component_id + num_terminal_symbol(),
                    children,
                    start_byte_pos,
                    end_byte_pos,
                };

                let next_state = stack.last().unwrap().0;
                let goto = goto_table
                    [(next_state * num_non_terminal_symbol() + reduced_component_id) as usize];

                match goto {
                    next_state if next_state >= 0 => {
                        stack.push((next_state as u32, node));
                    }
                    _ => {
                        return Err(ParseError {
                            message: format!(
                                "syntax error at byte position {}",
                                token.start_byte_pos
                            ),
                            start_byte_pos: token.start_byte_pos,
                            end_byte_pos: token.end_byte_pos,
                        });
                    }
                }
            }
            Action::Accept => {
                break;
            }
            Action::Error => {
                return Err(ParseError {
                    message: format!(
                        "Action::Error: syntax error at byte position {}",
                        token.start_byte_pos
                    ),
                    start_byte_pos: token.start_byte_pos,
                    end_byte_pos: token.end_byte_pos,
                });
            }
        }
    }

    while let Some(token) = tokens.next() {
        if last_pos < token.start_byte_pos {
            extras.push((
                SyntaxKind::Whitespace,
                last_pos,
                token.start_byte_pos,
                &input[last_pos..token.start_byte_pos],
            ));
        }

        last_pos = token.end_byte_pos;

        // 最後のトークンは$endなので、ここで終了
        if tokens.peek().is_none() {
            break;
        }

        let cid = token_kind_to_component_id(&token.kind);
        let kind = SyntaxKind::from_raw(RawSyntaxKind(cid));
        extras.push((
            kind,
            token.start_byte_pos,
            token.end_byte_pos,
            &input[token.start_byte_pos..token.end_byte_pos],
        ));
    }

    let parser = Parser {
        builder: GreenNodeBuilder::new(),
    };
    let root: Vec<&Node> = stack[1..].iter().map(|s| &s.1).collect();
    let (ast, resolver) = parser.parse(&root, extras, &complement_node_or_token);

    Ok(SyntaxNode::new_root_with_resolver(ast, resolver))
}

#[cfg(test)]
mod tests {
    use super::parse;

    #[test]
    fn test_missing_sample_value() {
        let s = r#"
select
    /*hoge*/ as hoge
,   /*fuga*/
,   /*fuga*/ * 1
,   /*fuga*/ || 'hoge'
from
    /*#tbl*/ t
where
    /*val*/ = 1;
"#;

        let expected = r#"
Root@0..117
  parse_toplevel@0..117
    stmtmulti@0..117
      stmtmulti@0..116
        toplevel_stmt@0..116
          stmt@0..116
            SelectStmt@0..116
              select_no_parens@0..116
                simple_select@0..116
                  SELECT@0..6 "select"
                  Whitespace@6..11 "\n    "
                  C_COMMENT@11..19 "/*hoge*/"
                  opt_all_clause@19..19
                  opt_target_list@19..74
                    target_list@19..74
                      target_list@19..57
                        target_list@19..40
                          target_list@19..27
                            target_el@19..27
                              a_expr@19..19
                                c_expr@19..19
                                  AexprConst@19..19
                                    Sconst@19..19
                                      SCONST@19..19 ""
                              Whitespace@19..20 " "
                              AS@20..22 "as"
                              Whitespace@22..23 " "
                              ColLabel@23..27
                                IDENT@23..27 "hoge"
                          Whitespace@27..28 "\n"
                          Comma@28..29 ","
                          Whitespace@29..32 "   "
                          C_COMMENT@32..40 "/*fuga*/"
                          target_el@40..40
                            a_expr@40..40
                              c_expr@40..40
                                AexprConst@40..40
                                  Sconst@40..40
                                    SCONST@40..40 ""
                        Whitespace@40..41 "\n"
                        Comma@41..42 ","
                        Whitespace@42..45 "   "
                        C_COMMENT@45..53 "/*fuga*/"
                        target_el@53..57
                          a_expr@53..57
                            a_expr@53..53
                              c_expr@53..53
                                AexprConst@53..53
                                  Sconst@53..53
                                    SCONST@53..53 ""
                            Whitespace@53..54 " "
                            Star@54..55 "*"
                            Whitespace@55..56 " "
                            a_expr@56..57
                              c_expr@56..57
                                AexprConst@56..57
                                  Iconst@56..57
                                    ICONST@56..57 "1"
                      Whitespace@57..58 "\n"
                      Comma@58..59 ","
                      Whitespace@59..62 "   "
                      C_COMMENT@62..70 "/*fuga*/"
                      target_el@70..74
                        a_expr@70..74
                          a_expr@70..70
                            c_expr@70..70
                              AexprConst@70..70
                                Sconst@70..70
                                  SCONST@70..70 ""
                          Whitespace@70..71 " "
                          qual_Op@71..73
                            Op@71..73 "||"
                          Whitespace@73..74 " "
                          a_expr@74..74
                            c_expr@74..74
                              AexprConst@74..74
                                Sconst@74..74
                                  SCONST@74..74 ""
                  Whitespace@74..75 "\n"
                  from_clause@75..94
                    FROM@75..79 "from"
                    Whitespace@79..84 "\n    "
                    C_COMMENT@84..92 "/*#tbl*/"
                    from_list@92..94
                      table_ref@92..94
                        relation_expr@92..92
                          qualified_name@92..92
                            ColId@92..92
                              IDENT@92..92 ""
                        Whitespace@92..93 " "
                        opt_alias_clause@93..94
                          alias_clause@93..94
                            ColId@93..94
                              IDENT@93..94 "t"
                  Whitespace@94..95 "\n"
                  where_clause@95..116
                    WHERE@95..100 "where"
                    Whitespace@100..105 "\n    "
                    C_COMMENT@105..112 "/*val*/"
                    a_expr@112..116
                      a_expr@112..112
                        c_expr@112..112
                          AexprConst@112..112
                            Sconst@112..112
                              SCONST@112..112 ""
                      Whitespace@112..113 " "
                      Equals@113..114 "="
                      Whitespace@114..115 " "
                      a_expr@115..116
                        c_expr@115..116
                          AexprConst@115..116
                            Iconst@115..116
                              ICONST@115..116 "1"
      Semicolon@116..117 ";"
 */"#;

        eprintln!("sql:{s}");
        dbg!(parse(s).unwrap());
    }
}
