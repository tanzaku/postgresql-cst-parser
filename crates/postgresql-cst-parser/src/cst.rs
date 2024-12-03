use std::collections::HashSet;

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
        complement_token: &HashSet<usize>,
    ) {
        if cfg!(feature = "remove-empty-node") {
            if node.start_byte_pos == node.end_byte_pos
                && !complement_token.contains(&node.start_byte_pos)
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
                .for_each(|c| self.parse_rec(c, peekable, complement_token));
            self.builder.finish_node();
        }
    }

    fn parse(
        mut self,
        nodes: &Vec<&Node>,
        extras: Vec<(SyntaxKind, usize, usize, &str)>,
        complement_token: &HashSet<usize>,
    ) -> (GreenNode, impl Resolver) {
        let mut peekable = extras.into_iter().peekable();

        self.builder.start_node(SyntaxKind::Root);

        for node in nodes {
            self.parse_rec(node, &mut peekable, complement_token);
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

pub enum ParseTransform {
    InsertToken(TokenKind),
    SkipToken,
}

pub struct LRParseState<'a> {
    state: u32,
    stack: &'a [(u32, Node)],
    action_table: &'a [i16],
    goto_table: &'a [i16],
    extras: &'a [(SyntaxKind, usize, usize, &'a str)],
    token: &'a Token,
}

impl<'a> LRParseState<'a> {
    pub fn adjacent_c_comment(&self) -> bool {
        match self.extras.last() {
            // 手前のCコメントと、これから処理しようとしているtokenが隣接していないこと
            Some(e) if e.2 != self.token.start_byte_pos && e.0 == SyntaxKind::C_COMMENT => true,
            _ => false,
        }
    }
}

pub trait ParseTransformer {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform>;
}

/// 欠落しているバインド変数のサンプル値を補間
struct ComplementMissingSampleValueTransformer;

impl ComplementMissingSampleValueTransformer {
    fn is_bind_variable_comment(s: impl AsRef<str>) -> bool {
        let s = s.as_ref();
        s.starts_with("/*")
            && s.ends_with("*/")
            && !s.contains('\n')
            && !matches!(s.chars().nth(2).unwrap(), '$' | '#')
    }

    fn is_missing_bind_variable<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        match lr_parse_state.extras.last() {
            Some((_, _, _, s)) if Self::is_bind_variable_comment(s) => {
                let action_index = (lr_parse_state.state * num_terminal_symbol()) as usize
                    + SyntaxKind::SCONST as usize;

                let a = lr_parse_state.action_table[action_index];
                a != 0x7FFF
            }
            _ => false,
        }
    }
}

impl ParseTransformer for ComplementMissingSampleValueTransformer {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !lr_parse_state.adjacent_c_comment() {
            return None;
        }

        if !Self::is_missing_bind_variable(&lr_parse_state) {
            return None;
        }

        Some(ParseTransform::InsertToken(TokenKind::SCONST))
    }
}

/// 欠落している置換文字列のサンプル値(FROM句のみ)を補間
struct ComplementMissingFromTableTransformer;

impl ComplementMissingFromTableTransformer {
    fn is_replacement_value_comment(s: impl AsRef<str>) -> bool {
        let s = s.as_ref();
        s.starts_with("/*#") && s.ends_with("*/") && !s.contains('\n')
    }

    fn is_from_table<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        match SyntaxKind::from_raw(RawSyntaxKind(
            lr_parse_state.stack.last().unwrap().1.component_id,
        )) {
            SyntaxKind::FROM => true,
            SyntaxKind::Comma => {
                let prev2_kind = lr_parse_state
                    .stack
                    .iter()
                    .nth_back(1)
                    .map(|(_, node)| SyntaxKind::from_raw(RawSyntaxKind(node.component_id)));

                if prev2_kind == Some(SyntaxKind::from_list) {
                    true
                } else {
                    false
                }
            }
            _ => false,
        }
    }

    fn is_missing_from_replacement_value<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        match lr_parse_state.extras.last() {
            Some((_, _, _, s))
                if Self::is_replacement_value_comment(s) && Self::is_from_table(lr_parse_state) =>
            {
                let action_index = (lr_parse_state.state * num_terminal_symbol()) as usize
                    + SyntaxKind::IDENT as usize;

                let a = lr_parse_state.action_table[action_index];
                a != 0x7FFF
            }
            _ => false,
        }
    }
}

impl ParseTransformer for ComplementMissingFromTableTransformer {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !lr_parse_state.adjacent_c_comment() {
            return None;
        }

        if !Self::is_missing_from_replacement_value(&lr_parse_state) {
            return None;
        }

        Some(ParseTransform::InsertToken(TokenKind::IDENT))
    }
}

/// WHERE句の先頭にある余分なAND/ORをスキップする
struct SkipExtraOperator;

impl SkipExtraOperator {
    fn allow_extra_operator<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        match lr_parse_state
            .stack
            .last()
            .map(|(_, node)| SyntaxKind::from_raw(RawSyntaxKind(node.component_id)))
        {
            Some(SyntaxKind::WHERE) => true,
            _ => false,
        }
    }
}

impl ParseTransformer for SkipExtraOperator {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !Self::allow_extra_operator(&lr_parse_state) {
            return None;
        }

        if matches!(lr_parse_state.token.kind, TokenKind::KEYWORD(_)) {
            let cid = token_kind_to_component_id(&lr_parse_state.token.kind);
            let syntax_kind = SyntaxKind::from_raw(RawSyntaxKind(cid));

            if !matches!(syntax_kind, SyntaxKind::AND | SyntaxKind::OR) {
                return None;
            }
        }

        Some(ParseTransform::SkipToken)
    }
}

/// SELECT句、FROM句、ORDER BY句の先頭にある余分なカンマをスキップする
struct SkipExtraComma;

impl SkipExtraComma {
    fn allow_extra_comma<'a>(lr_parse_state: &LRParseState<'a>) -> bool {
        match lr_parse_state
            .stack
            .last()
            .map(|(_, node)| SyntaxKind::from_raw(RawSyntaxKind(node.component_id)))
        {
            Some(SyntaxKind::FROM) | Some(SyntaxKind::BY) | Some(SyntaxKind::SELECT) => true,
            _ => false,
        }
    }
}

impl ParseTransformer for SkipExtraComma {
    fn transform<'a>(&self, lr_parse_state: &LRParseState<'a>) -> Option<ParseTransform> {
        if !Self::allow_extra_comma(&lr_parse_state) {
            return None;
        }

        if matches!(lr_parse_state.token.kind, TokenKind::RAW(_)) {
            let cid = token_kind_to_component_id(&lr_parse_state.token.kind);
            let syntax_kind = SyntaxKind::from_raw(RawSyntaxKind(cid));

            if !matches!(syntax_kind, SyntaxKind::Comma) {
                return None;
            }
        }

        Some(ParseTransform::SkipToken)
    }
}

/// Parsing a string as PostgreSQL syntax and converting it into a ResolvedNode
pub fn parse(input: &str) -> Result<ResolvedNode, ParseError> {
    parse_with_transformer(input, &[])
}

/// Parsing a string as PostgreSQL syntax and converting it into a ResolvedNode
pub fn parse_with_transformer(
    input: &str,
    transformers: &[&dyn ParseTransformer],
) -> Result<ResolvedNode, ParseError> {
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
    let mut complement_token = HashSet::new();

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

        {
            let lr_parse_state = LRParseState {
                state,
                stack: &stack,
                action_table,
                goto_table,
                extras: &extras,
                token: &token,
            };

            if let Some(parse_transform) = transformers
                .iter()
                .find_map(|t| t.transform(&lr_parse_state))
            {
                match parse_transform {
                    ParseTransform::InsertToken(token_kind) => {
                        let last_extra = extras.last().unwrap();

                        cid = token_kind_to_component_id(&token_kind);
                        token = Token {
                            start_byte_pos: last_extra.2,
                            end_byte_pos: last_extra.2,
                            kind: token_kind,
                            value: String::new(),
                        };
                        complement_token.insert(token.start_byte_pos);

                        action = match action_table[(state * num_terminal_symbol() + cid) as usize]
                        {
                            0x7FFF => Action::Error,
                            v if v > 0 => Action::Shift((v - 1) as usize),
                            v if v < 0 => Action::Reduce((-v - 1) as usize),
                            _ => Action::Accept,
                        };
                        insert_dummy_token = true;
                    }

                    ParseTransform::SkipToken => {
                        // extraとして扱う
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
                }
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
    let (ast, resolver) = parser.parse(&root, extras, &complement_token);

    Ok(SyntaxNode::new_root_with_resolver(ast, resolver))
}

#[cfg(test)]
mod tests {
    use crate::cst::{
        parse_with_transformer, ComplementMissingFromTableTransformer,
        ComplementMissingSampleValueTransformer, SkipExtraComma, SkipExtraOperator,
    };

    #[test]
    fn test_missing_sample_value() {
        let s = r#"
select
,   /*hoge*/ as hoge
,   /*fuga*/
,   /*fuga*/ * 1
,   /*fuga*/ || 'hoge'
from
,   /*#tbl1*/ t1
,   /*#tbl2*/ t2
where
and    /*val*/ = 1;
"#
        .trim();

        eprintln!("sql:{s}");
        // dbg!(parse(s).unwrap());
        dbg!(parse_with_transformer(
            s,
            &[
                &ComplementMissingFromTableTransformer,
                &ComplementMissingSampleValueTransformer,
                &SkipExtraOperator,
                &SkipExtraComma,
            ]
        )
        .unwrap());

        let expected = r#"
Root@0..139
  parse_toplevel@0..139
    stmtmulti@0..139
      stmtmulti@0..138
        toplevel_stmt@0..138
          stmt@0..138
            SelectStmt@0..138
              select_no_parens@0..138
                simple_select@0..138
                  SELECT@0..6 "select"
                  Whitespace@6..7 "\n"
                  Comma@7..8 ","
                  Whitespace@8..11 "   "
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
                  from_clause@75..113
                    FROM@75..79 "from"
                    Whitespace@79..80 "\n"
                    Comma@80..81 ","
                    Whitespace@81..84 "   "
                    C_COMMENT@84..93 "/*#tbl1*/"
                    from_list@93..113
                      from_list@93..96
                        table_ref@93..96
                          relation_expr@93..93
                            qualified_name@93..93
                              ColId@93..93
                                IDENT@93..93 ""
                          Whitespace@93..94 " "
                          opt_alias_clause@94..96
                            alias_clause@94..96
                              ColId@94..96
                                IDENT@94..96 "t1"
                      Whitespace@96..97 "\n"
                      Comma@97..98 ","
                      Whitespace@98..101 "   "
                      C_COMMENT@101..110 "/*#tbl2*/"
                      table_ref@110..113
                        relation_expr@110..110
                          qualified_name@110..110
                            ColId@110..110
                              IDENT@110..110 ""
                        Whitespace@110..111 " "
                        opt_alias_clause@111..113
                          alias_clause@111..113
                            ColId@111..113
                              IDENT@111..113 "t2"
                  Whitespace@113..114 "\n"
                  where_clause@114..138
                    WHERE@114..119 "where"
                    Whitespace@119..120 "\n"
                    AND@120..123 "and"
                    Whitespace@123..127 "    "
                    C_COMMENT@127..134 "/*val*/"
                    a_expr@134..138
                      a_expr@134..134
                        c_expr@134..134
                          AexprConst@134..134
                            Sconst@134..134
                              SCONST@134..134 ""
                      Whitespace@134..135 " "
                      Equals@135..136 "="
                      Whitespace@136..137 " "
                      a_expr@137..138
                        c_expr@137..138
                          AexprConst@137..138
                            Iconst@137..138
                              ICONST@137..138 "1"
      Semicolon@138..139 ";"
 */"#;
    }
}
