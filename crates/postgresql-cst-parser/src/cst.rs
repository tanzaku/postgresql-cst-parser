pub(crate) mod extra;
pub(crate) mod lr_parse_state;

pub(crate) use extra::*;
pub(crate) use lr_parse_state::*;

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
    transform::{ParseTransform, ParseTransformer},
};

use super::{lexer::Token, syntax_kind::SyntaxKind};

pub(crate) struct Node {
    token: Option<Token>,
    pub component_id: u32,
    children: Vec<Node>,
    start_byte_pos: usize,
    end_byte_pos: usize,
}

impl From<&Node> for SyntaxKind {
    fn from(value: &Node) -> Self {
        SyntaxKind::from_raw(RawSyntaxKind(value.component_id))
    }
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
        peekable: &mut std::iter::Peekable<std::vec::IntoIter<Extra>>,
        complement_token: &HashSet<usize>,
    ) {
        if cfg!(feature = "remove-empty-node") {
            if node.start_byte_pos == node.end_byte_pos
                && !complement_token.contains(&node.start_byte_pos)
            {
                return;
            }
        }

        while let Some(Extra {
            kind,
            start_byte_pos,
            comment,
            ..
        }) = peekable.peek()
        {
            // TODO: Consider whether the presence or absence of an equals sign changes the position of comments. Determine which option is preferable
            if *start_byte_pos >= node.start_byte_pos {
                // if *start > node.start_byte_pos {
                break;
            }
            self.builder.token(*kind, &comment);
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
        extras: Vec<Extra>,
        complement_token: &HashSet<usize>,
    ) -> (GreenNode, impl Resolver) {
        let mut peekable = extras.into_iter().peekable();

        self.builder.start_node(SyntaxKind::Root);

        for node in nodes {
            self.parse_rec(node, &mut peekable, complement_token);
        }

        while let Some(Extra { kind, comment, .. }) = peekable.peek() {
            self.builder.token(*kind, comment);
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

    if !tokens.is_empty() {
        init_tokens(&mut tokens);
    }

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
    let mut extras: Vec<Extra> = Vec::new();
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
                extras.push(Extra {
                    kind: SyntaxKind::Whitespace,
                    start_byte_pos: last_pos,
                    end_byte_pos: token.start_byte_pos,
                    comment: &input[last_pos..token.start_byte_pos],
                });
            }

            last_pos = token.end_byte_pos;

            let kind = SyntaxKind::from_raw(RawSyntaxKind(cid));
            extras.push(Extra {
                kind,
                start_byte_pos: token.start_byte_pos,
                end_byte_pos: token.end_byte_pos,
                comment: &input[token.start_byte_pos..token.end_byte_pos],
            });
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

        // transform
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
                            start_byte_pos: last_extra.end_byte_pos,
                            end_byte_pos: last_extra.end_byte_pos,
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
                        // Skip tokens are treated as extras
                        if last_pos < token.start_byte_pos {
                            extras.push(Extra {
                                kind: SyntaxKind::Whitespace,
                                start_byte_pos: last_pos,
                                end_byte_pos: token.start_byte_pos,
                                comment: &input[last_pos..token.start_byte_pos],
                            });
                        }

                        last_pos = token.end_byte_pos;

                        let kind = SyntaxKind::from_raw(RawSyntaxKind(cid));
                        extras.push(Extra {
                            kind,
                            start_byte_pos: token.start_byte_pos,
                            end_byte_pos: token.end_byte_pos,
                            comment: &input[token.start_byte_pos..token.end_byte_pos],
                        });
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
                    extras.push(Extra {
                        kind: SyntaxKind::Whitespace,
                        start_byte_pos: last_pos,
                        end_byte_pos: token.start_byte_pos,
                        comment: &input[last_pos..token.start_byte_pos],
                    });
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
                                .map(|e| e.end_byte_pos)
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
            extras.push(Extra {
                kind: SyntaxKind::Whitespace,
                start_byte_pos: last_pos,
                end_byte_pos: token.start_byte_pos,
                comment: &input[last_pos..token.start_byte_pos],
            });
        }

        last_pos = token.end_byte_pos;

        // 最後のトークンは$endなので、ここで終了
        if tokens.peek().is_none() {
            break;
        }

        let cid = token_kind_to_component_id(&token.kind);
        let kind = SyntaxKind::from_raw(RawSyntaxKind(cid));
        extras.push(Extra {
            kind,
            start_byte_pos: token.start_byte_pos,
            end_byte_pos: token.end_byte_pos,
            comment: &input[token.start_byte_pos..token.end_byte_pos],
        });
    }

    let parser = Parser {
        builder: GreenNodeBuilder::new(),
    };
    let root: Vec<&Node> = stack[1..].iter().map(|s| &s.1).collect();
    let (ast, resolver) = parser.parse(&root, extras, &complement_token);

    Ok(SyntaxNode::new_root_with_resolver(ast, resolver))
}
