pub(crate) mod extra;
pub(crate) mod lr_parse_state;

pub(crate) use extra::*;
pub(crate) use lr_parse_state::*;

use cstree::{
    build::GreenNodeBuilder, green::GreenNode, interning::Resolver, RawSyntaxKind, Syntax,
};

use crate::{
    lexer::{lex, lexer_ported::init_tokens, parser_error::ParserError, TokenKind},
    parser::{
        end_rule_id, end_rule_kind, num_terminal_symbol, rule_name_to_component_id,
        token_kind_to_component_id, Action, ACTION_CHECK_TABLE, ACTION_DEF_RULE_TABLE,
        ACTION_TABLE, ACTION_TABLE_INDEX, GOTO_CHECK_TABLE, GOTO_TABLE, GOTO_TABLE_INDEX, RULES,
    },
    transform::{ParseTransform, ParseTransformer},
};

use super::{lexer::Token, syntax_kind::SyntaxKind};

pub(crate) const ERROR_ACTION_CODE: i16 = 0x7FFF;
pub(crate) const DEFAULT_ACTION_CODE: i16 = 0x7FFE;
pub(crate) const INVALID_GOTO_CODE: i16 = -1;

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

/// ノードがトークンを含むか否かを判定する
/// トークンを含まないノードは削除し、ダミートークンを含む補完されたノードは残すために使用する
fn contains_token(node: &Node) -> bool {
    if node.token.is_some() {
        return true;
    }

    node.children.iter().any(contains_token)
}

impl Parser {
    fn parse_rec(
        &mut self,
        node: &Node,
        peekable: &mut std::iter::Peekable<std::vec::IntoIter<Extra>>,
    ) {
        if cfg!(feature = "remove-empty-node") {
            if node.start_byte_pos == node.end_byte_pos && !contains_token(node) {
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
                .for_each(|c| self.parse_rec(c, peekable));
            self.builder.finish_node();
        }
    }

    fn parse(mut self, nodes: &Vec<&Node>, extras: Vec<Extra>) -> (GreenNode, impl Resolver) {
        let mut peekable = extras.into_iter().peekable();

        self.builder.start_node(SyntaxKind::Root);

        for node in nodes {
            self.parse_rec(node, &mut peekable);
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

pub(crate) fn lookup_parser_action(state: u32, cid: u32) -> i16 {
    let state = state as usize;
    let cid = cid as usize;

    let i = ACTION_TABLE_INDEX[state] as usize;
    if ACTION_CHECK_TABLE[i + cid] == cid as i16 {
        if ACTION_TABLE[i + cid] == DEFAULT_ACTION_CODE {
            ACTION_DEF_RULE_TABLE[state]
        } else {
            ACTION_TABLE[i + cid]
        }
    } else {
        ERROR_ACTION_CODE
    }
}

pub(crate) fn lookup_goto_state(state: u32, cid: u32) -> i16 {
    let state = state as usize;
    let cid = cid as usize;

    let i = GOTO_TABLE_INDEX[state] as usize;
    if GOTO_CHECK_TABLE[i + cid] == cid as i16 {
        GOTO_TABLE[i + cid]
    } else {
        INVALID_GOTO_CODE
    }
}

/// Parsing a string as PostgreSQL syntax and converting it into a ResolvedNode
pub fn parse(input: &str) -> Result<ResolvedNode, ParserError> {
    parse_with_transformer(input, &[])
}

/// Parsing a string as PostgreSQL syntax and converting it into a ResolvedNode
pub fn parse_with_transformer(
    input: &str,
    transformers: &[&dyn ParseTransformer],
) -> Result<ResolvedNode, ParserError> {
    let mut tokens = lex(input)?;

    if !tokens.is_empty() {
        init_tokens(&mut tokens);
    }

    tokens.push(Token {
        kind: end_rule_kind(),
        value: "".to_string(),
        start_byte_pos: input.len(),
        end_byte_pos: input.len(),
    });

    struct TokenQueue {
        tokens: std::iter::Peekable<std::vec::IntoIter<Token>>,
        dummy_token: Option<Token>,
    }

    impl TokenQueue {
        fn new(tokens: Vec<Token>) -> Self {
            Self {
                tokens: tokens.into_iter().peekable(),
                dummy_token: None,
            }
        }

        fn next(&mut self) -> Option<Token> {
            if self.dummy_token.is_some() {
                let dummy_token = self.dummy_token.take();
                dummy_token
            } else {
                self.tokens.next()
            }
        }

        fn peek(&mut self) -> Option<&Token> {
            self.dummy_token.as_ref().or_else(|| self.tokens.peek())
        }

        fn insert_dummy_token(&mut self, token: Token) {
            if self.dummy_token.is_some() {
                panic!();
            }

            self.dummy_token = Some(token);
        }
    }

    let mut stack: Vec<(u32, Node)> = Vec::new();
    let mut tokens = TokenQueue::new(tokens);

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

    loop {
        let state = stack.last().unwrap().0;
        let mut token = match tokens.peek() {
            Some(token) => token.clone(),
            None => {
                return Err(ParserError::ParseError {
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

        let mut action = match lookup_parser_action(state, cid) {
            0x7FFF => Action::Error,
            v if v > 0 => Action::Shift((v - 1) as usize),
            v if v < 0 => Action::Reduce((-v - 1) as usize),
            _ => Action::Accept,
        };

        // transform
        if action == Action::Error {
            let lr_parse_state = LRParseState {
                state,
                stack: &stack,
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

                        action = match lookup_parser_action(state, cid) {
                            0x7FFF => Action::Error,
                            v if v > 0 => Action::Shift((v - 1) as usize),
                            v if v < 0 => Action::Reduce((-v - 1) as usize),
                            _ => Action::Accept,
                        };
                        tokens.insert_dummy_token(token.clone());
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
                tokens.next();
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
                let goto = lookup_goto_state(next_state, reduced_component_id);

                match goto {
                    next_state if next_state >= 0 => {
                        stack.push((next_state as u32, node));
                    }
                    _ => {
                        return Err(ParserError::ParseError {
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
                return Err(ParserError::ParseError {
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

        // The last token is $end, so exit the loop here
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
    let (ast, resolver) = parser.parse(&root, extras);

    Ok(SyntaxNode::new_root_with_resolver(ast, resolver))
}
