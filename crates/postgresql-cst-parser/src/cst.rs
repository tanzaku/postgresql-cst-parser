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
};

use super::{lexer::Token, syntax_kind::SyntaxKind};

const ERROR_ACTION_CODE: i16 = 0x7FFF;
const DEFAULT_ACTION_CODE: i16 = 0x7FFE;
const INVALID_GOTO_CODE: i16 = -1;

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

impl Parser {
    fn parse_rec(
        &mut self,
        node: &Node,
        peekable: &mut std::iter::Peekable<std::vec::IntoIter<(SyntaxKind, usize, usize, &str)>>,
    ) {
        if cfg!(feature = "remove-empty-node") && node.start_byte_pos == node.end_byte_pos {
            return;
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
                .for_each(|c| self.parse_rec(c, peekable));
            self.builder.finish_node();
        }
    }

    fn parse(
        mut self,
        nodes: &Vec<&Node>,
        extras: Vec<(SyntaxKind, usize, usize, &str)>,
    ) -> (GreenNode, impl Resolver) {
        let mut peekable = extras.into_iter().peekable();

        self.builder.start_node(SyntaxKind::Root);

        for node in nodes {
            self.parse_rec(node, &mut peekable);
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

fn lookup_parser_action(state: u32, cid: u32) -> i16 {
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

fn lookup_goto_state(state: u32, cid: u32) -> i16 {
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

    loop {
        let state = stack.last().unwrap().0;
        let token = match tokens.peek() {
            Some(token) => token,
            None => {
                return Err(ParserError::ParseError {
                    message: "unexpected end of input".to_string(),
                    start_byte_pos: input.len(),
                    end_byte_pos: input.len(),
                });
            }
        };

        let cid = token_kind_to_component_id(&token.kind);

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

        let action = match lookup_parser_action(state, cid) {
            ERROR_ACTION_CODE => Action::Error,
            v if v > 0 => Action::Shift((v - 1) as usize),
            v if v < 0 => Action::Reduce((-v - 1) as usize),
            _ => Action::Accept,
        };

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
            extras.push((
                SyntaxKind::Whitespace,
                last_pos,
                token.start_byte_pos,
                &input[last_pos..token.start_byte_pos],
            ));
        }

        last_pos = token.end_byte_pos;

        // The last token is $end, so exit the loop here
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
    let (ast, resolver) = parser.parse(&root, extras);

    Ok(SyntaxNode::new_root_with_resolver(ast, resolver))
}
