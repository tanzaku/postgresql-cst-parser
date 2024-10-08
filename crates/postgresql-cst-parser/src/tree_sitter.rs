use std::{collections::HashMap, fmt::Display, rc::Rc};

use cstree::text::TextRange;

use crate::{syntax_kind::SyntaxKind, NodeOrToken, ResolvedNode};

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{:?}", self)
    }
}

#[derive(Debug, Clone)]
pub struct Node<'a> {
    input: &'a str,
    range_map: Rc<HashMap<TextRange, Range>>,
    pub node_or_token: NodeOrToken<'a>,
}

#[derive(Debug, Clone)]
pub struct TreeCursor<'a> {
    pub input: &'a str,
    range_map: Rc<HashMap<TextRange, Range>>,
    node_or_token: NodeOrToken<'a>,
}

#[derive(Debug, Clone)]
pub struct Range {
    pub start_row: usize,
    pub start_col: usize,
    pub end_row: usize,
    pub end_col: usize,
}

fn is_flatten_all(node_or_token: NodeOrToken) -> bool {
    matches!(
        node_or_token.kind(),
        SyntaxKind::parse_toplevel
            | SyntaxKind::stmtmulti
            | SyntaxKind::toplevel_stmt
            | SyntaxKind::stmt
            | SyntaxKind::select_clause
            | SyntaxKind::select_with_parens
            | SyntaxKind::select_no_parens
            | SyntaxKind::simple_select
            | SyntaxKind::opt_target_list
            // | SyntaxKind::relation_expr
            // | SyntaxKind::extended_relation_expr
            // | SyntaxKind::qualified_name
            // | SyntaxKind::indirection
            // | SyntaxKind::indirection_el
            // | SyntaxKind::table_ref
            | SyntaxKind::alias_clause
            | SyntaxKind::opt_alias_clause
    )
}

fn is_flatten_except_top(node_or_token: NodeOrToken) -> bool {
    matches!(
        node_or_token.kind(),
        SyntaxKind::target_list | SyntaxKind::from_list
    ) && node_or_token.parent().unwrap().kind() == node_or_token.kind()
}

fn is_flatten(node_or_token: NodeOrToken) -> bool {
    is_flatten_all(node_or_token) || is_flatten_except_top(node_or_token)
}

fn is_skip(kind: SyntaxKind) -> bool {
    matches!(kind, SyntaxKind::Whitespace)
}

impl<'a> Node<'a> {
    pub fn walk(&self) -> TreeCursor<'a> {
        unimplemented!()
    }

    pub fn kind(&self) -> SyntaxKind {
        self.node_or_token.kind()
    }

    pub fn range(&self) -> Range {
        // dbg!(self.node_or_token.text_range(), &self.range_map);
        self.range_map
            .get(&self.node_or_token.text_range())
            .cloned()
            .unwrap()
    }

    pub fn text(&self) -> &'a str {
        // self.node_or_token
        //     .as_token()
        //     .map(|t| t.text())
        //     .unwrap_or_default()
        let start = self.node_or_token.text_range().start().into();
        let end = self.node_or_token.text_range().end().into();
        &self.input[start..end]
    }

    pub fn children(&self) {}

    pub fn child_count(&self) {}

    pub fn next_sibling(&self) -> Option<Node<'a>> {
        self.node_or_token
            .next_sibling_or_token()
            .map(|sibling| Node {
                input: self.input,
                range_map: Rc::clone(&self.range_map),
                node_or_token: sibling,
            })
    }

    pub fn is_comment(&self) -> bool {
        matches!(self.kind(), SyntaxKind::C_COMMENT | SyntaxKind::SQL_COMMENT)
    }
}

impl<'a> From<Node<'a>> for TreeCursor<'a> {
    fn from(value: Node<'a>) -> Self {
        Self {
            input: value.input,
            range_map: value.range_map,
            node_or_token: value.node_or_token,
        }
    }
}

impl<'a> TreeCursor<'a> {
    pub fn goto_first_child(&mut self) -> bool {
        if self.node_or_token.as_node().is_none() {
            return false;
        }

        let mut cursor = self.clone();

        // TODO 書き捨てコードなのでリファクタ
        loop {
            if let Some(node) = cursor.node_or_token.as_node() {
                if let Some(child) = node.first_child_or_token() {
                    cursor.node_or_token = child;

                    if is_skip(child.kind()) || is_flatten(child) {
                        continue;
                    }

                    self.node_or_token = cursor.node_or_token;
                    return true;
                }
            }
            if let Some(sibling) = cursor.node_or_token.next_sibling_or_token() {
                cursor.node_or_token = sibling;

                if is_skip(sibling.kind()) || is_flatten(sibling) {
                    continue;
                }

                return true;
            } else {
                cursor.node_or_token = NodeOrToken::Node(cursor.node_or_token.parent().unwrap());
            }
        }
    }

    pub fn goto_next_sibling(&mut self) -> bool {
        let mut cursor = self.clone();

        loop {
            while let Some(sibling) = cursor.node_or_token.next_sibling_or_token() {
                cursor.node_or_token = sibling;

                if is_skip(sibling.kind()) {
                    continue;
                }

                if is_flatten(sibling) {
                    cursor.goto_first_child();
                }

                self.node_or_token = cursor.node_or_token;
                return true;
            }

            if let Some(parent) = cursor.node_or_token.parent() {
                if !is_flatten(NodeOrToken::Node(parent)) {
                    return false;
                }

                cursor.node_or_token = NodeOrToken::Node(parent);
            } else {
                return false;
            }
        }
    }

    pub fn goto_direct_prev_sibling(&mut self) -> bool {
        if let Some(prev) = self.node_or_token.prev_sibling_or_token() {
            self.node_or_token = prev;
            true
        } else {
            false
        }
    }

    pub fn goto_parent(&mut self) -> bool {
        while let Some(parent) = self.node_or_token.parent() {
            self.node_or_token = NodeOrToken::Node(parent);

            if is_flatten(self.node_or_token) {
                continue;
            }

            return true;
        }

        false
    }

    pub fn node(&self) -> Node<'a> {
        Node {
            input: self.input,
            range_map: Rc::clone(&self.range_map),
            node_or_token: self.node_or_token,
        }
    }

    pub fn is_comment(&self) -> bool {
        matches!(
            self.node_or_token.kind(),
            SyntaxKind::C_COMMENT | SyntaxKind::SQL_COMMENT
        )
    }
}

pub fn as_tree_sitter_cursor<'a>(input: &'a str, node: &'a ResolvedNode) -> TreeCursor<'a> {
    let mut range_map = HashMap::new();

    let new_line_indices: Vec<_> = input
        .char_indices()
        .filter(|&(_, c)| c == '\n')
        .map(|(i, _)| i)
        .collect();

    traverse_pre_order(node, |node_or_token| {
        let text_range = node_or_token.text_range();

        let before_start_new_line_count =
            match new_line_indices.binary_search(&text_range.start().into()) {
                Ok(i) => i,
                Err(i) => i,
            };

        let before_end_new_line_count =
            match new_line_indices.binary_search(&text_range.end().into()) {
                Ok(i) => i,
                Err(i) => i,
            };

        range_map.insert(
            node_or_token.text_range(),
            Range {
                start_row: before_start_new_line_count,
                start_col: usize::from(node_or_token.text_range().start())
                    - match before_start_new_line_count {
                        0 => 0,
                        i => new_line_indices[i - 1] + 1,
                    },
                end_row: before_end_new_line_count,
                end_col: usize::from(node_or_token.text_range().end())
                    - 1
                    - match before_end_new_line_count {
                        0 => 0,
                        i => new_line_indices[i - 1],
                    },
            },
        );
    });

    TreeCursor {
        input,
        range_map: Rc::new(range_map),
        node_or_token: NodeOrToken::Node(node),
    }
}

fn traverse_pre_order<F: FnMut(NodeOrToken)>(node: &ResolvedNode, mut f: F) {
    let mut node_or_token = NodeOrToken::Node(node);

    loop {
        f(node_or_token);

        if let Some(node) = node_or_token.as_node() {
            if let Some(child) = node.first_child_or_token() {
                node_or_token = child;
                continue;
            }
        }

        if let Some(sibling) = node_or_token.next_sibling_or_token() {
            node_or_token = sibling;
        } else {
            loop {
                if let Some(parent) = node_or_token.parent() {
                    node_or_token = NodeOrToken::Node(parent);
                } else {
                    return;
                }

                if let Some(sibling) = node_or_token.next_sibling_or_token() {
                    node_or_token = sibling;
                    break;
                }
            }
        }
    }
}

pub fn dump_as_tree_sitter_like(input: &str, node: &ResolvedNode) {
    let mut cursor = as_tree_sitter_cursor(input, node);

    let mut depth = 0;
    loop {
        dbg!(cursor.node().kind(), cursor.node().text(), depth);

        if cursor.goto_first_child() {
            depth += 1;
        } else if cursor.goto_next_sibling() {
        } else {
            loop {
                if !cursor.goto_parent() {
                    return;
                }

                depth -= 1;
                if cursor.goto_next_sibling() {
                    break;
                }
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{cst, tree_sitter::dump_as_tree_sitter_like, ParseError};

    #[test]
    fn test() -> Result<(), ParseError> {
        let input = r#"
SELECT
	1 as X
,	2
,	3
FROM
	A
,	B"#;
        // dbg!(input);
        let node = cst::parse(input)?;
        dbg!(&node);

        dump_as_tree_sitter_like(input, &node);

        Ok(())
    }
}
