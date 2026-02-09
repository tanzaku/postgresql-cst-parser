#[cfg(test)]
mod assert_util;

mod convert;
use convert::get_ts_tree_and_range_map;

use std::{collections::HashMap, fmt::Display, rc::Rc, str};

use cstree::text::TextRange;

use crate::{cst, syntax_kind::SyntaxKind, NodeOrToken, ParserError, ResolvedNode};

impl Display for SyntaxKind {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{self:?}")
    }
}

pub fn parse(input: &str) -> Result<Tree, ParserError> {
    let parsed = cst::parse(input)?;
    let (root, range_map) = get_ts_tree_and_range_map(input, &parsed);
    Ok(Tree::new(input, root, range_map))
}

pub fn parse_2way(input: &str) -> Result<Tree, ParserError> {
    let parsed = crate::parse_2way(input)?;
    let (root, range_map) = get_ts_tree_and_range_map(input, &parsed);
    Ok(Tree::new(input, root, range_map))
}

pub struct Tree {
    src: String,
    root: ResolvedNode,
    range_map: Rc<HashMap<TextRange, Range>>,
}

impl Tree {
    pub fn new<T: Into<String>>(
        src: T,
        root: ResolvedNode,
        range_map: HashMap<TextRange, Range>,
    ) -> Self {
        Self {
            src: src.into(),
            root,
            range_map: Rc::new(range_map),
        }
    }

    pub fn root_node(&self) -> Node<'_> {
        Node {
            input: &self.src,
            range_map: Rc::clone(&self.range_map),
            node_or_token: NodeOrToken::Node(&self.root),
        }
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

// https://github.com/tree-sitter/tree-sitter/blob/90666c951d53c13cc6cf5002d971a6debed74244/lib/binding_rust/lib.rs#L74-L78
#[derive(Debug, Clone)]
pub struct Point {
    pub row: usize,
    pub column: usize,
}

impl std::fmt::Display for Point {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "({}, {})", self.row, self.column)
    }
}

// https://github.com/tree-sitter/tree-sitter/blob/90666c951d53c13cc6cf5002d971a6debed74244/lib/binding_rust/lib.rs#L80-L88
#[derive(Debug, Clone)]
pub struct Range {
    pub start_byte: usize,
    pub end_byte: usize,
    pub start_position: Point,
    pub end_position: Point,
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}-{}]", self.start_position, self.end_position)
    }
}

impl<'a> Node<'a> {
    pub fn walk(&self) -> TreeCursor<'a> {
        TreeCursor {
            input: self.input,
            range_map: Rc::clone(&self.range_map),
            node_or_token: self.node_or_token,
        }
    }

    pub fn kind(&self) -> SyntaxKind {
        self.node_or_token.kind()
    }

    pub fn range(&self) -> Range {
        self.range_map
            .get(&self.node_or_token.text_range())
            .cloned()
            .unwrap()
    }

    pub fn start_position(&self) -> Point {
        self.range().start_position
    }

    pub fn end_position(&self) -> Point {
        self.range().end_position
    }

    pub fn text(&self) -> &'a str {
        let Range {
            start_byte,
            end_byte,
            ..
        } = self.range();

        &self.input[start_byte..end_byte]
    }

    pub fn child_count(&self) -> usize {
        if let Some(node) = self.node_or_token.as_node() {
            node.children_with_tokens().count()
        } else {
            0
        }
    }

    pub fn next_sibling(&self) -> Option<Node<'a>> {
        self.node_or_token
            .next_sibling_or_token()
            .map(|sibling| Node {
                input: self.input,
                range_map: Rc::clone(&self.range_map),
                node_or_token: sibling,
            })
    }

    pub fn parent(&self) -> Option<Node<'a>> {
        self.node_or_token.parent().map(|parent| Node {
            input: self.input,
            range_map: Rc::clone(&self.range_map),
            node_or_token: NodeOrToken::Node(parent),
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
    pub fn node(&self) -> Node<'a> {
        Node {
            input: self.input,
            range_map: Rc::clone(&self.range_map),
            node_or_token: self.node_or_token,
        }
    }

    pub fn goto_first_child(&mut self) -> bool {
        if let Some(current_node) = self.node_or_token.as_node() {
            if let Some(child) = current_node.first_child_or_token() {
                self.node_or_token = child;
                return true;
            }
        }
        false
    }

    pub fn goto_parent(&mut self) -> bool {
        if let Some(parent) = self.node_or_token.parent() {
            self.node_or_token = NodeOrToken::Node(parent);
            true
        } else {
            false
        }
    }

    pub fn goto_next_sibling(&mut self) -> bool {
        if let Some(sibling) = self.node_or_token.next_sibling_or_token() {
            self.node_or_token = sibling;
            true
        } else {
            false
        }
    }

    pub fn is_comment(&self) -> bool {
        matches!(
            self.node_or_token.kind(),
            SyntaxKind::C_COMMENT | SyntaxKind::SQL_COMMENT
        )
    }
}

pub fn as_tree_sitter_cursor<'a>(
    input: &'a str,
    node: &'a ResolvedNode,
    range_map: HashMap<TextRange, Range>,
) -> TreeCursor<'a> {
    TreeCursor {
        input,
        range_map: Rc::new(range_map),
        node_or_token: NodeOrToken::Node(node),
    }
}

#[cfg(test)]
mod tests {
    use crate::{syntax_kind::SyntaxKind, tree_sitter::parse};

    #[test]
    fn empty_src_range() {
        let src = "";
        let tree = parse(src).unwrap();
        let root = tree.root_node();

        assert_eq!(root.range().to_string(), "[(0, 0)-(0, 0)]");
    }

    #[test]
    fn test_parent_method() {
        let src = "SELECT id FROM users;";
        let tree = parse(src).unwrap();
        let root = tree.root_node();

        // ルートノードの親はない
        assert!(root.parent().is_none());

        // 子ノードから親ノードへの参照をテスト
        let mut cursor = root.walk();
        assert!(cursor.goto_first_child());

        let select_stmt = cursor.node();
        assert_eq!(select_stmt.kind(), SyntaxKind::SelectStmt);

        // 子ノードの親はルートノード
        let parent = select_stmt.parent().unwrap();
        assert_eq!(parent.kind(), SyntaxKind::Root);

        // さらに深いノードの階層関係をテスト
        if cursor.goto_first_child() {
            let child_node = cursor.node();
            let parent_node = child_node.parent().unwrap();
            assert_eq!(parent_node.kind(), SyntaxKind::SelectStmt);

            // 親の親はルートノード
            let grandparent = parent_node.parent().unwrap();
            assert_eq!(grandparent.kind(), SyntaxKind::Root);
        }
    }

    #[test]
    fn range_single_line() {
        let src = "select a from b;";
        let tree = parse(src).unwrap();
        let root = tree.root_node();
        let mut cursor = root.walk();

        // extract tokens
        let mut tokens = vec![];
        'traverse: loop {
            if cursor.node().child_count() == 0 {
                tokens.push((cursor.node().text(), cursor.node().range()));
            }

            if cursor.goto_first_child() || cursor.goto_next_sibling() {
            } else {
                loop {
                    if !cursor.goto_parent() {
                        break 'traverse;
                    }

                    if cursor.goto_next_sibling() {
                        break;
                    }
                }
            }
        }

        assert_eq!(tokens.len(), 5);
        let mut tokens = tokens.iter();

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "select");
        assert_eq!(range.to_string(), "[(0, 0)-(0, 6)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "a");
        assert_eq!(range.to_string(), "[(0, 7)-(0, 8)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "from");
        assert_eq!(range.to_string(), "[(0, 9)-(0, 13)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "b");
        assert_eq!(range.to_string(), "[(0, 14)-(0, 15)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, ";");
        assert_eq!(range.to_string(), "[(0, 15)-(0, 16)]");

        assert!(tokens.next().is_none());
    }

    #[test]
    fn range_multiple_line() {
        let src = r#"
select
	a
from
	b
;"#;
        let tree = parse(src).unwrap();
        let root = tree.root_node();
        let mut cursor = root.walk();

        // extract tokens
        let mut tokens = vec![];
        'traverse: loop {
            if cursor.node().child_count() == 0 {
                tokens.push((cursor.node().text(), cursor.node().range()));
            }

            if cursor.goto_first_child() {
            } else if cursor.goto_next_sibling() {
            } else {
                loop {
                    if !cursor.goto_parent() {
                        break 'traverse;
                    }

                    if cursor.goto_next_sibling() {
                        break;
                    }
                }
            }
        }

        assert_eq!(tokens.len(), 5);
        let mut tokens = tokens.iter();

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "select");
        assert_eq!(range.to_string(), "[(1, 0)-(1, 6)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "a");
        assert_eq!(range.to_string(), "[(2, 1)-(2, 2)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "from");
        assert_eq!(range.to_string(), "[(3, 0)-(3, 4)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, "b");
        assert_eq!(range.to_string(), "[(4, 1)-(4, 2)]");

        let (text, range) = tokens.next().unwrap();
        assert_eq!(*text, ";");
        assert_eq!(range.to_string(), "[(5, 0)-(5, 1)]");

        assert!(tokens.next().is_none());
    }

    #[test]
    fn test_tree_basics() {
        let src = "SELECT id FROM users;";
        let tree = parse(src).unwrap();
        let root = tree.root_node();

        assert_eq!(root.kind(), SyntaxKind::Root);
        assert_eq!(root.text(), src);
        assert!(root.child_count() > 0);
    }

    #[test]
    fn test_cursor_navigation() {
        let src = "SELECT id FROM users;";
        let tree = parse(src).unwrap();
        let mut cursor = tree.root_node().walk();

        assert!(cursor.goto_first_child());
        let select_stmt = cursor.node();
        assert_eq!(select_stmt.kind(), SyntaxKind::SelectStmt);

        assert!(cursor.goto_parent());
        assert_eq!(cursor.node().kind(), SyntaxKind::Root);
    }

    #[test]
    fn test_node_properties() {
        let src = "SELECT id\nFROM users;";
        let tree = parse(src).unwrap();
        let root = tree.root_node();

        let start = root.start_position();
        let end = root.end_position();
        assert_eq!(start.row, 0);
        assert_eq!(start.column, 0);
        assert!(end.row >= 1);
    }

    #[test]
    fn test_comment_handling() {
        let src = "-- This is a comment\nSELECT id FROM users;";
        let tree = parse(src).unwrap();
        let mut cursor = tree.root_node().walk();

        cursor.goto_first_child();
        assert!(cursor.node().is_comment());
    }

    #[test]
    fn test_multiple_statements() {
        let src = "SELECT 1; SELECT 2;";
        let tree = parse(src).unwrap();
        let root = tree.root_node();

        let mut cursor = root.walk();
        let mut stmt_count = 0;

        cursor.goto_first_child();
        loop {
            if cursor.node().kind() == SyntaxKind::SelectStmt {
                stmt_count += 1;
            }
            if !cursor.goto_next_sibling() {
                break;
            }
        }

        assert_eq!(stmt_count, 2);
    }
}
