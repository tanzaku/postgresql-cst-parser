#[cfg(test)]
mod assert_util;

mod convert;
pub use convert::get_ts_tree_and_range_map;

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
    start_byte: usize,
    end_byte: usize,
    start_position: Point,
    end_position: Point,
}

impl std::fmt::Display for Range {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "[{}-{}]", self.start_position, self.end_position)
    }
}

impl<'a> Node<'a> {
    pub fn walk(&self, src: &'a str, range_map: HashMap<TextRange, Range>) -> TreeCursor<'a> {
        as_tree_sitter_cursor(src, self.node_or_token.as_node().unwrap(), range_map)
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
    use crate::{
        parse,
        syntax_kind::SyntaxKind,
        tree_sitter::{as_tree_sitter_cursor, get_ts_tree_and_range_map},
    };
    
    #[test]
    fn walk() {
        let src = "select a, b, c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());

        let mut cursor = as_tree_sitter_cursor(src, &root, range_map.clone());
        cursor.goto_first_child();
        let node = cursor.node();
        
        let new_cursor = node.walk(src, range_map);
        assert_eq!(cursor.node().kind(), new_cursor.node().kind())
    }

    #[test]
    fn goto_first_child_from_node() {
        let src = "select a, b, c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());
        let first_select = root
            .descendants()
            .find(|x| x.kind() == SyntaxKind::SelectStmt)
            .unwrap();

        let mut cursor = as_tree_sitter_cursor(src, &first_select, range_map);
        assert_eq!(cursor.node().kind(), SyntaxKind::SelectStmt);

        assert!(cursor.goto_first_child());
        assert_eq!(cursor.node().kind(), SyntaxKind::SELECT);
    }

    #[test]
    fn goto_first_child_from_token() {
        let src = "select a, b, c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());
        let column_id_node = root
            .descendants()
            .find(|x| x.kind() == SyntaxKind::ColId)
            .unwrap();

        let mut cursor = as_tree_sitter_cursor(&src, column_id_node, range_map);
        cursor.goto_first_child();
        assert_eq!(cursor.node().kind(), SyntaxKind::IDENT);

        assert!(!cursor.goto_first_child());
        assert_eq!(cursor.node().kind(), SyntaxKind::IDENT);
    }

    #[test]
    fn goto_parent_from_root() {
        let src = "select a, b, c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());

        let mut cursor = as_tree_sitter_cursor(src, &root, range_map);

        assert_eq!(cursor.node().kind(), SyntaxKind::Root);
        assert!(!cursor.goto_parent());
        assert_eq!(cursor.node().kind(), SyntaxKind::Root);
    }

    #[test]
    fn goto_parent_from_node() {
        let src = "select a, b, c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());

        let target_element = root
            .descendants()
            .find(|x| x.kind() == SyntaxKind::target_el)
            .unwrap();
        let mut cursor = as_tree_sitter_cursor(src, &target_element, range_map);
        assert_eq!(cursor.node().kind(), SyntaxKind::target_el);

        assert!(cursor.goto_parent());
        assert_eq!(cursor.node().kind(), SyntaxKind::target_list);
    }

    #[test]
    fn goto_parent_from_token() {
        let src = "select a, b, c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());

        let column_id_node = root
            .descendants()
            .find(|x| x.kind() == SyntaxKind::ColId)
            .unwrap();
        let mut cursor = as_tree_sitter_cursor(src, &column_id_node, range_map);

        cursor.goto_first_child();
        assert_eq!(cursor.node().kind(), SyntaxKind::IDENT);

        assert!(cursor.goto_parent());
        assert_eq!(cursor.node().kind(), SyntaxKind::ColId);
    }

    #[test]
    fn goto_next_sibling() {
        let src = "select a,b,c from tbl;";
        let (root, range_map) = get_ts_tree_and_range_map(&src, &parse(&src).unwrap());

        let target_element = root
            .descendants()
            .find(|x| x.kind() == SyntaxKind::target_el)
            .unwrap();
        let mut cursor = as_tree_sitter_cursor(src, &target_element, range_map);
        //
        // - target_list
        //   - target_el (1)
        //   - Comma ","
        //   - target_el (2)
        //   - Comma ","
        //   - target_el (3)
        //

        // 1
        assert_eq!(cursor.node().kind(), SyntaxKind::target_el);

        assert!(cursor.goto_next_sibling());
        assert_eq!(cursor.node().kind(), SyntaxKind::Comma);

        // 2
        assert!(cursor.goto_next_sibling());
        assert_eq!(cursor.node().kind(), SyntaxKind::target_el);

        assert!(cursor.goto_next_sibling());
        assert_eq!(cursor.node().kind(), SyntaxKind::Comma);

        // 3
        assert!(cursor.goto_next_sibling());
        assert_eq!(cursor.node().kind(), SyntaxKind::target_el);

        // No more siblings
        assert!(!cursor.goto_next_sibling());
        assert_eq!(cursor.node().kind(), SyntaxKind::target_el);
    }

    #[test]
    fn range() {
        let src = r#"
-- comment
SELECT
	1 as X
,	2 -- comment 2
,	3
FROM
	A
,	B"#;

        let node = parse(&src).unwrap();
        let (node, range_map) = get_ts_tree_and_range_map(&src, &node);

        let mut cursor = as_tree_sitter_cursor(&src, &node, range_map);
        let mut text_buf = String::from("\n");

        'traverse: loop {
            if cursor.node().child_count() == 0 {
                text_buf.push_str(&format!("{}\n", cursor.node().range()));
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

        let expected = r#"
[(1, 0)-(1, 10)]
[(2, 0)-(2, 6)]
[(3, 1)-(3, 2)]
[(3, 3)-(3, 5)]
[(3, 6)-(3, 7)]
[(4, 0)-(4, 1)]
[(4, 2)-(4, 3)]
[(4, 4)-(4, 16)]
[(5, 0)-(5, 1)]
[(5, 2)-(5, 3)]
[(6, 0)-(6, 4)]
[(7, 1)-(7, 2)]
[(8, 0)-(8, 1)]
[(8, 2)-(8, 3)]
"#;

        assert_eq!(text_buf, expected);
    }

    #[test]
    fn texts() {
        let src = r#"
-- comment
SELECT
	1 as X
,	2 -- comment 2
,	3
FROM
	A
,	B"#;

        let node = parse(&src).unwrap();
        let (node, range_map) = get_ts_tree_and_range_map(&src, &node);

        let mut cursor = as_tree_sitter_cursor(&src, &node, range_map);
        let mut text_buf = Vec::new();

        'traverse: loop {
            if cursor.node().child_count() == 0 {
                text_buf.push(cursor.node().text());
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

        let mut text_buf = text_buf.iter();
        assert_eq!(text_buf.next(), Some(&"-- comment"));
        assert_eq!(text_buf.next(), Some(&"SELECT"));
        assert_eq!(text_buf.next(), Some(&"1"));
        assert_eq!(text_buf.next(), Some(&"as"));
        assert_eq!(text_buf.next(), Some(&"X"));
        assert_eq!(text_buf.next(), Some(&","));
        assert_eq!(text_buf.next(), Some(&"2"));
        assert_eq!(text_buf.next(), Some(&"-- comment 2"));
        assert_eq!(text_buf.next(), Some(&","));
        assert_eq!(text_buf.next(), Some(&"3"));
        assert_eq!(text_buf.next(), Some(&"FROM"));
        assert_eq!(text_buf.next(), Some(&"A"));
        assert_eq!(text_buf.next(), Some(&","));
        assert_eq!(text_buf.next(), Some(&"B"));
        assert_eq!(text_buf.next(), None);
    }
}
