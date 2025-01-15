use postgresql_cst_parser::{
    ts_parse,
    tree_sitter::TreeCursor,
};

fn main() {
    let src = r#"
-- comment
SELECT
	1 as X
,	2 -- comment
,	3
FROM
	A
,	B
;
select
    1
,   2
;

"#;

    let tree = ts_parse(&src).unwrap();
    let root = tree.root_node();
    let mut cursor = root.walk();

    visit(&mut cursor, 0, &src);
}

const UNIT: usize = 2;
fn visit(cursor: &mut TreeCursor, depth: usize, src: &str) {
    (0..(depth * UNIT)).for_each(|_| print!("-"));

    print!("{}", cursor.node().kind());

    if cursor.node().child_count() == 0 {
        print!(" \"{}\"", cursor.node().text().escape_debug());
    }
    println!(" {}", cursor.node().range());

    if cursor.goto_first_child() {
        visit(cursor, depth + 1, src);
        while cursor.goto_next_sibling() {
            visit(cursor, depth + 1, src);
        }
        cursor.goto_parent();
    }
}
