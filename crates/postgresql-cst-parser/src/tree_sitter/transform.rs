use cstree::{build::GreenNodeBuilder, syntax::SyntaxNode};

use crate::{syntax_kind::SyntaxKind, PostgreSQLSyntax, ResolvedNode};

/// Function call flow:
///  transform_cst (public)
///    +- walk_and_build 
///      +- flatten 
///      +- remove_exact <- 消した後どうやって復帰するか、簡単に復帰できるかは未検討

pub fn transform_cst(root: &ResolvedNode) -> ResolvedNode {
    let mut builder = GreenNodeBuilder::new();

    builder.start_node(SyntaxKind::Root);
    walk_and_build(&mut builder, root);
    builder.finish_node();

    let (tree, cache) = builder.finish();
    let new_root =
        SyntaxNode::new_root_with_resolver(tree, cache.unwrap().into_interner().unwrap());

    new_root
}

/// CST を走査し、いくつかの Node を書き換える
/// e.g. flatten list node, remove option node
fn walk_and_build(
    builder: &mut GreenNodeBuilder<'static, 'static, PostgreSQLSyntax>,
    node: &ResolvedNode,
) {
    //TODO: transform cst. but for now, just walk
    use cstree::util::NodeOrToken;
    let mut children = node.children_with_tokens();

    while let Some(child) = children.next() {
        match child {
            NodeOrToken::Node(n) => {
                // for debug
                if format!("{}", n.kind()).starts_with("opt_") {
                    println!("Node  (kind: {})", n.kind());
                }

                // thinking: opt_* でマッチして処理するか、match で全列挙するか？
                //     node.kind って Enum か Raw Value しかないから、前方一致で分岐させるのは厳しいか
                //     format!("{}", n.kind()).starts_with("opt_") とはできるけど、これってオーバーヘッドないのか？
                //     tree_sitter::is_flattern_all では列挙してるからそれに従おう

                match n.kind() {
                    // TODO:
                    // SyntaxKind::target_list=> {},

                    // all pattern
                    kind @ _ => {
                        builder.start_node(kind);

                        walk_and_build(builder, n);

                        builder.finish_node();
                    }
                }
            }
            NodeOrToken::Token(t) => {
                // for debug
                // println!(
                //     "Token (kind: {}, text: \"{}\")",
                //     t.kind(),
                //     t.text().escape_debug()
                // );

                builder.token(t.kind(), t.text());
            }
        }
    }
}

#[cfg(test)]
mod tests {
    use crate::{cst, tree_sitter::transform::transform_cst};

    #[test]
    fn compare_new_tree_and_old_tree() {
        let input = r#"
SELECT
	1 as X
,	2
,	3
FROM
	A
,	B"#;
        // dbg!(input);
        let root = cst::parse(input).unwrap();

        let new_root = transform_cst(&root);

        // dbg!(&root);
        // dbg!(&new_root);
        assert_eq!(format!("{root}"), format!("{new_root}")); // 書き換え前なので、同一のTreeになることを確認
    }
}
