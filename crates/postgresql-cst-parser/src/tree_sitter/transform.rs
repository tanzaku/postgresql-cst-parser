use cstree::{build::GreenNodeBuilder, green::GreenNode, interning::Interner, Syntax};

use crate::{syntax_kind::SyntaxKind, PostgreSQLSyntax, ResolvedNode};

// TODO(refactor): TreeBuilderにしなくても、(ResolvedNode -> ResolvedNode) な純粋関数でいいかも
struct TreeBuilder {
    // lexer がいらない
    builder: GreenNodeBuilder<'static, 'static, PostgreSQLSyntax>,
}

impl TreeBuilder {
    pub fn new() -> Self {
        Self {
            builder: GreenNodeBuilder::new(),
        }
    }

    pub fn build(&mut self, node: &ResolvedNode) -> Result<(), String> {
        self.builder.start_node(SyntaxKind::Root);
        self.walk_and_build(node)?;
        self.builder.finish_node();
        Ok(())
    }

    fn walk_and_build(&mut self, node: &ResolvedNode) -> Result<(), String> {
        // for now, just walk
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
                            self.builder.start_node(kind);

                            self.walk_and_build(n)?;

                            self.builder.finish_node();
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

                    self.builder.token(t.kind(), t.text());
                }
            }
        }
        Ok(())
    }

    pub fn finish(self) -> (GreenNode, impl Interner) {
        let (tree, cache) = self.builder.finish();
        (tree, cache.unwrap().into_interner().unwrap())
    }
}

#[cfg(test)]
mod tests {
    use crate::{cst, tree_sitter::transform::TreeBuilder, SyntaxNode};

    #[test]
    fn test() -> Result<(), String> {
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

        let mut tree_builder = TreeBuilder::new();
        tree_builder.build(&root)?;

        let (tree, interner) = tree_builder.finish();
        let new_root = SyntaxNode::new_root_with_resolver(tree, interner);

        // dbg!(&root);
        // dbg!(&new_root);
        assert_eq!(format!("{root}"), format!("{new_root}"));

        Ok(())
    }
}
