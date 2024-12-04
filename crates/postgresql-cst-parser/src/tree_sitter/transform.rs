use cstree::{build::GreenNodeBuilder, green::GreenNode, interning::Interner};

use crate::{syntax_kind::SyntaxKind, PostgreSQLSyntax, ResolvedNode};

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
                    self.builder.start_node(n.kind());

                    self.walk_and_build(n)?;

                    self.builder.finish_node();
                }
                NodeOrToken::Token(t) => {
                    let kind = t.kind();
                    let text = t.text();
                    println!("Token (kind: {kind:?}, text: {text})");

                    // build
                    self.builder.token(kind, text);
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

        dbg!(&root);
        dbg!(&new_root);
        assert_eq!(format!("{root}"), format!("{new_root}"));

        Ok(())
    }
}
