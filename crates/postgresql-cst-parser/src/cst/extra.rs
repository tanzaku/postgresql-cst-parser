use crate::syntax_kind::SyntaxKind;

#[derive(Debug)]
pub(crate) struct Extra<'a> {
    pub(crate) kind: SyntaxKind,
    pub(crate) start_byte_pos: usize,
    pub(crate) end_byte_pos: usize,
    pub(crate) comment: &'a str,
}
