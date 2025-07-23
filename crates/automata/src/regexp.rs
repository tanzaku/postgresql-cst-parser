// const EOF: &'static str = "<<EOF>>";
const EOF_MARKER: [u8; 7] = [b'<', b'<', b'E', b'O', b'F', b'>', b'>'];

/// Abstract Syntax Tree node for regular expression patterns.
/// Represents different components that can appear in a regex pattern.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegexpNode {
    Char(u8),
    AnyOf(Vec<u8>),
    NoneOf(Vec<u8>),
    Concatenation(Vec<RegexpNode>),
    Alternative(Vec<RegexpNode>),
    ZeroOrMore(Box<RegexpNode>),
    OneOrMore(Box<RegexpNode>),
    Repeat(Box<RegexpNode>, usize, usize),
    Reference(String),
}

impl RegexpNode {
    fn as_char_value(&self) -> u8 {
        match self {
            RegexpNode::Char(c) => *c,
            _ => panic!("Called as_char_value on non-Char node"),
        }
    }

    fn concat(nodes: Vec<RegexpNode>) -> RegexpNode {
        if nodes.len() == 1 {
            nodes.into_iter().next().unwrap()
        } else {
            let flatten = nodes
                .into_iter()
                .flat_map(|node| {
                    if let RegexpNode::Concatenation(nodes) = node {
                        nodes
                    } else {
                        vec![node]
                    }
                })
                .collect();

            RegexpNode::Concatenation(flatten)
        }
    }

    fn alternate(nodes: Vec<RegexpNode>) -> RegexpNode {
        if nodes.len() == 1 {
            nodes.into_iter().next().unwrap()
        } else {
            RegexpNode::Alternative(nodes)
        }
    }
}

#[derive(Debug)]
pub struct RegexpParser {
    index: usize,
    bytes: Vec<u8>,
    parsing_context_stack: Vec<Vec<RegexpNode>>,
}

impl RegexpParser {
    pub fn new() -> Self {
        RegexpParser {
            index: 0,
            bytes: Vec::new(),
            parsing_context_stack: vec![],
        }
    }

    fn pop_node(&mut self) -> RegexpNode {
        self.parsing_context_stack
            .last_mut()
            .unwrap()
            .pop()
            .unwrap()
    }

    fn parse_escape_sequence_as_byte(&mut self) -> u8 {
        self.index += 1;

        let c = self.bytes[self.index];
        self.index += 1;

        match c {
            b'b' => b'\x08',
            b'f' => b'\x0c',
            b't' => b'\t',
            b'r' => b'\r',
            b'n' => b'\n',
            b'v' => b'\x0b',
            b'x' => {
                let mut v = 0;

                while self.index < self.bytes.len() {
                    let c = self.bytes[self.index];

                    if !c.is_ascii_hexdigit() {
                        break;
                    }

                    let d = match c {
                        b'0'..=b'9' => c - b'0',
                        b'a'..=b'f' => c - b'a' + 10,
                        b'A'..=b'F' => c - b'A' + 10,
                        _ => unreachable!(),
                    };

                    v = v * 16 + d;
                    self.index += 1;
                }

                v
            }
            // If no format specified, treat as octal number
            b'0'..=b'9' => {
                let mut v = c - b'0';

                while self.index < self.bytes.len() {
                    let c = self.bytes[self.index];

                    if !c.is_ascii_digit() {
                        break;
                    }

                    v = v * 8 + (c - b'0');
                    self.index += 1;
                }

                v
            }
            _ => c,
        }
    }

    fn parse_escape_sequence_as_node(&mut self) -> RegexpNode {
        RegexpNode::Char(self.parse_escape_sequence_as_byte())
    }

    /// Parses a character class expression like [a-z0-9]
    /// Can handle ranges, escapes and negation ([^...])
    fn parse_character_class_to_node(&mut self) -> RegexpNode {
        let mut nodes: Vec<RegexpNode> = Vec::new();

        self.index += 1;

        let is_not = if self.bytes[self.index] == b'^' {
            self.index += 1;
            true
        } else {
            false
        };

        let mut is_minus = false;
        while self.index < self.bytes.len() {
            let c = self.bytes[self.index];

            let prev_is_minus = is_minus;
            is_minus = false;

            match c {
                b']' => {
                    let bs = nodes
                        .into_iter()
                        .map(|node| match node {
                            RegexpNode::Char(c) => c,
                            _ => panic!(),
                        })
                        .collect::<Vec<_>>();

                    let node: RegexpNode = if is_not {
                        RegexpNode::NoneOf(bs)
                    } else {
                        RegexpNode::AnyOf(bs)
                    };

                    self.index += 1;
                    return node;
                }
                b'\\' => nodes.push(self.parse_escape_sequence_as_node()),
                _ => {
                    self.index += 1;
                    is_minus = c == b'-';
                    nodes.push(RegexpNode::Char(c))
                }
            }

            if prev_is_minus && nodes.len() >= 3 {
                let last = nodes.pop().unwrap().as_char_value();
                nodes.pop();
                let first = nodes.pop().unwrap().as_char_value();

                for i in first..=last {
                    nodes.push(RegexpNode::Char(i));
                }
            }
        }

        unreachable!()
    }

    fn parse_quoted_string(&mut self) -> RegexpNode {
        let mut nodes = Vec::new();
        self.index += 1;
        loop {
            let c = self.bytes[self.index];
            self.index += 1;

            if c == b'"' {
                break;
            }

            nodes.push(RegexpNode::Char(c));
        }

        RegexpNode::concat(nodes)
    }

    fn parse_ref_or_qualifier(&mut self) -> RegexpNode {
        self.index += 1;

        let mut content = Vec::new();

        while self.index < self.bytes.len() {
            let c = self.bytes[self.index];

            if c == b'}' {
                self.index += 1;
                break;
            }

            if c == b'\\' {
                content.push(self.parse_escape_sequence_as_byte());
            } else {
                self.index += 1;
                content.push(c);
            }
        }

        // If a minimum repetition count is specified
        if let Some(i) = content.iter().position(|&b| b == b',') {
            let last = self.pop_node();

            let min_str = std::str::from_utf8(&content[0..i]).unwrap();
            let max_str = std::str::from_utf8(&content[i + 1..]).unwrap();

            let min = min_str.parse().unwrap();
            let max = max_str.parse().unwrap();
            return RegexpNode::Repeat(last.into(), min, max);
        }

        // If this is a repetition count (all digits)
        if content.iter().all(|&b| b.is_ascii_digit()) {
            let last = self.pop_node();
            let val_str = std::str::from_utf8(&content).unwrap();
            let val = val_str.parse().unwrap();
            return RegexpNode::Repeat(last.into(), val, val);
        }

        // Otherwise, treat as a reference to another pattern
        let s = std::str::from_utf8(&content).unwrap();
        RegexpNode::Reference(s.to_string())
    }

    fn parse_group(&mut self) -> RegexpNode {
        assert_eq!(self.bytes[self.index], b'(');
        self.index += 1;

        let node = self.parse_alternative();

        assert_eq!(self.bytes[self.index], b')');
        self.index += 1;

        node
    }

    fn parse_single_node(&mut self) -> RegexpNode {
        if self.bytes[self.index..].starts_with(&EOF_MARKER) {
            self.index += EOF_MARKER.len();
            return RegexpNode::Char(0);
        }

        let c = self.bytes[self.index];

        match c {
            b'[' => self.parse_character_class_to_node(),
            b'(' => self.parse_group(),
            b'{' => self.parse_ref_or_qualifier(),
            b'\\' => self.parse_escape_sequence_as_node(),
            b'"' => self.parse_quoted_string(),
            b'*' => {
                self.index += 1;
                let last = self.pop_node();
                RegexpNode::ZeroOrMore(last.into())
            }
            b'+' => {
                self.index += 1;
                let last = self.pop_node();
                RegexpNode::OneOrMore(last.into())
            }
            b'?' => {
                self.index += 1;
                let last = self.pop_node();
                RegexpNode::Repeat(Box::new(last), 0, 1)
            }
            b'.' => {
                self.index += 1;
                RegexpNode::NoneOf(Vec::new())
            }
            b']' | b')' | b'}' => unreachable!(),
            _ => {
                self.index += 1;
                RegexpNode::Char(c)
            }
        }
    }

    fn parse_alternative(&mut self) -> RegexpNode {
        let mut alt_nodes: Vec<RegexpNode> = vec![];
        self.parsing_context_stack.push(vec![]);

        while self.index < self.bytes.len() {
            let c = self.bytes[self.index];

            match c {
                b')' | b']' | b'}' => {
                    break;
                }
                b'|' => {
                    self.index += 1;
                    let nodes = self.parsing_context_stack.pop().unwrap();
                    alt_nodes.push(RegexpNode::concat(nodes));
                    self.parsing_context_stack.push(vec![]);
                }
                _ => {
                    let node = self.parse_single_node();
                    self.parsing_context_stack.last_mut().unwrap().push(node);
                }
            }
        }

        let nodes = self.parsing_context_stack.pop().unwrap();
        if !nodes.is_empty() {
            alt_nodes.push(RegexpNode::concat(nodes));
        }

        RegexpNode::alternate(alt_nodes)
    }

    pub fn parse(&mut self, p: &str) -> RegexpNode {
        self.index = 0;
        self.bytes = p.as_bytes().to_vec();
        self.parse_alternative()
    }
}

impl Default for RegexpParser {
    fn default() -> Self {
        RegexpParser::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_whitespace_character_class_parsing() {
        let mut parser = RegexpParser::new();
        assert_eq!(
            parser.parse(r#"[ \t\n\r\f\v]"#),
            RegexpNode::AnyOf(vec![b' ', b'\t', b'\n', b'\r', 12, 11])
        );
    }

    #[test]
    fn test_negated_newline_character_class() {
        let mut parser = RegexpParser::new();
        assert_eq!(
            parser.parse(r#"[^\n\r]"#),
            RegexpNode::NoneOf(vec![b'\n', b'\r'])
        );
    }

    #[test]
    fn test_comment_pattern() {
        let mut parser = RegexpParser::new();
        assert_eq!(
            parser.parse(r#"("--"{non_newline}*)"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::Char(b'-'),
                RegexpNode::Char(b'-'),
                RegexpNode::ZeroOrMore(Box::new(RegexpNode::Reference(String::from(
                    "non_newline"
                )))),
            ])
        );
    }

    #[test]
    fn test_hexadecimal_character_class() {
        let mut parser = RegexpParser::new();
        assert_eq!(
            parser.parse(r#"[0-9A-Fa-f]"#),
            RegexpNode::AnyOf(vec![
                48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70, 97, 98, 99, 100,
                101, 102
            ])
        );
    }

    #[test]
    fn test_hex_literal_pattern() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"0[xX](_?{hexdigit})+"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::Char(48),
                RegexpNode::AnyOf(vec![120, 88]),
                RegexpNode::OneOrMore(
                    RegexpNode::Concatenation(vec![
                        RegexpNode::Repeat(RegexpNode::Char(b'_').into(), 0, 1),
                        RegexpNode::Reference(String::from("hexdigit")),
                    ])
                    .into()
                )
            ])
        );
    }

    #[test]
    fn test_exponential_notation_pattern() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"({decinteger}|{numeric})[Ee][-+]?{decinteger}"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::alternate(vec![
                    RegexpNode::Reference(String::from("decinteger")),
                    RegexpNode::Reference(String::from("numeric")),
                ]),
                RegexpNode::AnyOf(vec![69, 101]),
                RegexpNode::Repeat(Box::new(RegexpNode::AnyOf(vec![b'-', b'+'])), 0, 1),
                RegexpNode::Reference(String::from("decinteger")),
            ])
        );
    }

    #[test]
    fn test_quoted_string_escape_pattern() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[^\\']+"#),
            RegexpNode::OneOrMore(RegexpNode::NoneOf(vec![b'\\', b'\'']).into())
        );
    }

    #[test]
    fn test_hex_escape_sequence_pattern() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[\\]x[0-9A-Fa-f]{1,2}"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::AnyOf(vec![b'\\']),
                RegexpNode::Char(b'x'),
                RegexpNode::Repeat(
                    Box::new(RegexpNode::AnyOf(vec![
                        48, 49, 50, 51, 52, 53, 54, 55, 56, 57, 65, 66, 67, 68, 69, 70, 97, 98, 99,
                        100, 101, 102
                    ])),
                    1,
                    2
                )
            ])
        );
    }

    #[test]
    fn test_identifier_character_class() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[A-Za-z\x80-\xFF_]"#),
            RegexpNode::AnyOf(vec![
                65, 66, 67, 68, 69, 70, 71, 72, 73, 74, 75, 76, 77, 78, 79, 80, 81, 82, 83, 84, 85,
                86, 87, 88, 89, 90, 97, 98, 99, 100, 101, 102, 103, 104, 105, 106, 107, 108, 109,
                110, 111, 112, 113, 114, 115, 116, 117, 118, 119, 120, 121, 122, 128, 129, 130,
                131, 132, 133, 134, 135, 136, 137, 138, 139, 140, 141, 142, 143, 144, 145, 146,
                147, 148, 149, 150, 151, 152, 153, 154, 155, 156, 157, 158, 159, 160, 161, 162,
                163, 164, 165, 166, 167, 168, 169, 170, 171, 172, 173, 174, 175, 176, 177, 178,
                179, 180, 181, 182, 183, 184, 185, 186, 187, 188, 189, 190, 191, 192, 193, 194,
                195, 196, 197, 198, 199, 200, 201, 202, 203, 204, 205, 206, 207, 208, 209, 210,
                211, 212, 213, 214, 215, 216, 217, 218, 219, 220, 221, 222, 223, 224, 225, 226,
                227, 228, 229, 230, 231, 232, 233, 234, 235, 236, 237, 238, 239, 240, 241, 242,
                243, 244, 245, 246, 247, 248, 249, 250, 251, 252, 253, 254, 255, 95
            ])
        );
    }

    #[test]
    fn test_escaped_double_quote() {
        let mut parser = RegexpParser::new();

        assert_eq!(parser.parse(r#"\""#), RegexpNode::Char(34));
    }

    #[test]
    fn test_non_quote_repetition() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[^']*"#),
            RegexpNode::ZeroOrMore(RegexpNode::NoneOf(vec![39]).into())
        );
    }

    #[test]
    fn test_escaped_operator_character_class() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[\+\-\*]"#),
            RegexpNode::AnyOf(vec![b'+', b'-', b'*'])
        );
    }
}
