// const EOF: &'static str = "<<EOF>>";
const EOF_CHARS: [char; 7] = ['<', '<', 'E', 'O', 'F', '>', '>'];

#[derive(Debug, Clone, PartialEq, Eq)]
pub enum RegexpNode {
    Char(u8),
    Concatenation(Vec<RegexpNode>),
    AnyOf(Vec<u8>),
    NoneOf(Vec<u8>),
    Alternative(Vec<RegexpNode>),
    Kleene0(Box<RegexpNode>),
    Kleene1(Box<RegexpNode>),
    Repeat(Box<RegexpNode>, usize, usize),
    RefAutomata(String),
}

impl RegexpNode {
    fn as_u8(&self) -> u8 {
        match self {
            RegexpNode::Char(c) => *c,
            _ => panic!(),
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
    chars: Vec<char>,
    parsed_nodes_stack: Vec<Vec<RegexpNode>>,
}

impl RegexpParser {
    pub fn new() -> Self {
        RegexpParser {
            index: 0,
            chars: Vec::new(),
            parsed_nodes_stack: vec![],
        }
    }

    fn pop_node(&mut self) -> RegexpNode {
        self.parsed_nodes_stack.last_mut().unwrap().pop().unwrap()
    }

    // dolq_start		[A-Za-z\200-\377_]
    // dolq_cont		[A-Za-z\200-\377_0-9]
    // dolqdelim		\$({dolq_start}{dolq_cont}*)?\$
    // dolqfailed		\${dolq_start}{dolq_cont}*
    // dolqinside		[^$]+
    // xeinside		[^\\']+
    // xeescape		[\\][^0-7]
    // xqinside		[^']+
    fn read_escaped_byte(&mut self) -> u8 {
        self.index += 1;

        let c = self.chars[self.index];
        self.index += 1;

        match c {
            'b' => b'\x08',
            'f' => b'\x0c',
            't' => b'\t',
            'r' => b'\r',
            'n' => b'\n',
            'v' => b'\x0b',
            'x' => {
                let mut v = 0;

                while self.index < self.chars.len() {
                    let c = self.chars[self.index];

                    if !c.is_ascii_hexdigit() {
                        break;
                    }

                    let d = match c {
                        '0'..='9' => c as u8 - b'0',
                        'a'..='f' => c as u8 - b'a' + 10,
                        'A'..='F' => c as u8 - b'A' + 10,
                        _ => unreachable!(),
                    };

                    v = v * 16 + d;
                    self.index += 1;
                }

                v
            }
            // 未指定は8進数
            '0'..='9' => {
                let mut v = c as u8 - b'0';

                while self.index < self.chars.len() {
                    let c = self.chars[self.index];

                    if !c.is_ascii_digit() {
                        break;
                    }

                    v = v * 8 + (c as u8 - b'0');
                    self.index += 1;
                }

                v
            }
            _ => c as u8,
        }
    }

    fn read_escaped_node(&mut self) -> RegexpNode {
        RegexpNode::Char(self.read_escaped_byte())
    }

    fn parse_character_class(&mut self) -> RegexpNode {
        let mut nodes: Vec<RegexpNode> = Vec::new();

        self.index += 1;

        let is_not = if self.chars[self.index] == '^' {
            self.index += 1;
            true
        } else {
            false
        };

        let mut is_minus = false;
        while self.index < self.chars.len() {
            let c = self.chars[self.index];

            let prev_is_minus = is_minus;
            is_minus = false;

            match c {
                ']' => {
                    let bs = nodes
                        .into_iter()
                        .map(|node| match node {
                            RegexpNode::Char(c) => c as u8,
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
                '\\' => nodes.push(self.read_escaped_node()),
                _ => {
                    self.index += 1;
                    is_minus = c == '-';
                    nodes.push(RegexpNode::Char(c as u8))
                }
            }

            if prev_is_minus && nodes.len() >= 3 {
                let last = nodes.pop().unwrap().as_u8();
                nodes.pop();
                let first = nodes.pop().unwrap().as_u8();

                for i in first..=last {
                    nodes.push(RegexpNode::Char(i));
                }
            }
        }

        unreachable!()
    }

    // fn parse_alternative(p: &str) -> RegexpNode {
    //     let mut stack: Vec<Vec<RegexpNode>> = vec![Vec::new()];
    //     let chars: Vec<_> = p.chars().collect();
    //     let mut i = 0;

    //     while i < chars.len() {
    //         let c = chars[i];

    //         match c {
    //             '(' => {
    //                 stack.push(Vec::new());
    //             }
    //             ')' => {
    //                 let nodes = stack.pop().unwrap();
    //                 stack
    //                     .last_mut()
    //                     .unwrap()
    //                     .push(RegexpNode::Alternative(nodes));
    //             }
    //             '|' => {
    //                 let nodes = stack.pop().unwrap();
    //                 stack
    //                     .last_mut()
    //                     .unwrap()
    //                     .push(RegexpNode::Alternative(nodes));
    //             }
    //             _ => stack.push(parse_regexp(p)),
    //         }

    //         i += 1;
    //     }

    //     dbg!(&stack);
    //     assert_eq!(stack.len(), 1);
    //     let nodes = stack.pop().unwrap();
    //     if nodes.len() == 1 {
    //         nodes.into_iter().next().unwrap()
    //     } else {
    //         RegexpNode::Concatenation(nodes)
    //     }
    // }

    fn parse_double_quote(&mut self) -> RegexpNode {
        let mut nodes = Vec::new();
        self.index += 1;
        loop {
            let c = self.chars[self.index];
            self.index += 1;

            if c == '"' {
                break;
            }

            nodes.push(RegexpNode::Char(c as u8));
        }

        RegexpNode::concat(nodes)
    }

    fn parse_ref_or_qualifier(&mut self) -> RegexpNode {
        self.index += 1;

        let mut s = String::new();

        while self.index < self.chars.len() {
            let c = self.chars[self.index];

            if c == '}' {
                self.index += 1;
                break;
            }

            if c == '\\' {
                s.push(self.read_escaped_byte() as char);
            } else {
                self.index += 1;
                s.push(c);
            }
        }

        if let Some(i) = s.find(',') {
            let (l, r) = s.split_at(i);
            let last = self.pop_node();

            let min = l.parse().unwrap();
            let max = r[1..].parse().unwrap();
            return RegexpNode::Repeat(last.into(), min, max);
        }

        if s.chars().all(|c| c.is_ascii_digit()) {
            let last = self.pop_node();

            let val = s.parse().unwrap();
            return RegexpNode::Repeat(last.into(), val, val);
        }

        RegexpNode::RefAutomata(s)

        // self.parse_alternative();
        // while self.index < self.chars.len() {
        //     let c = self.chars[self.index];

        //     match c {
        //         ')' | ']' | '}' => {
        //             self.index += 1;
        //             break;
        //         }
        //         '|' => {
        //             self.index += 1;
        //             alt_nodes.push(RegexpNode::concat(self.parsed_nodes.clone()));
        //             self.parsed_nodes.clear();
        //         }
        //         _ => {
        //             let node = self.parse_single_node();
        //             self.parsed_nodes.push(node);
        //         }
        //     }
        // }

        // let nodes = stack.pop().unwrap();
        // let s = nodes
        //     .into_iter()
        //     .map(|node| match node {
        //         RegexpNode::Char(c) => c as char,
        //         _ => panic!(),
        //     })
        //     .collect::<String>();

        // if let Some((min, max)) = s.split_once(',') {
        //     let min = min.parse::<usize>().unwrap();
        //     let max = max.parse::<usize>().unwrap();

        //     let last = stack.last_mut().unwrap().pop().unwrap();
        //     stack
        //         .last_mut()
        //         .unwrap()
        //         .push(RegexpNode::Repeat(Box::new(last), min, max));
        // } else if s.chars().all(|c| c.is_ascii_digit()) {
        //     let n = s.parse::<usize>().unwrap();
        //     let last = stack.last_mut().unwrap().pop().unwrap();
        //     stack
        //         .last_mut()
        //         .unwrap()
        //         .push(RegexpNode::Repeat(Box::new(last), n, n));
        // } else {
        //     stack.last_mut().unwrap().push(RegexpNode::RefAutomata(s));
        // }
    }

    // fn parse_regexp(p: &str) -> RegexpNode {
    //     let mut stack: Vec<Vec<RegexpNode>> = vec![Vec::new()];
    //     let chars: Vec<_> = p.chars().collect();
    //     let mut i = 0;

    //     while i < chars.len() {
    //         let c = chars[i];

    //         match c {
    //             '[' => {
    //                 let node;
    //                 (node, i) = parse_regexp_paren1(&chars, i);
    //                 stack.last_mut().unwrap().push(node);
    //             }
    //             ']' => unreachable!(),
    //             '(' => stack.last_mut().unwrap().push(parse_alternative(p)),
    //             ')' => unreachable!(),
    //             '|' => {
    //                 let nodes = stack.pop().unwrap();
    //                 stack
    //                     .last_mut()
    //                     .unwrap()
    //                     .push(RegexpNode::Alternative(nodes));
    //             }
    //             '{' => {
    //                 stack.push(Vec::new());
    //             }
    //             '}' => {
    //                 let nodes = stack.pop().unwrap();
    //                 let s = nodes
    //                     .into_iter()
    //                     .map(|node| match node {
    //                         RegexpNode::Char(c) => c as char,
    //                         _ => panic!(),
    //                     })
    //                     .collect::<String>();

    //                 if let Some((min, max)) = s.split_once(',') {
    //                     let min = min.parse::<usize>().unwrap();
    //                     let max = max.parse::<usize>().unwrap();

    //                     let last = stack.last_mut().unwrap().pop().unwrap();
    //                     stack.last_mut().unwrap().push(RegexpNode::Repeat(
    //                         Box::new(last),
    //                         min,
    //                         max,
    //                     ));
    //                 } else if s.chars().all(|c| c.is_ascii_digit()) {
    //                     let n = s.parse::<usize>().unwrap();
    //                     let last = stack.last_mut().unwrap().pop().unwrap();
    //                     stack
    //                         .last_mut()
    //                         .unwrap()
    //                         .push(RegexpNode::Repeat(Box::new(last), n, n));
    //                 } else {
    //                     stack.last_mut().unwrap().push(RegexpNode::RefAutomata(s));
    //                 }
    //             }
    //             '\\' => stack.last_mut().unwrap().push(read_escaped(&chars, &mut i)),
    //             '"' => {
    //                 i += 1;
    //                 while chars[i] != '"' {
    //                     let c = chars[i];
    //                     stack.last_mut().unwrap().push(RegexpNode::Char(c as u8));
    //                     i += 1;
    //                 }
    //             }
    //             '*' => {
    //                 let last = stack.last_mut().unwrap().pop().unwrap();
    //                 stack
    //                     .last_mut()
    //                     .unwrap()
    //                     .push(RegexpNode::Kleene0(last.into()));
    //             }
    //             '+' => {
    //                 let last = stack.last_mut().unwrap().pop().unwrap();
    //                 stack
    //                     .last_mut()
    //                     .unwrap()
    //                     .push(RegexpNode::Kleene1(last.into()));
    //             }
    //             '?' => {
    //                 let last = stack.last_mut().unwrap().pop().unwrap();
    //                 stack
    //                     .last_mut()
    //                     .unwrap()
    //                     .push(RegexpNode::Repeat(Box::new(last), 0, 1));
    //             }
    //             _ => {
    //                 stack.last_mut().unwrap().push(RegexpNode::Char(c as u8));
    //             }
    //         }

    //         i += 1;
    //     }

    //     dbg!(&stack);
    //     assert_eq!(stack.len(), 1);
    //     let nodes = stack.pop().unwrap();
    //     if nodes.len() == 1 {
    //         nodes.into_iter().next().unwrap()
    //     } else {
    //         RegexpNode::Concatenation(nodes)
    //     }
    // }

    fn parse_group(&mut self) -> RegexpNode {
        assert_eq!(self.chars[self.index], '(');
        self.index += 1;

        let node = self.parse_alternative();

        assert_eq!(self.chars[self.index], ')');
        self.index += 1;

        node
    }

    fn parse_single_node(&mut self) -> RegexpNode {
        if self.chars[self.index..].starts_with(&EOF_CHARS) {
            self.index += EOF_CHARS.len();
            return RegexpNode::Char(0);
        }

        let c = self.chars[self.index];

        match c {
            '[' => self.parse_character_class(),
            '(' => self.parse_group(),
            '{' => self.parse_ref_or_qualifier(),
            '\\' => self.read_escaped_node(),
            '"' => self.parse_double_quote(),
            '*' => {
                self.index += 1;
                let last = self.pop_node();
                RegexpNode::Kleene0(last.into())
            }
            '+' => {
                self.index += 1;
                let last = self.pop_node();
                RegexpNode::Kleene1(last.into())
            }
            '?' => {
                self.index += 1;
                let last = self.pop_node();
                RegexpNode::Repeat(Box::new(last), 0, 1)
            }
            '.' => {
                self.index += 1;
                RegexpNode::NoneOf(Vec::new())
            }
            ']' | ')' | '}' => unreachable!(),
            _ => {
                self.index += 1;
                RegexpNode::Char(c as u8)
            }
        }
    }

    fn parse_alternative(&mut self) -> RegexpNode {
        let mut alt_nodes: Vec<RegexpNode> = vec![];
        self.parsed_nodes_stack.push(vec![]);

        while self.index < self.chars.len() {
            let c = self.chars[self.index];

            match c {
                ')' | ']' | '}' => {
                    break;
                }
                '|' => {
                    self.index += 1;
                    let nodes = self.parsed_nodes_stack.pop().unwrap();
                    alt_nodes.push(RegexpNode::concat(nodes));
                    self.parsed_nodes_stack.push(vec![]);
                }
                _ => {
                    let node = self.parse_single_node();
                    self.parsed_nodes_stack.last_mut().unwrap().push(node);
                }
            }
        }

        let nodes = self.parsed_nodes_stack.pop().unwrap();
        if !nodes.is_empty() {
            alt_nodes.push(RegexpNode::concat(nodes));
        }

        RegexpNode::alternate(alt_nodes)
    }

    pub fn parse(&mut self, p: &str) -> RegexpNode {
        self.index = 0;
        self.chars = p.chars().collect();
        self.parse_alternative()
    }
}

// pub fn build_dfa(patterns: Vec<String>) -> DFA {
//     let mut nfa = nfa::NFA::new();

//     for pattern in patterns {
//         let start = nfa.new_state();
//         let end = nfa.new_state();
//         nfa.add_pattern(start, end, pattern);
//     }

//     let dfa = DFA::from_nfa(&nfa);

//     dfa
// }

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_simple1() {
        let mut parser = RegexpParser::new();
        assert_eq!(
            parser.parse(r#"[ \t\n\r\f\v]"#),
            RegexpNode::AnyOf(vec![32, 9, 10, 13, 12, 11])
        );
    }

    #[test]
    fn test_simple2() {
        let mut parser = RegexpParser::new();
        assert_eq!(parser.parse(r#"[^\n\r]"#), RegexpNode::NoneOf(vec![10, 13]));
    }

    #[test]
    fn test_simple3() {
        let mut parser = RegexpParser::new();
        assert_eq!(
            parser.parse(r#"("--"{non_newline}*)"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::Char(b'-'),
                RegexpNode::Char(b'-'),
                RegexpNode::Kleene0(Box::new(RegexpNode::RefAutomata(String::from(
                    "non_newline"
                )))),
            ])
        );
    }

    #[test]
    fn test_simple4() {
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
    fn test_simple5() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"0[xX](_?{hexdigit})+"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::Char(48),
                RegexpNode::AnyOf(vec![120, 88]),
                RegexpNode::Kleene1(
                    RegexpNode::Concatenation(vec![
                        RegexpNode::Repeat(RegexpNode::Char(b'_').into(), 0, 1),
                        RegexpNode::RefAutomata(String::from("hexdigit")),
                    ])
                    .into()
                )
            ])
        );
    }

    #[test]
    fn test_simple6() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"({decinteger}|{numeric})[Ee][-+]?{decinteger}"#),
            RegexpNode::Concatenation(vec![
                RegexpNode::alternate(vec![
                    RegexpNode::RefAutomata(String::from("decinteger")),
                    RegexpNode::RefAutomata(String::from("numeric")),
                ]),
                RegexpNode::AnyOf(vec![69, 101]),
                RegexpNode::Repeat(Box::new(RegexpNode::AnyOf(vec![b'-', b'+'])), 0, 1),
                RegexpNode::RefAutomata(String::from("decinteger")),
            ])
        );
    }

    #[test]
    fn test_simple7() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[^\\']+"#),
            RegexpNode::Kleene1(RegexpNode::NoneOf(vec![b'\\', b'\'']).into())
        );
    }

    #[test]
    fn test_simple8() {
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
    fn test_simple9() {
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
    fn test_simple10() {
        let mut parser = RegexpParser::new();

        assert_eq!(parser.parse(r#"\""#), RegexpNode::Char(34));
    }

    #[test]
    fn test_simple11() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            parser.parse(r#"[^']*"#),
            RegexpNode::Kleene0(RegexpNode::NoneOf(vec![39]).into())
        );
    }

    #[test]
    fn test_simple12() {
        let mut parser = RegexpParser::new();

        assert_eq!(
            // parser.parse(r#"[,()\[\].;\:\+\-\*\/\%\^<>\=]"#),
            // RegexpNode::AnyOf(vec![44, 40, 41, 91, 93, 46, 59, 58, 47, 37, 94, 60, 62, 61])
            parser.parse(r#"[\+\-\*]"#),
            RegexpNode::AnyOf(vec![b'+', b'-', b'*'])
        );
    }
}
