#[derive(Debug, PartialEq)]
pub enum ParserError {
    /// SQL syntax error
    ParseError {
        message: String,
        start_byte_pos: usize,
        end_byte_pos: usize,
    },
    /// Error raised by ereport
    ScanReport(ScanReport),
    /// Error raised by yyerror
    ScanError { message: String },
}

impl ParserError {
    pub fn new_report(message: &str, detail: &str, position: usize) -> Self {
        Self::ScanReport(ScanReport::new(message, detail, position))
    }

    pub fn new_error(message: &str) -> Self {
        Self::ScanError {
            message: message.to_string(),
        }
    }
}

#[derive(Debug, PartialEq)]
pub struct ScanReport {
    pub message: String,
    pub detail: String,
    pub position_in_bytes: usize,
}

impl ScanReport {
    pub fn new(message: &str, detail: &str, position: usize) -> Self {
        Self {
            message: message.to_string(),
            detail: detail.to_string(),
            position_in_bytes: position,
        }
    }
}
