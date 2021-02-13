use crate::position::PositionRange;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ErrorInfo {
    NotProperToken(char),

    UnexpectedEOF,
    NotAllowedControlChar(char),
    NotProperEscapedChar(char),
    NotHexDigit(char),

    NotDecDigit(char),

    NotNullToken,
    NotTrueToken,
    NotFalseToken,

    NotProperHandledPoint(usize),
    UnreachablePoint(usize),
}

impl fmt::Display for ErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorInfo::NotProperToken(ch) => {
                write!(f, "Not proper token first character, '{}'", ch)
            }
            ErrorInfo::UnexpectedEOF => {
                write!(f, "Unexpected EOF character while parsing")
            }
            ErrorInfo::NotAllowedControlChar(ch) => {
                write!(f, "Not allowed control character, '\\u{:04x}'", ch as u32)
            }
            ErrorInfo::NotProperEscapedChar(ch) => {
                write!(f, "Not proper escaped character, '\\{}'", ch)
            }
            ErrorInfo::NotHexDigit(ch) => {
                write!(f, "Not hexadecimal digit(0..=F), '{}'", ch)
            }
            ErrorInfo::NotDecDigit(ch) => {
                write!(f, "Not decimal digit(0..=9), '{}'", ch)
            }
            ErrorInfo::NotNullToken => {
                write!(f, "Not null token, expected 'null'")
            }
            ErrorInfo::NotTrueToken => {
                write!(f, "Not true token, expected 'true'")
            }
            ErrorInfo::NotFalseToken => {
                write!(f, "Not false token, expected 'false'")
            }
            ErrorInfo::NotProperHandledPoint(id) => {
                write!(f, "Not proper handled point #{}, Please check core", id)
            }
            ErrorInfo::UnreachablePoint(id) => {
                write!(f, "Unreachable point #{}, Please report issue", id)
            }
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub info: ErrorInfo,
    pub range: PositionRange,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} - {}", self.range, self.info)
    }
}
