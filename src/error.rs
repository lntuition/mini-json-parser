use crate::position::Position;
use std::fmt;

#[derive(Debug, PartialEq)]
pub enum ErrorInfo {
    NotStringClosed,
    NotAllowedControlChar,
    UnrecognizableEscapedChar,
    NotHexDigitChar,

    NotExpectCharGiven(char),

    UnreachablePoint(usize),
    Custom(String),
}

impl fmt::Display for ErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorInfo::NotStringClosed => {
                write!(f, "Found end of JSON, until string not closed")
            }
            ErrorInfo::NotAllowedControlChar => {
                write!(f, "Only non-control character is allowed")
            }
            ErrorInfo::UnrecognizableEscapedChar => {
                write!(f, "Unrecognizable escaped character was given")
            }
            ErrorInfo::NotExpectCharGiven(ref expect) => {
                write!(f, "Expect character({}) is not given", expect)
            }
            ErrorInfo::NotHexDigitChar => {
                write!(f, "Only hex digit(0..=F) character is allowed")
            }
            ErrorInfo::UnreachablePoint(ref idx) => {
                write!(f, "Join to unreached point(#{}), please report issue", idx)
            }
            ErrorInfo::Custom(ref msg) => {
                write!(f, "{}", msg)
            }
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub pos: Position,
    pub info: ErrorInfo,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        writeln!(f, "Between {} and {}", self.pos, self.pos)?;
        writeln!(f, "Error : {}", self.info)
    }
}
