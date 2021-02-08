use crate::position::Position;
use std::fmt;

#[derive(Debug)]
pub enum ErrorInfo {
    UnexpectedError(usize),
    CustomError(String),
}

impl fmt::Display for ErrorInfo {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match *self {
            ErrorInfo::UnexpectedError(ref idx) => {
                write!(f, "Unexpected error(#{}), please report issue", idx)
            }
            ErrorInfo::CustomError(ref s) => write!(f, "{}", s),
        }
    }
}

#[derive(Debug)]
pub struct Error {
    pub pos: Position,
    pub info: String,
}

impl fmt::Display for Error {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.pos, self.info)
    }
}
