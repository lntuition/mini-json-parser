use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq)]
pub enum Error {
    // TODO : Consider add reason to unexpected character
    UnexpectedChar { ch: char, row: usize, col: usize },
    UnexpectedEndOfJson,
    UnexpectedError { msg: String },
}

impl Display for Error {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Error::UnexpectedChar {
                ref ch,
                ref row,
                ref col,
            } => {
                write!(
                    f,
                    "Unexpected character '{}' at ln {} : col {}",
                    ch, row, col
                )
            }
            Error::UnexpectedEndOfJson => {
                write!(f, "Unexpected end of JSON")
            }
            Error::UnexpectedError { ref msg } => {
                write!(f, "Unexpected error : {}", msg)
            }
        }
    }
}
