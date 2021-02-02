use crate::position::Position;
use std::fmt;

#[derive(Debug)]
pub struct JsonParseError {
    pub pos: Position,
    pub msg: String,
}

impl fmt::Display for JsonParseError {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{} : {}", self.pos, self.msg)
    }
}
