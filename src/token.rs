use crate::position::PositionRange;

#[derive(Debug, PartialEq)]
pub enum TokenValue {
    Comma,
    Colon,
    LeftBracket,
    RightBracket,
    LeftBrace,
    RightBrace,

    String(String),
    Number(f64),
    Bool(bool),
    Null,
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub val: TokenValue,
    pub range: PositionRange,
}
