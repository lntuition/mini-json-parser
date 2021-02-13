use crate::position::PositionRange;

#[derive(Debug, PartialEq)]
pub enum TokenValue {
    ValueSeperator,
    NameSeperator,
    BeginArray,
    EndArray,
    BeginObject,
    EndObject,

    Null,
    True,
    False,
    String(String),
    Number(f64),
}

#[derive(Debug, PartialEq)]
pub struct Token {
    pub val: TokenValue,
    pub range: PositionRange,
}
