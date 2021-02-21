use std::collections::HashMap;

use crate::{
    error::Error,
    json::Json,
    position::{Position, PositionRange},
    token::{Token, TokenValue},
};

struct Parser {
    tokens: Vec<Token>,
}

impl Parser {
    pub fn new(tokens: Vec<Token>) -> Self {
        Parser { tokens }
    }

    fn move_next(idx: &mut usize) {
        *idx += 1;
    }

    fn get_cur(&self, idx: &mut usize) -> Result<&TokenValue, &'static str> {
        Ok(&self.tokens.get(*idx).ok_or("unexpected EOF")?.val)
    }

    pub fn parse(&self, idx: &mut usize) -> Result<Json, &'static str> {
        Ok(match self.get_cur(idx)? {
            TokenValue::BeginArray => self.parse_array(idx)?,
            TokenValue::BeginObject => self.parse_object(idx)?,
            _ => self.parse_value(idx)?,
        })
    }

    fn parse_value(&self, idx: &mut usize) -> Result<Json, &'static str> {
        let val = self.get_cur(idx)?;
        Parser::move_next(idx);

        Ok(match val {
            TokenValue::Null => Json::Null,
            TokenValue::True => Json::Bool(true),
            TokenValue::False => Json::Bool(false),
            TokenValue::String(string) => Json::String(string.clone()),
            TokenValue::Number(num) => Json::Number(*num),
            _ => return Err("Unexpected Token"),
        })
    }

    fn parse_array(&self, idx: &mut usize) -> Result<Json, &'static str> {
        if &TokenValue::BeginArray != self.get_cur(idx)? {
            return Err("Unreachable point");
        }
        Parser::move_next(idx);

        let mut v: Vec<Box<Json>> = vec![];
        loop {
            match self.get_cur(idx)? {
                TokenValue::EndArray => {
                    Parser::move_next(idx);
                    break;
                }
                _ => {
                    v.push(Box::new(self.parse(idx)?));
                    match self.get_cur(idx)? {
                        TokenValue::EndArray => continue,
                        TokenValue::ValueSeperator => Parser::move_next(idx),
                        _ => return Err("Require value seperator"),
                    }
                }
            }
        }

        Ok(Json::Array(v))
    }

    fn parse_object(&self, idx: &mut usize) -> Result<Json, &'static str> {
        if &TokenValue::BeginObject != self.get_cur(idx)? {
            return Err("Unreachable point");
        }
        Parser::move_next(idx);

        let mut m: HashMap<String, Box<Json>> = HashMap::new();
        loop {
            match self.get_cur(idx)? {
                TokenValue::EndObject => {
                    Parser::move_next(idx);
                    break;
                }
                TokenValue::String(key) => {
                    Parser::move_next(idx);
                    
                    if &TokenValue::NameSeperator != self.get_cur(idx)? {
                        return Err("Require name speartor");
                    }
                    Parser::move_next(idx);

                    m.insert(key.clone(), Box::new(self.parse(idx)?));

                    match self.get_cur(idx)? {
                        TokenValue::EndObject => continue,
                        TokenValue::ValueSeperator => Parser::move_next(idx),
                        _ => return Err("Require value seperator"),
                    }
                }
                _ => return Err("Unexpected token"),
            }
        }

        Ok(Json::Object(m))
    }
}

#[cfg(test)]
mod tests {
    use super::{Json, Parser, Position, PositionRange, Token, TokenValue};
    use pretty_assertions::assert_eq;

    #[test]
    fn array_test() {
        assert_eq!(
            Parser::new(vec![
                Token {
                    val: TokenValue::BeginArray,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndArray,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
            ])
            .parse(&mut 0),
            Ok(crate::json_array![]),
        );
    }

    #[test]
    fn obj_test() {
        assert_eq!(
            Parser::new(vec![
                Token {
                    val: TokenValue::BeginObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
            ])
            .parse(&mut 0),
            Ok(crate::json_object!{}),
        );
    }

    #[test]
    fn array_nest_test() {
        assert_eq!(
            Parser::new(vec![
                Token {
                    val: TokenValue::BeginArray,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::BeginArray,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::Null,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::ValueSeperator,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::False,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndArray,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndArray,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
            ])
            .parse(&mut 0),
            Ok(crate::json_array![crate::json_array![
                Json::Null,
                Json::Bool(false)
            ]]),
        );
    }

    #[test]
    fn obj_nest_test() {
        assert_eq!(
            Parser::new(vec![
                Token {
                    val: TokenValue::BeginObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::String("hello".to_string()),
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::NameSeperator,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::BeginObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::ValueSeperator,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::String("hi".to_string()),
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::NameSeperator,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::BeginObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
                Token {
                    val: TokenValue::EndObject,
                    range: PositionRange {
                        start: Position { row: 1, col: 1 },
                        end: Position { row: 1, col: 2 },
                    }
                },
            ])
            .parse(&mut 0),
            Ok(crate::json_object!{
                "hi".to_string(); crate::json_object!{},
                "hello".to_string(); crate::json_object!{}
            }),
        );
    }
}