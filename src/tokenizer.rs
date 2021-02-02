use crate::error::JsonParseError;
use crate::position::Position;
use std::{fmt, str::Chars};

const SPACE: char = ' ';
const TAB: char = '\t';
const CARRIAGE_RETURN: char = '\r';
const LINE_FEED: char = '\n';

const COMMA: char = ',';
const COLON: char = ':';
const LEFT_BRACKET: char = '[';
const RIGHT_BRACKET: char = ']';
const LEFT_BRACE: char = '{';
const RIGHT_BRACE: char = '}';

#[derive(Debug, PartialEq)]
struct Token {
    val: TokenValue,
    pos: Position,
}

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

impl fmt::Display for TokenValue {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            TokenValue::Comma => write!(f, "comma"),
            TokenValue::Colon => write!(f, "colon"),
            TokenValue::LeftBracket => write!(f, "left bracket"),
            TokenValue::RightBracket => write!(f, "right bracket"),
            TokenValue::LeftBrace => write!(f, "left brace"),
            TokenValue::RightBrace => write!(f, "right brace"),
            TokenValue::String(val) => write!(f, "string({})", val),
            TokenValue::Number(val) => write!(f, "number({})", val),
            TokenValue::Bool(true) => write!(f, "bool(true)"),
            TokenValue::Bool(false) => write!(f, "bool(false)"),
            TokenValue::Null => write!(f, "null"),
        }
    }
}

#[derive(Debug)]
struct Tokenizer<'a> {
    chars: Chars<'a>,
    cur: Option<char>,
    pos: Position,
}

impl<'a> Tokenizer<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut chars = s.chars();
        let cur = chars.next();
        let pos = Position { row: 1, col: 1 };

        Tokenizer { chars, cur, pos }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, JsonParseError> {
        let mut tokens: Vec<Token> = vec![];

        while let Some(ch) = self.cur {
            let pos = self.pos;

            self.move_next();
            if ch == SPACE || ch == TAB || ch == CARRIAGE_RETURN || ch == LINE_FEED {
                continue;
            }

            let token = match self.get_token_value(ch) {
                Ok(val) => Token { pos, val },
                Err(msg) => return Err(JsonParseError { pos, msg }),
            };

            tokens.push(token);
        }
        Ok(tokens)
    }

    fn move_next(&mut self) {
        self.cur = self.chars.next();
        if let Some(ch) = self.cur {
            match ch {
                '\n' => self.pos.next_row(),
                _ => self.pos.next_col(),
            }
        }
    }

    fn get_token_value(&mut self, start: char) -> Result<TokenValue, String> {
        match start {
            COMMA => Ok(TokenValue::Comma),
            COLON => Ok(TokenValue::Colon),
            LEFT_BRACE => Ok(TokenValue::LeftBrace),
            RIGHT_BRACE => Ok(TokenValue::RightBrace),
            LEFT_BRACKET => Ok(TokenValue::LeftBracket),
            RIGHT_BRACKET => Ok(TokenValue::RightBracket),
            'n' => self.get_null_token_value(),
            't' => self.get_true_token_value(),
            'f' => self.get_false_token_value(),
            '"' => self.get_string_token_value(),
            '0'..='9' | '-' => self.get_number_token_value(start),
            _ => Err("Failed to get token value".to_string()),
        }
    }

    fn get_null_token_value(&mut self) -> Result<TokenValue, String> {
        self.get_expected_token_value(&['u', 'l', 'l'], TokenValue::Null)
    }

    fn get_true_token_value(&mut self) -> Result<TokenValue, String> {
        self.get_expected_token_value(&['r', 'u', 'e'], TokenValue::Bool(true))
    }

    fn get_false_token_value(&mut self) -> Result<TokenValue, String> {
        self.get_expected_token_value(&['a', 'l', 's', 'e'], TokenValue::Bool(false))
    }

    fn get_string_token_value(&mut self) -> Result<TokenValue, String> {
        let mut val: String = String::new();
        while let Some(ch) = self.cur {
            self.move_next();

            // TODO : check control character
            if ch == '"' {
                return Ok(TokenValue::String(val));
            }
            val.push(ch);
        }

        Err("Failed to process string, not closed".to_string())
    }

    fn get_number_token_value(&mut self, start: char) -> Result<TokenValue, String> {
        let mut val: String = String::from(start);
        while let Some(ch) = self.cur {
            // TODO : check fraction and exponent
            if !ch.is_numeric() {
                break;
            }

            self.move_next();
            val.push(ch);
        }

        match val.parse::<f64>() {
            Ok(num) => Ok(TokenValue::Number(num)),
            Err(_) => Err("Failed to parse number".to_string()),
        }
    }

    fn get_expected_token_value(
        &mut self,
        expected: &[char],
        result: TokenValue,
    ) -> Result<TokenValue, String> {
        for &expect in expected.iter() {
            if self.cur != Some(expect) {
                return Err(format!(
                    "Failed to process {}, '{}' is not given",
                    result, expect
                ));
            }
            self.move_next();
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::{Token, TokenValue, Tokenizer};

    #[test]
    fn get_null_token_value_success() {
        assert_eq!(
            Tokenizer::new("ull").get_null_token_value(),
            Ok(TokenValue::Null)
        )
    }

    #[test]
    fn get_null_token_value_fail() {
        assert_eq!(
            Tokenizer::new("ul").get_null_token_value(),
            Err("Failed to process null, 'l' is not given".to_string())
        )
    }

    #[test]
    fn get_true_token_value_success() {
        assert_eq!(
            Tokenizer::new("rue").get_true_token_value(),
            Ok(TokenValue::Bool(true))
        )
    }

    #[test]
    fn get_true_token_value_fail() {
        assert_eq!(
            Tokenizer::new("rum").get_true_token_value(),
            Err("Failed to process bool(true), 'e' is not given".to_string())
        )
    }

    #[test]
    fn get_false_token_value_success() {
        assert_eq!(
            Tokenizer::new("alse").get_false_token_value(),
            Ok(TokenValue::Bool(false))
        )
    }

    #[test]
    fn get_false_token_value_fail() {
        assert_eq!(
            Tokenizer::new("").get_false_token_value(),
            Err("Failed to process bool(false), 'a' is not given".to_string())
        )
    }

    macro_rules! get_string_token_value_success {
        ($($func:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $func() {
                let (string, val) = $value;
                assert_eq!(
                    Tokenizer::new(string).get_string_token_value(),
                    Ok(TokenValue::String(val.to_string()))
                );
            }
        )*
        }
    }

    get_string_token_value_success! {
        get_string_token_value: (r#"string""#, "string"),
    }

    #[test]
    fn get_string_token_value_fail() {
        assert_eq!(
            Tokenizer::new("string").get_string_token_value(),
            Err("Failed to process string, not closed".to_string())
        )
    }

    macro_rules! get_number_token_value_success {
        ($($func:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $func() {
                let (start, string, num) = $value;
                assert_eq!(
                    Tokenizer::new(string).get_number_token_value(start),
                    Ok(TokenValue::Number(num))
                );
            }
        )*
        }
    }

    get_number_token_value_success! {
        // TODO : Check concat_idents! macro comes to stable
        get_number_token_value_success_with_zero: ('0', "", 0.0),
        get_number_token_value_success_with_integer: ('1', "234", 1234.0),
    }
}
