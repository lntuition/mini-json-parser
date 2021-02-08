use crate::position::Position;
use crate::{
    error::{Error, ErrorInfo},
    token::{Token, TokenValue},
};
use std::str::Chars;

const SPACE: char = '\u{0020}';
const QUOTATION_MARK: char = '\u{0022}';
const REVERSE_SOLIDUS: char = '\u{005C}';
const SOLIDUS: char = '\u{002F}';
const BACK_SPACE: char = '\u{0008}';
const FORM_FEED: char = '\u{000C}';
const LINE_FEED: char = '\u{000A}';
const CARRIAGE_RETURN: char = '\u{000D}';
const TAB: char = '\u{0009}';

const COMMA: char = ',';
const COLON: char = ':';
const LEFT_BRACKET: char = '[';
const RIGHT_BRACKET: char = ']';
const LEFT_BRACE: char = '{';
const RIGHT_BRACE: char = '}';

#[derive(Debug)]
struct Lexer<'a> {
    chars: Chars<'a>,
    cur: Option<char>,
    pos: Position,
}

impl<'a> Lexer<'a> {
    pub fn new(s: &'a str) -> Self {
        let mut chars = s.chars();
        let cur = chars.next();
        let pos = Position { row: 1, col: 1 };

        Lexer { chars, cur, pos }
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = vec![];

        while let Some(ch) = self.cur {
            let pos = self.pos;

            self.move_next();
            if ch == SPACE || ch == TAB || ch == CARRIAGE_RETURN || ch == LINE_FEED {
                continue;
            }

            let token = match self.get_token_value(ch) {
                Ok(val) => Token { pos, val },
                Err(info) => return Err(Error { pos, info }),
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

    fn get_token_value(&mut self, start: char) -> Result<TokenValue, ErrorInfo> {
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
            '"' => self.generate_string(),
            '0'..='9' | '-' => self.get_number_token_value(start),
            _ => Err(ErrorInfo::Custom("Failed to get token value".to_string())),
        }
    }

    fn get_null_token_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        self.get_expected_token_value(&['u', 'l', 'l'], TokenValue::Null)
    }

    fn get_true_token_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        self.get_expected_token_value(&['r', 'u', 'e'], TokenValue::Bool(true))
    }

    fn get_false_token_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        self.get_expected_token_value(&['a', 'l', 's', 'e'], TokenValue::Bool(false))
    }

    fn generate_string(&mut self) -> Result<TokenValue, ErrorInfo> {
        let mut val: String = String::new();
        while let Some(ch) = self.generate_char()? {
            val.push(ch);
        }
        Ok(TokenValue::String(val))
    }

    fn generate_char(&mut self) -> Result<Option<char>, ErrorInfo> {
        let ch = self.cur.ok_or(ErrorInfo::NotStringClosed)?;
        self.move_next();

        Ok(match ch {
            REVERSE_SOLIDUS => Some(self.generate_escaped_char()?),
            QUOTATION_MARK => None,
            ch @ _ if ch.is_control() => return Err(ErrorInfo::NotAllowedControlChar),
            ch @ _ => Some(ch),
        })
    }

    fn generate_escaped_char(&mut self) -> Result<char, ErrorInfo> {
        let ch = self.cur.ok_or(ErrorInfo::NotStringClosed)?;
        self.move_next();

        Ok(match ch {
            QUOTATION_MARK => QUOTATION_MARK,
            REVERSE_SOLIDUS => REVERSE_SOLIDUS,
            SOLIDUS => SOLIDUS,
            'b' => BACK_SPACE,
            'f' => FORM_FEED,
            'n' => LINE_FEED,
            'r' => CARRIAGE_RETURN,
            't' => TAB,
            'u' => self.generate_escaped_hex_digit_char()?,
            _ => return Err(ErrorInfo::UnrecognizableEscapedChar),
        })
    }

    fn generate_escaped_hex_digit_char(&mut self) -> Result<char, ErrorInfo> {
        let mut hex_digits = String::new();
        for _ in 0..4 {
            let digit = self.cur.ok_or(ErrorInfo::NotStringClosed)?;
            if !digit.is_ascii_hexdigit() {
                return Err(ErrorInfo::NotHexDigitChar);
            }
            self.move_next();
            hex_digits.push(digit)
        }

        let hex_num = match u16::from_str_radix(&hex_digits, 16) {
            Ok(hex_num) => hex_num,
            Err(_) => return Err(ErrorInfo::UnreachablePoint(1)),
        };

        let mut unicode_chars = match String::from_utf16(&[hex_num]) {
            Ok(s) => s,
            Err(_) => return Err(ErrorInfo::UnreachablePoint(2)),
        };

        let ch = unicode_chars.pop().ok_or(ErrorInfo::UnreachablePoint(3))?;
        if !unicode_chars.is_empty() {
            return Err(ErrorInfo::UnreachablePoint(4));
        }

        Ok(ch)
    }

    fn get_number_token_value(&mut self, start: char) -> Result<TokenValue, ErrorInfo> {
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
            Err(_) => Err(ErrorInfo::Custom("Failed to parse number".to_string())),
        }
    }

    fn get_expected_token_value(
        &mut self,
        expected: &[char],
        result: TokenValue,
    ) -> Result<TokenValue, ErrorInfo> {
        for &expect in expected.iter() {
            if self.cur != Some(expect) {
                return Err(ErrorInfo::NotExpectCharGiven(expect));
            }
            self.move_next();
        }
        Ok(result)
    }
}

#[cfg(test)]
mod tests {
    use super::{ErrorInfo, TokenValue, Lexer};

    #[test]
    fn get_null_token_value_success() {
        assert_eq!(
            Lexer::new("ull").get_null_token_value(),
            Ok(TokenValue::Null)
        )
    }

    #[test]
    fn get_true_token_value_success() {
        assert_eq!(
            Lexer::new("rue").get_true_token_value(),
            Ok(TokenValue::Bool(true))
        )
    }

    #[test]
    fn get_false_token_value_success() {
        assert_eq!(
            Lexer::new("alse").get_false_token_value(),
            Ok(TokenValue::Bool(false))
        )
    }

    macro_rules! generate_string_success {
        ($($func:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $func() {
                let (string, val) = $value;
                assert_eq!(
                    Lexer::new(string).generate_string(),
                    Ok(TokenValue::String(val.to_string()))
                );
            }
        )*
        }
    }

    generate_string_success! {
        generate_string_success_non_escaped: (r#"string""#, "string"),
        generate_string_success_escaped_quotation_mark: (r#"\"""#, "\u{0022}"),
        generate_string_success_escaped_reverse_solidus: (r#"\\""#, "\u{005C}"),
        generate_string_success_escaped_solidus: (r#"\/""#, "\u{002F}"),
        generate_string_success_escaped_backspace: (r#"\b""#, "\u{0008}"),
        generate_string_success_escaped_form_feed: (r#"\f""#, "\u{000C}"),
        generate_string_success_escaped_line_feed: (r#"\n""#, "\u{000A}"),
        generate_string_success_escaped_carrage_return: (r#"\r""#, "\u{000D}"),
        generate_string_success_escaped_tab: (r#"\t""#, "\u{0009}"),
        generate_string_success_escaped_hex_digits: (r#"\u2661""#, "\u{2661}"),
        generate_string_success_mixed: (r#"\t\u2661rust""#, "\u{0009}♡rust"),
    }

    macro_rules! generate_string_fail {
        ($($func:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $func() {
                let (string, err) = $value;
                assert_eq!(
                    Lexer::new(string).generate_string(),
                    Err(err)
                );
            }
        )*
        }
    }

    generate_string_fail! {
        generate_string_fail_not_closed: (r#"string"#, ErrorInfo::NotStringClosed),
        generate_string_fail_control_char: ("\u{000C}\"", ErrorInfo::NotAllowedControlChar),
        generate_string_fail_unrecognizable_escaped: (r#"\k""#, ErrorInfo::UnrecognizableEscapedChar),
        generate_string_fail_not_hex_digit: (r#"\u100G""#, ErrorInfo::NotHexDigitChar),
    }

    macro_rules! get_number_token_value_success {
        ($($func:ident: $value:expr,)*) => {
        $(
            #[test]
            fn $func() {
                let (start, string, num) = $value;
                assert_eq!(
                    Lexer::new(string).get_number_token_value(start),
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
