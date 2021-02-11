use crate::{
    error::{Error, ErrorInfo},
    position::{Position, PositionRange},
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

    fn move_next(&mut self) {
        self.cur = self.chars.next();
        if let Some(ch) = self.cur {
            match ch {
                LINE_FEED => self.pos.next_row(),
                _ => self.pos.next_col(),
            }
        }
    }

    fn move_next_ignore_whitespace(&mut self) {
        while let Some(ch) = self.cur {
            if ch != SPACE && ch != TAB && ch != CARRIAGE_RETURN && ch != LINE_FEED {
                break;
            }
            self.move_next();
        }
    }

    fn check_expected(&mut self, expected: &str) -> bool {
        for expect in expected.chars() {
            if self.cur != Some(expect) {
                return false;
            }
            self.move_next();
        }
        true
    }

    pub fn tokenize(&mut self) -> Result<Vec<Token>, Error> {
        let mut tokens: Vec<Token> = vec![];
        loop {
            self.move_next_ignore_whitespace();
            match self.cur {
                Some(ch) => tokens.push(self.generate_token_start_with(ch)?),
                None => return Ok(tokens),
            }
        }
    }

    fn generate_token_start_with(&mut self, ch: char) -> Result<Token, Error> {
        let start = self.pos;
        let res = self.generate_value_start_with(ch);
        let range = PositionRange {
            start,
            end: self.pos,
        };

        match res {
            Ok(val) => Ok(Token { val, range }),
            Err(info) => Err(Error { info, range }),
        }
    }

    fn generate_value_start_with(&mut self, ch: char) -> Result<TokenValue, ErrorInfo> {
        match ch {
            'n' => self.generate_null_value(),
            't' => self.generate_true_value(),
            'f' => self.generate_false_value(),
            //'0'..='9' | '-' => self.get_number_token_value(),
            QUOTATION_MARK => self.generate_string_value(),
            _ => self.generate_reserved_value(),
        }
    }

    fn generate_reserved_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        let val = match self.cur.ok_or(ErrorInfo::NotProperHandledPoint(1))? {
            COMMA => TokenValue::Comma,
            COLON => TokenValue::Colon,
            LEFT_BRACE => TokenValue::LeftBrace,
            RIGHT_BRACE => TokenValue::RightBrace,
            LEFT_BRACKET => TokenValue::LeftBracket,
            RIGHT_BRACKET => TokenValue::RightBracket,
            ch @ _ => return Err(ErrorInfo::NotProperToken(ch)),
        };
        self.move_next();
        Ok(val)
    }

    fn generate_null_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        // TODO : ignore condition check logic with release
        if Some('n') != self.cur {
            return Err(ErrorInfo::NotProperHandledPoint(2));
        }
        self.move_next();

        match self.check_expected("ull") {
            true => Ok(TokenValue::Null),
            false => Err(ErrorInfo::NotNullToken),
        }
    }

    fn generate_true_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        // TODO : ignore condition check logic with release
        if Some('t') != self.cur {
            return Err(ErrorInfo::NotProperHandledPoint(3));
        }
        self.move_next();

        match self.check_expected("rue") {
            true => Ok(TokenValue::Bool(true)),
            false => Err(ErrorInfo::NotTrueToken),
        }
    }

    fn generate_false_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        // TODO : ignore condition check logic with release
        if Some('f') != self.cur {
            return Err(ErrorInfo::NotProperHandledPoint(4));
        }
        self.move_next();

        match self.check_expected("alse") {
            true => Ok(TokenValue::Bool(false)),
            false => Err(ErrorInfo::NotFalseToken),
        }
    }

    fn generate_string_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        // TODO : ignore condition check logic with release
        if Some(QUOTATION_MARK) != self.cur {
            return Err(ErrorInfo::NotProperHandledPoint(5));
        }
        self.move_next();

        let mut val: String = String::new();
        while let Some(ch) = self.generate_char()? {
            val.push(ch);
        }
        Ok(TokenValue::String(val))
    }

    fn generate_char(&mut self) -> Result<Option<char>, ErrorInfo> {
        let ch = self.cur.ok_or(ErrorInfo::UnexpectedEOF)?;
        self.move_next();

        Ok(match ch {
            REVERSE_SOLIDUS => Some(self.generate_escaped_char()?),
            QUOTATION_MARK => None,
            ch @ _ if ch.is_control() => return Err(ErrorInfo::NotAllowedControlChar(ch)),
            ch @ _ => Some(ch),
        })
    }

    fn generate_escaped_char(&mut self) -> Result<char, ErrorInfo> {
        let ch = self.cur.ok_or(ErrorInfo::UnexpectedEOF)?;
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
            ch @ _ => return Err(ErrorInfo::NotProperEscapedChar(ch)),
        })
    }

    fn generate_escaped_hex_digit_char(&mut self) -> Result<char, ErrorInfo> {
        let mut hex_digits = String::new();
        for _ in 0..4 {
            let digit = self.cur.ok_or(ErrorInfo::UnexpectedEOF)?;
            if !digit.is_ascii_hexdigit() {
                return Err(ErrorInfo::NotHexDigitChar(digit));
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

    // fn get_number_token_value(&mut self, start: char) -> Result<TokenValue, ErrorInfo> {
    //     let mut val: String = String::from(start);
    //     while let Some(ch) = self.cur {
    //         // TODO : check fraction and exponent
    //         if !ch.is_numeric() {
    //             break;
    //         }

    //         self.move_next();
    //         val.push(ch);
    //     }

    //     match val.parse::<f64>() {
    //         Ok(num) => Ok(TokenValue::Number(num)),
    //         Err(_) => Err(ErrorInfo::Custom("Failed to parse number".to_string())),
    //     }
    // }
}

#[cfg(test)]
mod tests {
    use super::{ErrorInfo, Lexer, TokenValue};

    macro_rules! generate_value {
        ($name:ident, $input:expr, $generator:ident, $expect:expr) => {
            #[test]
            fn $name() {
                assert_eq!(Lexer::new($input).$generator(), $expect);
            }
        };
    }

    macro_rules! generate_value_ok {
        ($name:ident, $input:expr, $generator:ident, $val:expr) => {
            generate_value!($name, $input, $generator, Ok($val));
        };
    }

    macro_rules! generate_value_err {
        ($name:ident, $input:expr, $generator:ident, $err:expr) => {
            generate_value!($name, $input, $generator, Err($err));
        };
    }

    macro_rules! generate_null_value_ok {
        ($($name:ident: $input:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_null_value, TokenValue::Null);
        )*
        }
    }

    generate_null_value_ok! {
        generate_null_value_ok: ("null");
    }

    macro_rules! generate_null_value_err {
        ($($name:ident: $input:expr, $err:expr;)*) => {
        $(
            generate_value_err! ($name, $input, generate_null_value, $err);
        )*
        }
    }

    generate_null_value_err! {
        generate_null_value_err: ("wrong"), ErrorInfo::NotProperHandledPoint(2);
        generate_null_value_err_not_null: ("none"), ErrorInfo::NotNullToken;
    }

    macro_rules! generate_true_value_ok {
        ($($name:ident: $input:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_true_value, TokenValue::Bool(true));
        )*
        }
    }

    generate_true_value_ok! {
        generate_true_value_ok: ("true");
    }

    macro_rules! generate_true_value_err {
        ($($name:ident: $input:expr, $err:expr;)*) => {
        $(
            generate_value_err! ($name, $input, generate_true_value, $err);
        )*
        }
    }

    generate_true_value_err! {
        generate_true_value_err: ("wrong"), ErrorInfo::NotProperHandledPoint(3);
        generate_true_value_err_not_true: ("torr"), ErrorInfo::NotTrueToken;
    }

    macro_rules! generate_false_value_ok {
        ($($name:ident: $input:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_false_value, TokenValue::Bool(false));
        )*
        }
    }

    generate_false_value_ok! {
        generate_false_value_ok: ("false");
    }

    macro_rules! generate_false_value_err {
        ($($name:ident: $input:expr, $err:expr;)*) => {
        $(
            generate_value_err! ($name, $input, generate_false_value, $err);
        )*
        }
    }

    generate_false_value_err! {
        generate_false_value_err: ("wrong"), ErrorInfo::NotProperHandledPoint(4);
        generate_false_value_err_not_false: ("falsy"), ErrorInfo::NotFalseToken;
    }

    macro_rules! generate_string_value_ok {
        ($($name:ident: $input:expr, $val:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_string_value, TokenValue::String($val.to_string()));
        )*
        }
    }

    generate_string_value_ok! {
        generate_string_value_ok_non_escaped: (r#""string""#), "string";
        generate_string_value_ok_escaped_quotation_mark: (r#""\"""#), "\u{0022}";
        generate_string_value_ok_escaped_reverse_solidus: (r#""\\""#), "\u{005C}";
        generate_string_value_ok_escaped_solidus: (r#""\/""#), "\u{002F}";
        generate_string_value_ok_escaped_backspace: (r#""\b""#), "\u{0008}";
        generate_string_value_ok_escaped_form_feed: (r#""\f""#), "\u{000C}";
        generate_string_value_ok_escaped_line_feed: (r#""\n""#), "\u{000A}";
        generate_string_value_ok_escaped_carrage_return: (r#""\r""#), "\u{000D}";
        generate_string_value_ok_escaped_tab: (r#""\t""#), "\u{0009}";
        generate_string_value_ok_escaped_hex_digits: (r#""\u2661""#), "\u{2661}";
        generate_string_value_ok_mixed: (r#""\t\u2661rust""#), "\u{0009}♡rust";
    }

    macro_rules! generate_string_value_err {
        ($($name:ident: $input:expr, $err:expr;)*) => {
        $(
            generate_value_err! ($name, $input, generate_string_value, $err);
        )*
        }
    }

    generate_string_value_err! {
        generate_string_value_err_wrong_trial: (r#"string""#), ErrorInfo::NotProperHandledPoint(5);
        generate_string_value_err_not_terminated: (r#""string"#), ErrorInfo::UnexpectedEOF;
        generate_string_value_err_control_char: ("\"\u{000C}\""), ErrorInfo::NotAllowedControlChar('\u{000C}');
        generate_string_value_err_wrong_escaped: (r#""\k""#), ErrorInfo::NotProperEscapedChar('k');
        generate_string_value_err_not_hex_digit: (r#""\u100G""#), ErrorInfo::NotHexDigitChar('G');
    }
}
