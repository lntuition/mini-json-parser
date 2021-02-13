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

    fn move_if_expected(&mut self, expected: &str) -> bool {
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
            QUOTATION_MARK => self.generate_string_value(),
            '0'..='9' | '-' => self.generate_number_value(),
            _ => self.generate_reserved_value(),
        }
    }

    fn generate_reserved_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        let val = match self.cur.ok_or(ErrorInfo::NotProperHandledPoint(1))? {
            COMMA => TokenValue::ValueSeperator,
            COLON => TokenValue::NameSeperator,
            LEFT_BRACE => TokenValue::BeginObject,
            RIGHT_BRACE => TokenValue::EndObject,
            LEFT_BRACKET => TokenValue::BeginArray,
            RIGHT_BRACKET => TokenValue::EndArray,
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

        match self.move_if_expected("ull") {
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

        match self.move_if_expected("rue") {
            true => Ok(TokenValue::True),
            false => Err(ErrorInfo::NotTrueToken),
        }
    }

    fn generate_false_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        // TODO : ignore condition check logic with release
        if Some('f') != self.cur {
            return Err(ErrorInfo::NotProperHandledPoint(4));
        }
        self.move_next();

        match self.move_if_expected("alse") {
            true => Ok(TokenValue::False),
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
                return Err(ErrorInfo::NotHexDigit(digit));
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

    fn generate_number_value(&mut self) -> Result<TokenValue, ErrorInfo> {
        let mut num = match self.cur.ok_or(ErrorInfo::UnexpectedEOF)? {
            '-' => {
                self.move_next();
                String::from('-')
            }
            _ => String::new(),
        };

        num.push_str(&self.generate_integer()?);
        num.push_str(&self.generate_fraction()?);
        num.push_str(&self.generate_exponent()?);

        match num.parse::<f64>() {
            Ok(num) => Ok(TokenValue::Number(num)),
            Err(_) => Err(ErrorInfo::UnreachablePoint(5)),
        }
    }

    fn generate_integer(&mut self) -> Result<String, ErrorInfo> {
        if Some('0') == self.cur {
            self.move_next();
            return Ok(String::from('0'));
        }
        self.generate_digits()
    }

    fn generate_fraction(&mut self) -> Result<String, ErrorInfo> {
        if Some('.') != self.cur {
            return Ok(String::new());
        }

        let mut fraction = String::from('.');
        self.move_next();

        fraction.push_str(&self.generate_digits()?);
        Ok(fraction)
    }

    fn generate_exponent(&mut self) -> Result<String, ErrorInfo> {
        if Some('e') != self.cur && Some('E') != self.cur {
            return Ok(String::new());
        }

        let mut exponent = String::from('e');
        self.move_next();

        if let Some(sign) = self.cur {
            if sign == '+' || sign == '-' {
                self.move_next();
                exponent.push(sign);
            }
        }

        exponent.push_str(&self.generate_digits()?);
        Ok(exponent)
    }

    fn generate_digits(&mut self) -> Result<String, ErrorInfo> {
        let mut digits = String::new();
        while let Some(digit) = self.cur {
            match digit {
                '0'..='9' => {
                    self.move_next();
                    digits.push(digit)
                }
                _ => break,
            }
        }

        match digits.is_empty() {
            true => Err(ErrorInfo::NotDecDigits),
            false => Ok(digits),
        }
    }
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

    macro_rules! generate_reserved_value_ok {
        ($($name:ident: $input:expr, $val:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_reserved_value, $val);
        )*
        }
    }

    generate_reserved_value_ok! {
        generate_reserved_value_ok_value_sperator: (","), TokenValue::ValueSeperator;
        generate_reserved_value_ok_name_sperator: (":"), TokenValue::NameSeperator;
        generate_reserved_value_ok_begin_object: ("{"), TokenValue::BeginObject;
        generate_reserved_value_ok_end_object: ("}"), TokenValue::EndObject;
        generate_reserved_value_ok_begin_array: ("["), TokenValue::BeginArray;
        generate_reserved_value_ok_end_array: ("]"), TokenValue::EndArray;
    }

    macro_rules! generate_reserved_value_err {
        ($($name:ident: $input:expr, $err:expr;)*) => {
        $(
            generate_value_err! ($name, $input, generate_reserved_value, $err);
        )*
        }
    }

    generate_reserved_value_err! {
        generate_reserved_value_err_internal: (""), ErrorInfo::NotProperHandledPoint(1);
        generate_reserved_value_err_not_null: ("wrong"), ErrorInfo::NotProperToken('w');
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
        generate_null_value_err_internal: ("wrong"), ErrorInfo::NotProperHandledPoint(2);
        generate_null_value_err_not_null: ("none"), ErrorInfo::NotNullToken;
    }

    macro_rules! generate_true_value_ok {
        ($($name:ident: $input:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_true_value, TokenValue::True);
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
        generate_true_value_err_internal: ("wrong"), ErrorInfo::NotProperHandledPoint(3);
        generate_true_value_err_not_true: ("torr"), ErrorInfo::NotTrueToken;
    }

    macro_rules! generate_false_value_ok {
        ($($name:ident: $input:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_false_value, TokenValue::False);
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
        generate_false_value_err_internal: ("wrong"), ErrorInfo::NotProperHandledPoint(4);
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
        generate_string_value_err_internal: (r#"string""#), ErrorInfo::NotProperHandledPoint(5);
        generate_string_value_err_not_terminated: (r#""string"#), ErrorInfo::UnexpectedEOF;
        generate_string_value_err_control_char: ("\"\u{000C}\""), ErrorInfo::NotAllowedControlChar('\u{000C}');
        generate_string_value_err_wrong_escaped: (r#""\k""#), ErrorInfo::NotProperEscapedChar('k');
        generate_string_value_err_not_hex_digit: (r#""\u100G""#), ErrorInfo::NotHexDigit('G');
    }

    macro_rules! generate_number_value_ok {
        ($($name:ident: $input:expr, $val:expr;)*) => {
        $(
            generate_value_ok! ($name, $input, generate_number_value, TokenValue::Number($val));
        )*
        }
    }

    generate_number_value_ok! {
        generate_number_value_ok_integer_zero: ("0"), 0.0;
        generate_number_value_ok_negative_zero: ("-0"), 0.0;
        generate_number_value_ok_real_zero: ("0.0"), 0.0;
        generate_number_value_ok_real_negative_zero: ("-0.0"), 0.0;

        generate_number_value_ok_integer: ("123"), 123.0;
        generate_number_value_ok_negative_integer: ("-100"), -100.0;
        generate_number_value_ok_real: ("1.234"), 1.234;
        generate_number_value_ok_negative_real: ("-1.234"), -1.234;

        generate_number_value_ok_integer_zero_positive_exponent: ("0e+4"), 0.0;
        generate_number_value_ok_integer_zero_negative_exponent: ("0e-4"), 0.0;
        generate_number_value_ok_integer_zero_default_exponent: ("0e4"), 0.0;
        generate_number_value_ok_negative_zero_positive_exponent: ("-0e+4"), 0.0;
        generate_number_value_ok_negative_zero_negative_exponent: ("-0e-4"), 0.0;
        generate_number_value_ok_negative_zero_default_exponent: ("-0e4"), 0.0;

        generate_number_value_ok_real_zero_positive_exponent: ("0.0e+4"), 0.0;
        generate_number_value_ok_real_zero_negative_exponent: ("0.0e-4"), 0.0;
        generate_number_value_ok_real_zero_default_exponent: ("0.0e4"), 0.0;
        generate_number_value_ok_real_negative_zero_positive_exponent: ("-0.0e+4"), 0.0;
        generate_number_value_ok_real_negative_zero_negative_exponent: ("-0.0e-4"), 0.0;
        generate_number_value_ok_real_negative_zero_default_exponent: ("-0.0e4"), 0.0;

        generate_number_value_ok_integer_positive_exponent: ("1e+2"), 100.0;
        generate_number_value_ok_integer_negative_exponent: ("1e-2"), 0.01;
        generate_number_value_ok_integer_default_exponent: ("1e2"), 100.0;
        generate_number_value_ok_negative_integer_positive_exponent: ("-1e+2"), -100.0;
        generate_number_value_ok_negative_integer_negative_exponent: ("-1e-2"), -0.01;
        generate_number_value_ok_negative_integer_default_exponent: ("-1e2"), -100.0;

        generate_number_value_ok_real_positive_exponent: ("1.23e+2"), 123.0;
        generate_number_value_ok_real_negative_exponent: ("1.23e-2"), 0.0123;
        generate_number_value_ok_real_default_exponent: ("1.23e2"), 123.0;
        generate_number_value_ok_negative_real_positive_exponent: ("-1.23e+2"), -123.0;
        generate_number_value_ok_negative_real_negative_exponent: ("-1.23e-2"), -0.0123;
        generate_number_value_ok_negative_real_default_exponent: ("-1.23e2"), -123.0;
    }

    macro_rules! generate_number_value_err {
        ($($name:ident: $input:expr, $err:expr;)*) => {
        $(
            generate_value_err! ($name, $input, generate_number_value, $err);
        )*
        }
    }

    generate_number_value_err! {
        generate_number_value_err_wrong_integer: ("A"), ErrorInfo::NotDecDigits;
        generate_number_value_err_wrong_fraction: ("1."), ErrorInfo::NotDecDigits;
        generate_number_value_err_wrong_exponent: ("1e"), ErrorInfo::NotDecDigits;
    }
}
