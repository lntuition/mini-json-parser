use crate::error::Error;
use crate::json::Json;
use std::{collections::HashMap, str};

const VALUE_SEPERATOR: u8 = b',';
const NAME_SEPERATOR: u8 = b':';
const BEGIN_ARRAY: u8 = b'[';
const END_ARRAY: u8 = b']';
const BEGIN_OBJECT: u8 = b'{';
const END_OBJECT: u8 = b'}';

const QUOTATION_MARK: u8 = b'"';
const REVERSE_SOLIDUS: u8 = b'\\';
const SOLIDUS: u8 = b'/';
const BACK_SPACE: u8 = b'\x08';
const FORM_FEED: u8 = b'\x0C';
const LINE_FEED: u8 = b'\n';
const CARRIAGE_RETURN: u8 = b'\r';
const HORIZONTAL_TAB: u8 = b'\t';

#[derive(Debug)]
struct Parser<'a> {
    // Original source bytes
    bytes: &'a [u8],

    // Current string length
    length: usize,

    // Current string index
    idx: usize,
}

macro_rules! expect_sequence {
    ( $parser:ident, $( $ch:expr ), *) => {
        $(
            if $ch != $parser.get_next_byte()? {
                of_unexpected_character!($parser)
            }
        )*
        $parser.bump_byte();
    }
}

macro_rules! expect_escaped {
    ( $parser:ident, $buffer:expr ) => {{
        let byte = $parser.get_next_byte()?;
        if byte == b'u' {
            let num = $parser.get_next_hex_digit()? << 12
                | $parser.get_next_hex_digit()? << 8
                | $parser.get_next_hex_digit()? << 4
                | $parser.get_next_hex_digit()?;

            String::from_utf16(&[num])
                .expect("Wrong 4 hex digits")
                .as_bytes()
                .iter()
                .for_each(|b| $buffer.push(b.to_owned()))
        } else {
            let byte = match byte {
                QUOTATION_MARK => QUOTATION_MARK,
                REVERSE_SOLIDUS => REVERSE_SOLIDUS,
                SOLIDUS => SOLIDUS,
                b'b' => BACK_SPACE,
                b'f' => FORM_FEED,
                b'n' => LINE_FEED,
                b'r' => CARRIAGE_RETURN,
                b't' => HORIZONTAL_TAB,
                _ => of_unexpected_character!($parser),
            };
            $buffer.push(byte)
        }
    }};
}

macro_rules! expect_dec_pointed {
    ( $parser:ident ) => {
        if let Ok(b'.') = $parser.get_cur_byte() {
            match $parser.get_next_byte()? {
                b'0'..=b'9' => {}
                _ => of_unexpected_character!($parser),
            }
            until_non_dec_digit!($parser);
        }
    };
}

macro_rules! expect_dec_exponent {
    ( $parser:ident ) => {
        if $parser.get_cur_byte().unwrap_or(0x00).to_ascii_lowercase() == b'e' {
            let mut byte = $parser.get_next_byte()?;
            if byte == b'+' || byte == b'-' {
                byte = $parser.get_next_byte()?;
            }

            match byte {
                b'0'..=b'9' => {}
                _ => of_unexpected_character!($parser),
            }
            until_non_dec_digit!($parser);
        }
    };
}

macro_rules! until_non_dec_digit {
    ( $parser:ident ) => {
        while let Ok(byte) = $parser.get_next_byte() {
            if !byte.is_ascii_digit() {
                break;
            }
        }
    };
}

macro_rules! of_unexpected_character {
    ( $parser:ident ) => {{
        let (prev, next) = $parser.bytes.split_at($parser.idx);

        let ch = str::from_utf8(&next)
            .expect("Wrong idx")
            .chars()
            .next()
            .expect("Wrong unexpected character");

        let (row, col) = str::from_utf8(&prev)
            .expect("Wrong idx")
            .lines()
            .enumerate()
            .last()
            .map(|(r, c)| (r + 1, c.chars().count() + 1))
            .unwrap_or((1, 1)); // Empty source

        return Err(Error::UnexpectedChar { ch, row, col });
    }};
}

impl<'a> Parser<'a> {
    pub fn new(source: &'a str) -> Self {
        Parser {
            bytes: source.as_bytes(),
            length: source.len(),
            idx: 0,
        }
    }

    fn bump_byte(&mut self) {
        self.idx += 1;
    }

    fn get_cur_byte(&mut self) -> Result<u8, Error> {
        if self.length <= self.idx {
            return Err(Error::UnexpectedEndOfJson);
        }

        Ok(self.bytes[self.idx])
    }

    fn get_cur_non_whitespace_byte(&mut self) -> Result<u8, Error> {
        loop {
            match self.get_cur_byte()?.is_ascii_whitespace() {
                true => self.bump_byte(),
                false => break,
            }
        }
        Ok(self.bytes[self.idx])
    }

    fn get_next_byte(&mut self) -> Result<u8, Error> {
        self.bump_byte();
        self.get_cur_byte()
    }

    fn get_next_hex_digit(&mut self) -> Result<u16, Error> {
        let byte = self.get_next_byte()?;
        Ok(match byte {
            b'0'..=b'9' => (byte - b'0'),
            b'a'..=b'f' => (byte + 10 - b'a'),
            b'A'..=b'F' => (byte + 10 - b'A'),
            _ => of_unexpected_character!(self),
        } as u16)
    }

    pub fn parse(&mut self) -> Result<Json, Error> {
        let ret = match self.get_cur_non_whitespace_byte()? {
            b'n' => {
                expect_sequence!(self, b'u', b'l', b'l');
                Json::Null
            }
            b't' => {
                expect_sequence!(self, b'r', b'u', b'e');
                Json::Bool(true)
            }
            b'f' => {
                expect_sequence!(self, b'a', b'l', b's', b'e');
                Json::Bool(false)
            }
            QUOTATION_MARK => {
                let s = self.parse_string()?;
                Json::String(s)
            }
            b'0'..=b'9' => {
                let num = self.parse_number()?;
                Json::Number(num)
            }
            b'-' => {
                let num = match self.get_next_byte()? {
                    b'0'..=b'9' => self.parse_number()?,
                    _ => of_unexpected_character!(self),
                };
                Json::Number(-num)
            }
            BEGIN_ARRAY => self.parse_array()?,
            BEGIN_OBJECT => self.parse_object()?,
            _ => of_unexpected_character!(self),
        };

        // TODO : Check unresolved source exist
        Ok(ret)
    }

    fn parse_string(&mut self) -> Result<String, Error> {
        let mut buffer: Vec<u8> = Vec::new();
        loop {
            match self.get_next_byte()? {
                QUOTATION_MARK => break,
                REVERSE_SOLIDUS => expect_escaped!(self, buffer),
                byte if byte.is_ascii_control() => of_unexpected_character!(self),
                byte => buffer.push(byte),
            }
        }

        let s = String::from_utf8(buffer).expect("Wrong string");
        self.bump_byte();
        Ok(s)
    }

    fn parse_number(&mut self) -> Result<f64, Error> {
        let idx = self.idx;

        match self.get_cur_byte()? {
            b'0' => self.bump_byte(),
            b'1'..=b'9' => until_non_dec_digit!(self),
            _ => of_unexpected_character!(self),
        }

        expect_dec_pointed!(self);
        expect_dec_exponent!(self);

        Ok(str::from_utf8(&self.bytes[idx..self.idx])
            .expect("Wrong idx")
            .parse::<f64>()
            .expect("Wrong number"))
    }

    fn parse_array(&mut self) -> Result<Json, Error> {
        self.bump_byte();

        let mut v = vec![];
        loop {
            match self.get_cur_non_whitespace_byte()? {
                END_ARRAY => break,
                _ => {
                    v.push(Box::new(self.parse()?));
                    match self.get_cur_non_whitespace_byte()? {
                        END_ARRAY => break,
                        VALUE_SEPERATOR => self.bump_byte(),
                        _ => of_unexpected_character!(self),
                    }
                }
            }
        }

        self.bump_byte();
        Ok(Json::Array(v))
    }

    fn parse_object(&mut self) -> Result<Json, Error> {
        self.bump_byte();

        let mut m = HashMap::new();
        loop {
            match self.get_cur_non_whitespace_byte()? {
                END_OBJECT => break,
                _ => {
                    let key = self.parse_string()?;
                    match self.get_cur_non_whitespace_byte()? {
                        NAME_SEPERATOR => self.bump_byte(),
                        _ => of_unexpected_character!(self),
                    }

                    m.insert(key, Box::new(self.parse()?));
                    match self.get_cur_non_whitespace_byte()? {
                        END_OBJECT => break,
                        VALUE_SEPERATOR => self.bump_byte(),
                        _ => of_unexpected_character!(self),
                    }
                }
            }
        }

        self.bump_byte();
        Ok(Json::Object(m))
    }
}

pub fn parse(source: &str) -> Result<Json, Error> {
    Parser::new(source).parse()
}

#[cfg(test)]
mod tests {
    use super::{Error, Json, Parser};
    use crate::{json_array, json_object, json_string};
    use pretty_assertions::assert_eq;

    macro_rules! test {
        ( $name:ident, $input:expr, $expect:expr ) => {
            #[test]
            fn $name() {
                assert_eq!(Parser::new($input).parse(), $expect);
            }
        };
    }

    macro_rules! test_ok {
        ( $( $name:ident: $input:expr, $ok:expr; ) *) => {
            $(
                test!($name, $input, Ok($ok));
            )*
        }
    }

    macro_rules! test_err {
        ( $( $name:ident: $input:expr, $err:expr; ) *) => {
            $(
                test!($name, $input, Err($err));
            )*
        }
    }

    test_ok! {
        null_: ("null"), Json::Null;
        true_: ("true"), Json::Bool(true);
        false_: ("false"), Json::Bool(false);
        string_non_escaped: (r#""string""#), json_string!("string");
        string_escaped_quotation_mark: (r#""\"""#), json_string!("\u{0022}");
        string_escaped_solidus: (r#""\/""#), json_string!("\u{002F}");
        string_escaped_backspace: (r#""\b""#), json_string!("\u{0008}");
        string_escaped_form_feed: (r#""\f""#), json_string!("\u{000C}");
        string_escaped_line_feed: (r#""\n""#), json_string!("\u{000A}");
        string_escaped_carrage_return: (r#""\r""#), json_string!("\u{000D}");
        string_escaped_tab: (r#""\t""#), json_string!("\u{0009}");
        string_escaped_hex_digits: (r#""\u2661""#), json_string!("\u{2661}");
        string_mixed: (r#""\t\u2661rust""#), json_string!("\u{0009}♡rust");
        number_zero: ("0"), Json::Number(0.0);
        number_integer_positive: ("123"), Json::Number(123.0);
        number_integer_negative: ("-123"), Json::Number(-123.0);
        number_decimal: ("123.45"), Json::Number(123.45);
        number_decimal_zero: ("0.0"), Json::Number(0.0);
        number_integer_positive_exponent: ("1e+2"), Json::Number(100.0);
        number_integer_negative_exponent: ("1e-2"), Json::Number(0.01);
        number_integer_default_exponent: ("1e2"), Json::Number(100.0);
        number_decimal_positive_exponent: ("1.23e+2"), Json::Number(123.0);
        number_decimal_negative_exponent: ("1.23e-2"), Json::Number(0.0123);
        number_decimal_default_exponent: ("1.23e2"), Json::Number(123.0);
        array: (r#"[null, "string"]"#), json_array![Json::Null, json_string!("string")];
        array_empty: ("[]"), json_array![];
        array_nested: (r#"[123, ["string", true], 1e-2]"#),
                json_array![
                    Json::Number(123.0),
                    json_array![
                        json_string!("string"),
                        Json::Bool(true)
                    ],
                    Json::Number(0.01)
                ];
        array_nested_empty: ("[[[]]]"),
            json_array![
                json_array![
                    json_array![]
                ]
            ];
        object: (r#"{ "key": 123 }"#),
            json_object!{
                ("key".to_string(), Json::Number(123.0))
            };
        object_empty: ("{}"), json_object!{};
        object_nested: (r#"{"key": {"key" : 123}, "true": true }"#),
            json_object!{
                ("key".to_string(), json_object!{
                    ("key".to_string(), Json::Number(123.0))
                }),
                ("true".to_string(), Json::Bool(true))
            };
    }

    test_err! {
        null_typo: ("nulo"), Error::UnexpectedChar { ch : 'o', row : 1, col: 4 };
        true_typo: ("trie"), Error::UnexpectedChar { ch : 'i', row : 1, col: 3 };
        false_typo: ("falsy"), Error::UnexpectedChar { ch : 'y', row : 1, col: 5 };
        string_not_terminated: (r#""string"#), Error::UnexpectedEndOfJson;
        string_control_char: ("\"\u{000C}\""), Error::UnexpectedChar { ch : '\u{000C}', row : 1, col: 2 };
        string_wrong_escaped: (r#""\k""#), Error::UnexpectedChar { ch : 'k', row : 1, col: 3 };
        string_not_hex_digit: (r#""\u100g""#), Error::UnexpectedChar { ch : 'g', row : 1, col: 7 };
        number_wrong_decimal_point: ("1.f"), Error::UnexpectedChar { ch : 'f', row : 1, col: 3 };
        number_wrong_expoent: ("1et"), Error::UnexpectedChar { ch : 't', row : 1, col: 3 };
        array_wrong_seperated: ("[true 123]"), Error::UnexpectedChar { ch : '1', row : 1, col: 7 };
        object_wrong_name_seperated: (r#"{"key" 123}"#), Error::UnexpectedChar { ch : '1', row : 1, col: 8 };
        object_wrong_value_seperated: (r#"{"key": 123 "true": true}"#), Error::UnexpectedChar { ch : '"', row : 1, col: 13 };
    }
}
