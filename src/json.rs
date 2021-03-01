use std::collections::HashMap;
use std::fmt::{Display, Formatter, Result};

#[derive(Debug, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Json>),
    Object(HashMap<String, Json>),
}

impl Display for Json {
    fn fmt(&self, f: &mut Formatter) -> Result {
        match *self {
            Json::Null => {
                write!(f, "null")
            }
            Json::Bool(ref bool) => {
                write!(f, "{}", bool)
            }
            Json::Number(ref num) => {
                write!(f, "{}", num)
            }
            Json::String(ref s) => {
                write!(f, "\"{}\"", s)
            }
            Json::Array(ref arr) => {
                let arr = arr
                    .iter()
                    .map(|e| e.to_string())
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "[{}]", arr)
            }
            Json::Object(ref obj) => {
                let obj = obj
                    .iter()
                    .map(|(k, v)| format!("{}: {}", k, v))
                    .collect::<Vec<String>>()
                    .join(", ");
                write!(f, "{{{}}}", obj)
            }
        }
    }
}

impl Json {
    fn get(&self, key: &str) -> Option<&Json> {
        match self {
            Json::Object(obj) => obj.get(key),
            _ => None,
        }
    }

    fn get_mut(&mut self, key: &str) -> Option<&mut Json> {
        match self {
            Json::Object(obj) => obj.get_mut(key),
            _ => None,
        }
    }
}

#[macro_export]
macro_rules! json_string {
    ( $x:expr ) => {
        Json::String($x.to_string())
    };
}

#[macro_export]
macro_rules! json_array {
    () => {
        Json::Array(vec![])
    };
    ( $( $x:expr ), *) => {
        {
            let mut v: Vec<Json> = vec![];
            $(
                v.push($x);
            )*
            Json::Array(v)
        }
    };
}

#[macro_export]
macro_rules! json_object {
    () => {
        {
            use std::collections::HashMap;
            Json::Object(HashMap::new())
        }
    };
    ( $( $x:expr ), *) => {
        {
            use std::collections::HashMap;
            let mut m: HashMap<String, Json> = HashMap::new();
            $(
                let (k, v) = $x;
                m.insert(k, v);
            )*
            Json::Object(m)
        }
    };
}

#[cfg(test)]
mod tests {
    use super::Json;

    macro_rules! test {
        ( $( $name:ident: $input:expr, $expect:expr; ) *)=> {
            $(
                #[test]
                fn $name() {
                    assert_eq!($input.to_string(), $expect);
                }
            )*
        };
    }

    test! {
        null_: (Json::Null), "null";
        true_: Json::Bool(true), "true";
        false_: Json::Bool(false), "false";
        number: Json::Number(1.23), "1.23";
        string: json_string!("string"), r#""string""#;
        array: json_array![Json::Null, json_string!("string")], r#"[null, "string"]"#;
        array_empty: json_array![], "[]";
        object: json_object!{("key".to_string(), Json::Number(123.0))}, "{key: 123}";
        object_empty: json_object!{}, "{}";
    }
}
