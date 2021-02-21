use std::collections::HashMap;
#[derive(Debug, PartialEq)]
pub enum Json {
    Null,
    Bool(bool),
    Number(f64),
    String(String),
    Array(Vec<Box<Json>>),
    Object(HashMap<String, Box<Json>>),
}

#[macro_export]
macro_rules! json_array {
    () => {
        Json::Array(vec![])
    };
    ( $( $x:expr ), *) => {
        {
            let mut v: Vec<Box<Json>> = vec![];
            $(
                v.push(Box::new($x));
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
    ( $( $k:expr; $v:expr), *) => {
        {
            use std::collections::HashMap;
            let mut m: HashMap<String, Box<Json>> = HashMap::new();
            $(
                m.insert($k, Box::new($v));
            )*
            Json::Object(m)
        }
    };
}
