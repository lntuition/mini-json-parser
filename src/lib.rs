mod error;
mod json;
mod parser;

#[cfg(test)]
mod tests {
    use crate::parser;
    use std::{fs::File, io::Read};

    macro_rules! test {
        ( $( $func:ident: $file:expr, $is_ok:expr; ) *) => {
            $(
                #[test]
                fn $func() {
                    let mut f = File::open($file).unwrap();
                    let mut source = String::new();
                    f.read_to_string(&mut source).unwrap();

                    match parser::parse(&source) {
                        Ok(_) => assert_eq!(true, $is_ok),
                        Err(_) => assert_eq!(false, $is_ok),
                    }
                }
            )*
        }
    }

    test! {
        canada: ("data/canada.json"), true;
        citm_catalog: ("data/citm_catalog.json"), true;
        twitter: ("data/twitter.json"), true;
    }
}
