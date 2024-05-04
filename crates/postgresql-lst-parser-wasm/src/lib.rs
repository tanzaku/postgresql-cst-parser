mod utils;

use postgresql_lst_parser::parse;
use wasm_bindgen::prelude::*;

#[wasm_bindgen]
extern "C" {
    fn alert(s: &str);
}

#[wasm_bindgen]
pub fn parse_sql(sql: &str) -> String {
    match parse(sql) {
        Ok(resolved_root) => format!("{:#?}", resolved_root),
        Err(e) => {
            return format!("Error: {:?}", e);
        }
    }
}
