use std::io::Read;

use postgresql_cst_parser;

fn main() {
    let mut sql = String::new();
    std::io::stdin().read_to_string(&mut sql).unwrap();

    match postgresql_cst_parser::parse(&sql) {
        Ok(tree) => println!("{tree:#?}"),
        Err(e) => eprintln!("{e:#?}"),
    }
}
