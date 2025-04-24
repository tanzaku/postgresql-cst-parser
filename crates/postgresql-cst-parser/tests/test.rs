use postgresql_cst_parser::parse_2way;

#[test]
fn test_all() -> Result<(), std::io::Error> {
    for e in std::fs::read_dir("./tests/data/src")? {
        let e = e?;

        if e.file_type()?.is_file() {
            let path = e.path();
            let file_stem = path.file_stem().unwrap();
            let file_stem = file_stem.to_str().unwrap();

            let dst_path = e
                .path()
                .parent()
                .unwrap()
                .parent()
                .unwrap()
                .join("dst")
                .join(format!("{}.txt", file_stem));

            let content = std::fs::read_to_string(path)?;
            let tree = parse_2way(&content).unwrap();
            std::fs::write(dst_path, format!("{:#?}", tree))?;
        }
    }

    Ok(())
}
