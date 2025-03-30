// use criterion::{black_box, Criterion};
// use postgresql_cst_parser::parse;

// fn test_all_bench(c: &mut Criterion) {
//     let mut sqls = Vec::new();
//     for e in std::fs::read_dir("./tests/data/src").unwrap() {
//         let e = e.unwrap();

//         if e.file_type().unwrap().is_file() {
//             let path = e.path();
//             let file_stem = path.file_stem().unwrap();
//             let file_stem = file_stem.to_str().unwrap();

//             let dst_path = e
//                 .path()
//                 .parent()
//                 .unwrap()
//                 .parent()
//                 .unwrap()
//                 .join("dst")
//                 .join(format!("{}.txt", file_stem));

//             let content = std::fs::read_to_string(path)?;
//             sqls.push(content);
//             // let tree = parse(&content).unwrap();
//         }
//     }

//     c.bench_function("bench", |b| {
//         b.iter(|| {
//             for sql in &sqls {
//                 parse(black_box(sql)).unwrap();
//             }
//         })
//     });
// }
