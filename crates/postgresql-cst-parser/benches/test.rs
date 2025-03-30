use criterion::{black_box, criterion_group, criterion_main, Criterion};
use postgresql_cst_parser::parse;

fn test_all_bench(c: &mut Criterion) {
    let mut sqls = Vec::new();
    for e in std::fs::read_dir("./tests/data/src").unwrap() {
        let e = e.unwrap();

        if e.file_type().unwrap().is_file() {
            let content = std::fs::read_to_string(e.path()).unwrap();
            sqls.push(content);
        }
    }

    c.bench_function("bench", |b| {
        b.iter(|| {
            for sql in &sqls {
                parse(black_box(sql)).unwrap();
            }
        })
    });
}

fn test_single_sql(c: &mut Criterion) {
    c.bench_function("bench", |b| {
        b.iter(|| {
            parse(black_box(r#"select;"#)).unwrap();
        })
    });
}

// ベンチマークグループを定義する
criterion_group!(benches, test_all_bench, test_single_sql);

// main関数を用意
criterion_main!(benches);
