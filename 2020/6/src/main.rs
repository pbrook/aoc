use std::fs::File;
use std::io::{self, BufRead};

fn collate(a: &[u32], n: u32) ->u32 {
    let count = a.iter().filter(|&&v| v == n).count();
    return count as u32;
}

fn parse(filename: &str) -> (u32, u32) {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut answers = [0u32; 26];
    let mut total: u32 = 0;
    let mut t2: u32 = 0;
    let mut n: u32 = 0;

    for person in lines.map(|v| v.unwrap()) {
        if person == "" {
            total += 26 - collate(&answers, 0);
            t2 += collate(&answers, n);
            answers = [0u32; 26];
            n = 0;
            continue;
        }
        n += 1;
        for c in person.chars() {
            assert!(c >= 'a' && c <= 'z');
            answers[c as usize - 'a' as usize] += 1
        }
    }
    total += 26 - collate(&answers, 0);
    t2 += collate(&answers, n);
    return (total, t2);
}


fn main() {
    assert_eq!(parse("test"), (11, 6));
    let (p1, p2) = parse("input");
    println!("{}", p1);
    println!("{}", p2);
}
