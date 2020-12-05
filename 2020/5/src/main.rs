use std::fs::File;
use std::io::{self, BufRead};

fn part_index(s: &str) -> u32 {
    let mut base = 0;
    let mut size = 1 << s.len();
    for c in s.chars() {
        size >>= 1;
        if c == 'B' || c == 'R' {
            base += size;
        }
    }
    return base;
}

fn part1(filename: &str) -> u32 {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| part_index(v.unwrap().as_str())).max().unwrap();
}

fn main() {
    assert_eq!(part_index(&"BFFFBBFRRR"), 567);
    assert_eq!(part_index(&"FFFBBBFRRR"), 119);
    assert_eq!(part_index(&"BBFFBBFRLL"), 820);
    println!("{}", part1("input"));
}
