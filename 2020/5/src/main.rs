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

fn parse(filename: &str) -> Vec<u32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| part_index(v.unwrap().as_str())).collect();
}

fn part1(v: &Vec<u32>) -> u32 {
    return v[v.len() - 1];
}

fn part2(v: &Vec<u32>) -> u32 {
    let invalid = 1 << 10;
    let mut prev = invalid;
    for &i in v.iter() {
        if prev == i - 2 {
            return i - 1;
        }
        prev = i;
    }
    return invalid;
}

fn main() {
    assert_eq!(part_index(&"BFFFBBFRRR"), 567);
    assert_eq!(part_index(&"FFFBBBFRRR"), 119);
    assert_eq!(part_index(&"BBFFBBFRLL"), 820);
    let mut v = parse("input");
    v.sort();
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
