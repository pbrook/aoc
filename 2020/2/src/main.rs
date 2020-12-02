use std::fs::File;
use std::io::{self, BufRead};

struct PassEntry {
    min: i32,
    max: i32,
    c: u8,
    s: String
}
fn parse_line(s: String) -> PassEntry {
    let mut it = s.split(' ');

    let mut range = it.next().unwrap().split('-');
    let min = range.next().unwrap().parse().unwrap();
    let max = range.next().unwrap().parse().unwrap();
    let c = it.next().unwrap().bytes().next().unwrap();
    let s = it.next().unwrap();
    
    PassEntry {
        min: min,
        max: max,
        c: c,
        s: s.to_string(),
    }
}

fn parse(filename: &str) -> Vec<PassEntry> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

fn part1(v: &Vec<PassEntry>) -> i32 {
    let mut valid = 0;
    for p in v {
        let mut n = 0;
        for c in p.s.bytes() {
            if c == p.c {
                n += 1
            }
        }
        if (n >= p.min) && (n <= p.max) {
            valid += 1;
        }
    }
    return valid;
}

fn part2(v: &Vec<PassEntry>) -> i32 {
    let mut valid = 0;
    for p in v {
        let s = p.s.as_bytes();
        let a = s[p.min as usize - 1] == p.c;
        let b = s[p.max as usize - 1] == p.c;
        if a ^ b {
            valid += 1;
        }
    }
    return valid;
}

fn main() {
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
