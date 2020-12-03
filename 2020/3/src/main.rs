use std::fs::File;
use std::io::{self, BufRead};

fn parse_line(s: String) -> Vec<bool> {
    return s.chars().map(|c| c == '#').collect()
}

fn parse(filename: &str) -> Vec<Vec<bool>> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

fn part1(v: &Vec<Vec<bool>>) -> u32 {
    let mut x = 0;
    let mut trees = 0;
    for row in v {
        if row[x] {
            trees += 1;
        }
        x += 3;
        if x >= row.len() {
            x -= row.len();
        }
    }
    return trees;
}

fn main() {
    let v = parse("input");
    println!("{}", part1(&v))
}
