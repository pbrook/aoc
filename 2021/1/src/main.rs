use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<i32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect();
}

fn part1(v: &Vec<i32>) -> i32 {
    let mut prev = v[0];
    let mut result = 0;
    for &i in v {
        if i > prev {
            result += 1;
        }
        prev = i
    }
    return result;
}

fn test() {
    if cfg!(benchmark) {
        return;
    }
    assert_eq!(part1(&parse("test")), 7)
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
