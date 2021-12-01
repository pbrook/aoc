use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<i32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect();
}

// When comparing two adjacent sliding windows, we're actually comparing the
// value "added" to the value "removed". Part 1 is the degenerate case
// of a single element window
fn count(v: &Vec<i32>, size: usize) -> i32 {
    let mut result = 0;
    for i in size..v.len() {
        if v[i - size] < v[i] {
            result += 1;
        }
    }
    return result;
}

fn test() {
    if cfg!(benchmark) {
        return;
    }
    let v = &parse("test");
    assert_eq!(count(v, 1), 7);
    assert_eq!(count(v, 3), 5);
}

fn main() {
    test();
    let v = &parse("input");
    println!("{}", count(v, 1));
    println!("{}", count(v, 3));
}
