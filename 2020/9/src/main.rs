use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<u64> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect()
}

fn find_sum(v: &Vec<u64>, target: u64) -> bool {
    // Numbers are sorted. Start with the smallest and biggest
    let mut a = 0;
    let mut b = v.len() - 1;

    loop {
        let tot = v[a] + v[b];
        if tot == target {
            return true;
        }
        // If total is not big enough then pick the next bigger "small" value
        // If totol too big then pick the next smaller "big" value.
        if tot < target {
            a += 1;
        } else {
            b -= 1;
        }
        if a >= b {
            return false;
        }
    }
}

fn part1(preamble: usize, input: &Vec<u64>) -> u64 {
    let mut v = input[..preamble].to_vec();
    let mut offset = 0;

    for &n in &input[preamble..] {
        let mut sorted = v.clone();
        sorted.sort_unstable();
        if !find_sum(&sorted, n) {
            return n;
        }

        v[offset] = n;
        offset += 1;
        if offset >= preamble {
            offset = 0;
        }
    }
    panic!("no result found");
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(5, &v), 127);
}

fn main() {
    test();

    let v = parse("input");
    println!("{}", part1(25, &v));
}
