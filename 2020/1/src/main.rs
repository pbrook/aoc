use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<i32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect();
}

fn part1(v: &Vec<i32>, target: i32) -> Option<i32> {
    // Numbers are sorted. Start with the smallest and biggest
    let mut a = 0;
    let mut b = v.len() - 1;

    loop {
        let tot = v[a] + v[b];
        if tot == target {
            return Some(v[a] * v[b]);
        }
        // If total is not big enough then pick the next bigger "small" value
        // If totol too big then pick the next smaller "big" value.
        if tot < target {
            a += 1;
        } else {
            b -= 1;
        }
        if a >= b {
            return None;
        }
    }
}

fn part2(v: &Vec<i32>, target: i32) -> i32 {
    for x in v {
        // There are a few micro-optimizations we could do here by restricting
        // the search-space. Doesn't get us any algorithmic benefit though.
        match part1(v, target - x) {
            Some(i) => return i * x,
            None => (),
        }
    }
    panic!("part2 fail");
}

fn main() {
    let mut v = parse("input");
    v.sort_unstable();
    // Could do v.dedup() here to discard duplicate values
    println!("{}", part1(&v, 2020).unwrap());
    println!("{}", part2(&v, 2020));
}
