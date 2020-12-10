use std::fs::File;
use std::io::{self, BufRead};
use std::cmp::Ordering;

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
        match tot.cmp(&target) {
            Ordering::Equal => return true,
            Ordering::Less => a += 1,
            Ordering::Greater => b -= 1,
        }
        if a >= b {
            return false;
        }
    }
}

fn part1(input: &Vec<u64>, preamble: usize) -> u64 {
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

fn part2(v: &Vec<u64>, target: u64) -> u64 {
    let mut start = 0;
    let mut end = 0;
    let mut sum = 0;
    loop {
        match sum.cmp(&target) {
            Ordering::Equal => {
                let range = &v[start..end];
                return range.iter().min().unwrap() + range.iter().max().unwrap();
            }
            Ordering::Less => {
                sum += v[end];
                end += 1;
            },
            Ordering::Greater => {
                sum -= v[start];
                start += 1;
                while (sum - v[end-1]) >= target {
                    sum -= v[end-1];
                    end -= 1;
                }
            },
        }
    }
}

fn test() {
    let v = parse("test");
    let p1 = part1(&v, 5);
    assert_eq!(p1, 127);
    assert_eq!(part2(&v, p1), 62);
}

fn main() {
    test();

    let v = parse("input");
    let p1 = part1(&v, 25);
    println!("{}", p1);
    println!("{}", part2(&v, p1));
}
