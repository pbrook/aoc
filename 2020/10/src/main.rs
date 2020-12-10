use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<u32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect()
}

fn part1(input: &Vec<u32>) -> u32 {
    let mut prev = 0;
    let mut n1 = 0;
    let mut n3 = 0;

    for &j in input {
        match j - prev {
            1 => n1 += 1,
            3 => n3 += 1,
            _ => (),
        }
        prev = j;
    }
    return n1 * (n3 + 1);
}

fn test() {
    let mut v = parse("test1");
    v.sort();
    assert_eq!(part1(&v), 7*5);

    let mut v = parse("test2");
    v.sort();
    assert_eq!(part1(&v), 22 * 10);
}

fn main() {
    test();

    let mut v = parse("input");
    v.sort();
    println!("{}", part1(&v));
}
