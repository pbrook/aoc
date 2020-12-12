use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<u32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect()
}

fn solve(input: &Vec<u32>) -> (u32, u64) {
    const MAX_DELTA: usize = 3;
    let mut prev = 0;
    let mut count = [0; MAX_DELTA];
    let mut ways = [0u64; MAX_DELTA];

    ways[0] = 1;

    for &j in input {
        let delta = (j - prev) as usize;
        prev = j;

        count[delta - 1] += 1;

        ways.rotate_right(delta);
        for n in 1..delta {
            ways[n] = 0;
        }
        let new_ways = ways.iter().sum();
        //println!("{} {} {:?} {}", j, delta, ways, new_ways);
        ways[0] = new_ways;

    }
    return (count[0] * (count[2] + 1), ways[0]);
}

fn test() {
    let mut v = parse("test1");
    v.sort_unstable();
    assert_eq!(solve(&v), (7*5, 8));

    let mut v = parse("test2");
    v.sort_unstable();
    assert_eq!(solve(&v), (22*10, 19208));
}

fn main() {
    test();

    let mut v = parse("input");
    v.sort_unstable();
    let (p1, p2) = solve(&v);
    println!("{}", p1);
    println!("{}", p2);
}
