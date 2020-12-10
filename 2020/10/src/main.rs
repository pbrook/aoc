use std::fs::File;
use std::io::{self, BufRead};

fn parse(filename: &str) -> Vec<u32> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| v.unwrap().parse().unwrap()).collect()
}

fn count_ways(n: u32) -> u64 {
    // There's probably a formula for this
    // Or we could calculate it by brute force
    // But we can get away with working out the values we need by hand
    match n {
        0 => 1, // 0 (3)
        1 => 1, // 0 1 (4)
        2 => 2, // 0 1 2 (5), 0 2 (5)
        3 => 4, // 0 1 2 3 (6), 0 1 3 (6), 0 2 3 (6), 0 3 (6)
        4 => 7, // 0 1 2 3 4 (7), 0 1 2 4 (7), 0 1 3 4 (7), 0 2 3 4 (7)
                // 0 1 4 (7), 0 3 4 (7), 0 2 4 (7)
        _ => panic!("chain too long"),
    }
}

fn solve(input: &Vec<u32>) -> (u32, u64) {
    let mut prev = 0;
    let mut n1 = 0;
    let mut n3 = 0;
    let mut chain = 0;
    let mut p2 = 1;

    for &j in input {
        match j - prev {
            1 => chain += 1,
            3 => {
                n1 += chain;
                n3 += 1;
                p2 *= count_ways(chain);
                chain = 0;
            },
            _ => panic!("bad delta"),
        }
        prev = j;
    }
    n3 += 1;
    n1 += chain;
    p2 *= count_ways(chain);
    return (n1 * n3, p2);
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
