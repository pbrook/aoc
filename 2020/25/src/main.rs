use std::fs::File;
use std::io::{self, BufRead};

const MODULUS: u64 = 20201227;
const SEED: u64 = 7;

fn parse(filename: &str) -> (u32, u32) {
    let file = File::open(filename).unwrap();
    let mut lines = io::BufReader::new(file).lines();
    let card = lines.next().unwrap().unwrap().parse().unwrap();
    let door = lines.next().unwrap().unwrap().parse().unwrap();
    return (card, door);
}

fn find_ls(pubkey: u32) -> u32 {
    let mut count = 0;
    let mut val = 1;
    while val != pubkey {
        count += 1;
        val = ((val as u64 * SEED) % MODULUS) as u32;
    }
    return count;
}

fn transform(subject: u32, ls: u32) -> u32 {
    let mut val = 1;
    for _ in 0..ls {
        val = ((val as u64 * subject as u64) % MODULUS) as u32;
    }
    return val;
}

fn part1(card: u32, door: u32) -> u32 {
    let cls = find_ls(card);
    let dls = find_ls(door);

    let ckey = transform(door, cls);
    let dkey = transform(card, dls);
    assert_eq!(ckey, dkey);
    return ckey;
}

fn test() {
    if cfg!(benchmark) {
        return;
    }
    assert_eq!(part1(5764801, 17807724), 14897079);
}

fn main() {
    test();
    let (card, door) = parse("input");
    println!("{}", part1(card, door));
}
