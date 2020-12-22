use std::fs::File;
use std::io::{self, BufRead};
use std::collections::VecDeque;
use std::cmp::Ordering;

type Deck = VecDeque<u32>;

fn parse(filename: &str) -> Vec<Deck> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut result = Vec::new();
    let mut deck: Deck = VecDeque::new();
    for s in lines.map(|v| v.unwrap()) {
        if s == "" {
            result.push(deck);
            deck = VecDeque::new();
            continue;
        }
        if s.starts_with("Player") {
            continue;
        }
        // Keep deck "backwards" to make caculating score easier
        deck.push_front(s.parse().unwrap());
    }
    result.push(deck);
    return result;
}

fn part1(input: &Vec<Deck>) -> u32 {
    let mut p1 = input[0].clone();
    let mut p2 = input[1].clone();
    while p1.len() != 0 && p2.len() != 0 {
        let c1 = p1.pop_back().unwrap();
        let c2 = p2.pop_back().unwrap();
        let winner = match c1.cmp(&c2) {
            Ordering::Greater => &mut p1,
            Ordering::Less => &mut p2,
            _ => panic!("Draw!"),
        };
        winner.push_front(c1.max(c2));
        winner.push_front(c1.min(c2));
    }
    let winner = if p1.len() > 0 {&p1} else {&p2};
    return winner.into_iter().enumerate().map(|(n, &v)| (n + 1) as u32 * v).sum();
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 306);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
