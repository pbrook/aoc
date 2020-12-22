use std::fs::File;
use std::io::{self, BufRead};
use std::collections::VecDeque;
use std::cmp::Ordering;

type Deck = VecDeque<usize>;

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
        deck.push_back(s.parse().unwrap());
    }
    result.push(deck);
    return result;
}

fn deck_score(d: &Deck) -> usize {
    let len = d.len();
    return d.iter().enumerate().map(|(n, &v)| (len - n) * v).sum();
}

fn part1(input: &Vec<Deck>) -> usize {
    let mut p1 = input[0].clone();
    let mut p2 = input[1].clone();
    while p1.len() != 0 && p2.len() != 0 {
        let c1 = p1.pop_front().unwrap();
        let c2 = p2.pop_front().unwrap();
        let winner = match c1.cmp(&c2) {
            Ordering::Greater => &mut p1,
            Ordering::Less => &mut p2,
            _ => panic!("Draw!"),
        };
        winner.push_back(c1.max(c2));
        winner.push_back(c1.min(c2));
    }
    return deck_score(if p1.len() > 0 {&p1} else {&p2});
}

fn play(p1: &mut Deck, p2: &mut Deck) -> bool {
    let mut loop1 = VecDeque::new();
    let mut loop2 = VecDeque::new();
    let mut loop_max = 1;
    let mut loop_count = 0;
    while p1.len() != 0 && p2.len() != 0 {
        if loop_count == 0 {
            loop_max *= 2;
            loop_count = loop_max;
            loop1 = p1.clone();
            loop2 = p2.clone();
        }
        loop_count -= 1;
        let c1 = p1.pop_front().unwrap();
        let c2 = p2.pop_front().unwrap();
        let p1_won = if p1.len() >= c1 && p2.len() >= c2 {
            let mut r1 = p1.clone();
            let mut r2 = p2.clone();
            r1.truncate(c1);
            r2.truncate(c2);
            play(&mut r1, &mut r2)
        } else {
            match c1.cmp(&c2) {
                Ordering::Greater => true,
                Ordering::Less => false,
                _ => panic!("Draw!"),
            }
        };
        if p1_won {
            p1.push_back(c1);
            p1.push_back(c2);
        } else {
            p2.push_back(c2);
            p2.push_back(c1);
        }
        if p1 == &loop1 && p2 == &loop2 {
            return true;
        }
    }
    return p1.len() > 0;
}

fn part2(input: &Vec<Deck>) -> usize {
    let p1 = &mut input[0].clone();
    let p2 = &mut input[1].clone();
    let winner = if play(p1, p2) {p1} else {p2};
    return deck_score(winner);
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 306);
    assert_eq!(part2(&v), 291);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
