extern crate regex;

use std::fs::File;
use std::io::{self, BufRead};
use regex::Regex;

#[derive(Debug)]
struct Rule {
    name: String,
    min: [u32; 2],
    max: [u32; 2],
}

impl Rule {
    fn test(&self, val: u32) -> bool {
        (self.min[0] <= val && val <= self.max[0])
        || (self.min[1] <= val && val <= self.max[1])
    }
}

#[derive(Debug,Default)]
struct Notes {
    rules: Vec<Rule>,
    mine: Vec<u32>,
    tickets: Vec<Vec<u32>>,
}

fn parse_ticket(s: String) -> Vec<u32> {
    return s.split(',').map(|x| x.parse().unwrap()).collect();
}

fn parse(filename: &str) -> Notes {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines().map(|x| x.unwrap());
    let re_rule = Regex::new(r"(.*): (\d+)-(\d+) or (\d+)-(\d+)").unwrap();
    let mut section = 0;
    let mut ignore = false;
    let mut notes: Notes = Default::default();
    for s in lines {
        if ignore {
            ignore = false;
            continue;
        }
        if s == "" {
            section += 1;
            ignore = true;
            continue;
        }
        match section {
            0 => {
                let c = re_rule.captures(&s).unwrap();
                let min = [c[2].parse().unwrap(), c[4].parse().unwrap()];
                let max = [c[3].parse().unwrap(), c[5].parse().unwrap()];
                let r = Rule{name: c[1].to_string(), min: min, max:max};
                notes.rules.push(r);
            },
            1 => {
                notes.mine = parse_ticket(s);
            },
            2 => {
                notes.tickets.push(parse_ticket(s));
            },
            _ => panic!(),
        }
    }
    return notes;
}

fn not_valid(rules: &Vec<Rule>, val: u32) -> bool {
    return !rules.iter().any(|r| r.test(val));
}

fn part1(notes: &Notes) -> u32 {
    let mut sum = 0;
    for t in &notes.tickets {
        sum += t.iter().filter(|&&x| not_valid(&notes.rules, x)).sum::<u32>();
    }
    return sum;
}

type Bitmask = u32;

fn test_rules(rules: &Vec<Rule>, val: u32) -> Bitmask {
    let mut result = 0;
    let mut mask = 1;

    for r in rules.iter() {
        if r.test(val) {
            result |= mask;
        }
        mask <<= 1;
    }
    return result;
}

fn is_pow2(val: Bitmask) -> bool {
    (val & (val - 1)) == 0
}

fn solve(mask: &mut Vec<Bitmask>) {
    let ones = (1 << mask.len()) - 1;
    let mut prev = 0;
    loop {
        let pow2 = mask.iter().fold(0, |acc, &b| if is_pow2(b) {acc | b} else {acc});
        if pow2 == prev {
            panic!("No solution found");
        }
        prev = pow2;
        if pow2 == ones {
            return;
        }
        for b in mask.iter_mut() {
            if !is_pow2(*b) {
                *b &= !pow2;
            }
        }
    }
}

fn find_position(notes: &Notes) -> Vec<usize> {
    let nrules = notes.rules.len();
    let ones = (1u32 << nrules) - 1;
    let mut rule_mask = vec![ones; nrules];

    for t in &notes.tickets {
        let valid: Vec<u32> = t.iter().map(|&x| test_rules(&notes.rules, x)).collect();
        if valid.iter().any(|&v| v == 0) {
            continue;
        }
        for n in 0..nrules {
            rule_mask[n] &= valid[n];
        }
    }
    solve(&mut rule_mask);
    return rule_mask.iter().map(|&b| b.trailing_zeros() as usize).collect();
}

fn part2(notes: &Notes) -> u64 {
    let pos = find_position(notes);
    let mut res = 1;
    for (&n, &val) in pos.iter().zip(&notes.mine) {
        if notes.rules[n].name.starts_with("departure") {
            res *= val as u64;
        }
    }
    return res;
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 71);
    let v = parse("test2");
    assert_eq!(find_position(&v), [1, 0, 2]);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
