use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

#[macro_use] extern crate lazy_static;
extern crate regex;

use regex::Regex;

struct BagCount {
    name: String,
    _count: u32,
}

struct Bag {
    name: String,
    holds: Vec<BagCount>
}

fn parse_line(s: String) -> Bag {
    lazy_static! {
        static ref NAME_RE: Regex = Regex::new(r"(.*) bags contain").unwrap();
        static ref HOLDS_RE: Regex = Regex::new(r"(\d+) ([^,]*) bag").unwrap();
    }
    let name = NAME_RE.captures(&s).unwrap()[1].to_string();
    let holds = HOLDS_RE.captures_iter(&s).map(
        |c| BagCount{_count:c[1].parse::<u32>().unwrap(), name: c[2].to_string()}
        ).collect();
    return Bag{name: name, holds: holds};
}

fn parse(filename: &str) -> Vec<Bag> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}


fn part1(v: &Vec<Bag>) -> usize {
    let mut shiny: HashMap<String, ()> = HashMap::new();
    shiny.insert("shiny gold".to_string(), ());
    let mut again = true;
    while again {
        again = false;
        for b in v.iter() {
            let outer = &b.name;
            if shiny.contains_key(outer) {
                continue;
            }
            for held in b.holds.iter() {
                if shiny.contains_key(&held.name) {
                    shiny.insert(outer.clone(), ());
                    again = true;
                    break;
                }
            }
        }
    }
    return shiny.len() - 1;
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 4);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
