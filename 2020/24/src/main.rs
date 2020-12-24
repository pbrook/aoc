use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;

#[derive(Debug,PartialEq,Eq,Hash)]
struct Point {
    a: i32,
    b: i32,
}

type Chain = Vec<Dir>;
#[derive(Debug)]
enum Dir {
    E,
    W,
    SE,
    SW,
    NW,
    NE
}

fn parse_line(s:String) -> Chain {
    let mut result = Vec::new();
    let mut c = s.chars();
    loop {
        match c.next() {
            None => {return result;}
            Some(ch) => {
                result.push(match ch {
                    'e' => Dir::E,
                    'w' => Dir::W,
                    'n' => match c.next().unwrap() {
                        'e' => Dir::NE,
                        'w' => Dir::NW,
                        _ => panic!(),
                    }
                    's' => match c.next().unwrap() {
                        'e' => Dir::SE,
                        'w' => Dir::SW,
                        _ => panic!(),
                    }
                    _ => panic!(),
                });
            }
        }
    }
}

fn parse(filename: &str) -> Vec<Chain> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

fn follow(c: &Chain) -> Point {
    // Choose NE (a) and NW (b) as our primary axes
    let mut a = 0;
    let mut b = 0;
    for d in c {
        match d {
            Dir::NE => {a += 1}
            Dir::SW => {a -= 1}
            Dir::NW => {b += 1}
            Dir::SE => {b -= 1}
            Dir::E => {a += 1; b -= 1}
            Dir::W => {a -= 1; b += 1}
        }
    }
    return Point{a, b};
}

fn part1(input: &Vec<Chain>) -> usize {
    let mut points = HashSet::new();
    for c in input {
        let p = follow(c);
        if points.contains(&p) {
            points.remove(&p);
        } else {
            points.insert(p);
        }
    }
    return points.len();
}

fn test() {
    let v = parse("test");
    assert_eq!(follow(&parse_line("nwwswee".to_string())), Point{a: 0, b: 0});
    assert_eq!(part1(&v), 10);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
