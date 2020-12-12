use std::fs::File;
use std::io::{self, BufRead};

struct Insn {
    action: char,
    arg: i32,
}

fn parse_line(s: String) -> Insn {
    return Insn{action: s.chars().next().unwrap(), arg: s[1..].parse().unwrap()};
}

fn parse(filename: &str) -> Vec<Insn> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

fn part1(v: &Vec<Insn>) -> i32 {
    let mut x = 0;
    let mut y = 0;
    let mut dir = vec!['E', 'N', 'W', 'S'];
    for i in v {
        let mut action = i.action;
        if action == 'F' {
            action = dir[0];
        }
        match action {
            'N' => y -= i.arg,
            'E' => x -= i.arg,
            'S' => y += i.arg,
            'W' => x += i.arg,
            'L' => dir.rotate_left((i.arg / 90) as usize),
            'R' => dir.rotate_right((i.arg / 90) as usize),
            _ => panic!("Bad insn"),
        }
    }
    return x.abs() + y.abs();
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 25);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
