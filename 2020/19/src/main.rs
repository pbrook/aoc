use std::fs::File;
use std::io::{self, BufRead};


#[derive(Debug,Clone,PartialEq)]
enum Rule {
    Char(char),
    Chain(Vec<Vec<usize>>),
}

#[derive(Debug,Default)]
struct Input {
    rules: Vec<Rule>,
    messages: Vec<String>,
}

fn parse_line(s: String) -> (usize, Rule) {
    let pos = s.find(':').unwrap();
    let n = s[..pos].parse().unwrap();
    let val = &s[pos+2..];
    let mut c = val.chars();
    let rule = if c.next() == Some('"') {
        Rule::Char(c.next().unwrap())
    } else {
        Rule::Chain(val.split('|').map(|opt| 
            opt.trim().split(' ').map(|num| num.parse().unwrap()).collect()
            ).collect())
    };
    return (n, rule);
}

fn parse(filename: &str) -> Input {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut input: Input = Default::default();
    let mut split = false;
    for s in lines.map(|v| v.unwrap()) {
        if s == "" {
            split = true;
            continue;
        }
        if split {
            input.messages.push(s);
        } else {
            let (n, rule) = parse_line(s);
            if n >= input.rules.len() {
                input.rules.resize(n + 1, Rule::Char(' '));
            }
            input.rules[n] = rule;
        }
    }
    return input;
}

fn match_rule(rules: &Vec<Rule>, n: usize, s: &str) -> Option<usize> {
    let result = match &rules[n] {
        Rule::Char(c) => {
            if s.chars().next() == Some(*c) {Some(1)} else {None}
        }
        Rule::Chain(v) => {
            let mut count = 0;
            let mut best = None;
            for opt in v {
                let mut len = 0;
                let mut ok = true;
                for &sub in opt {
                    match match_rule(rules, sub, &s[len..]) {
                        Some(n) => len += n,
                        None => {ok = false; break;}
                    }
                }
                if ok {
                    count += 1;
                    best = Some(len);
                }
            }
            assert!(count <= 1);
            best
        }
    };
    return result;
}

fn match_message(rules: &Vec<Rule>, s: &String) -> bool {
    match match_rule(rules, 0, s.as_str()) {
        Some(n) => n == s.len(),
        None => false,
    }
}

fn part1(input: &Input) -> usize {
    input.messages.iter().filter(|msg| match_message(&input.rules, msg)).count()
}

fn match_part2(rules: &Vec<Rule>, msg: &String) -> bool {
    let mut n42 = 0;
    let mut s = msg.as_str();
    loop {
        match match_rule(rules, 42, s) {
            Some(n) => {
                assert_eq!(match_rule(rules, 31, s), None);
                n42 += 1;
                s = &s[n..];
            }
            None => break,
        }
    }
    let mut n31 = 0;
    loop {
        match match_rule(rules, 31, s) {
            Some(n) => {
                n31 += 1;
                s = &s[n..];
            }
            None => break,
        }
    }
    if s != "" {
        return false;
    }
    return n42 > n31 && n31 > 0;
}

fn part2(input: &Input) -> usize {
    assert!(input.rules[0] == Rule::Chain(vec![vec![8, 11]]));
    input.messages.iter().filter(|msg| match_part2(&input.rules, msg)).count()
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 2);
    let v = parse("test2");
    assert_eq!(part1(&v), 3);
    assert_eq!(part2(&v), 12);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
