use std::fs::File;
use std::io::{self, BufRead};


#[derive(Debug,Clone)]
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
    //println!("rule   {} {}", n, s);
    let result = match &rules[n] {
        Rule::Char(c) => {
            if s.chars().next() == Some(*c) {Some(1)} else {None}
        }
        Rule::Chain(v) => {
            let mut count = 0;
            let mut best = None;
            for opt in v {
                //println!("opt    {} {:?}", n, opt);
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
    //println!("Result {} {:?}", n, result);
    return result;
}

fn match_all(rules: &Vec<Rule>, s: &String) -> bool {
    //println!("match_all {}", s);
    match match_rule(rules, 0, s.as_str()) {
        Some(n) => n == s.len(),
        None => false,
    }
}

fn part1(input: &Input) -> usize {
    input.messages.iter().filter(|msg| match_all(&input.rules, msg)).count()
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 2);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
