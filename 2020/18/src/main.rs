use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
enum Token {
    Number(u64),
    Symbol(char),
}

type Expr = Vec<Token>;

fn parse_line(s: &str) -> Expr {
    let mut result = Vec::new();
    let mut num: String = "".to_string();
    for c in s.chars() {
        match c {
            '0'..='9' => num.push(c),
            ' ' => (),
            _ => {
                if num != "" {
                    result.push(Token::Number(num.parse().unwrap()));
                    num = "".to_string();
                }
                result.push(Token::Symbol(c));
            },
        }
    }
    if num != "" {
        result.push(Token::Number(num.parse().unwrap()));
    }
    result.push(Token::Symbol(')'));
    return result;
}

fn parse(filename: &str) -> Vec<Expr> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap().as_str())).collect();
}

fn eval<'a>(e: &mut impl Iterator<Item=&'a Token>, advanced: bool) -> u64 {
    let mut result = 1;
    let mut accum = match e.next() {
        Some(&Token::Number(n)) => n,
        Some(&Token::Symbol('(')) => eval(e, advanced),
        t => panic!("Expected expr, got {:?}", t),
    };
    loop {
        match e.next() {
            None | Some(Token::Symbol(')')) => {
                return result * accum;
            }
            Some(Token::Symbol(c)) => {
                let arg = e.next();
                let val = match arg {
                    Some(&Token::Number(n)) => n,
                    Some(&Token::Symbol('(')) => eval(e, advanced),
                    _ => panic!("Expected arg, got {:?}", arg),
                };
                match c {
                    '+' => accum += val,
                    '*' => if advanced {
                        result *= accum;
                        accum = val;
                    } else {
                        accum *= val;
                    },
                    _ => panic!("Expected operator, got {:?}", c),
                };
            },
            Some(Token::Number(n)) => panic!("Expected operator, got {:?}", n),
        }
    }
}

fn eval_expr(e: &Expr, advanced: bool) -> u64 {
    return eval(&mut e.iter(), advanced);
}

fn part1(input: &Vec<Expr>) -> u64 {
    return input.iter().map(|e| eval_expr(e, false)).sum();
}

fn part2(input: &Vec<Expr>) -> u64 {
    return input.iter().map(|e| eval_expr(e, true)).sum();
}

fn test_expr(s: &str) -> (u64, u64) {
    let e = parse_line(s);
    return (eval_expr(&e, false), eval_expr(&e, true));
}

fn test() {
    //let v = parse("test");
    //assert_eq!(part1(&v), 112);
    assert_eq!(test_expr("1 + 2 * 3 + 4 * 5 + 6"), (71, 231));
    assert_eq!(test_expr("1 + (2 * 3) + (4 * (5 + 6))"), (51, 51));
    assert_eq!(test_expr("2 * 3 + (4 * 5)"), (26, 46));
    assert_eq!(test_expr("5 + (8 * 3 + 9 + 3 * 4 * 3)"), (437, 1445));
    assert_eq!(test_expr("5 * 9 * (7 * 3 * 3 + 9 * 3 + (8 + 6 * 4))"), (12240, 669060));
    assert_eq!(test_expr("((2 + 4 * 9) * (6 + 9 * 8 + 6) + 6) + 2 + 4 * 2"), (13632, 23340));
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
