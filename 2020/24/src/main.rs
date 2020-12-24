use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;

#[derive(Debug,Copy,Clone,PartialEq,Eq,Hash)]
struct Point_<T> {
    a: T,
    b: T,
}

// This can be any integer type.
// i8 is substantially faster than i32, and sufficient for my input!
type Point = Point_<i8>;

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

fn points_near(p: &Point) -> Vec<Point> {
    let a = p.a;
    let b = p.b;

    return vec![
        Point{a: a + 1, b: b},
        Point{a: a - 1, b: b},
        Point{a: a, b: b + 1},
        Point{a: a, b: b - 1},
        Point{a: a + 1, b: b - 1},
        Point{a: a - 1, b: b + 1},
    ]
}

fn count_near(points: &HashSet<Point>, cur: &Point) -> usize {
    let near = points_near(cur);
    return near.iter().filter(|p| points.contains(p)).count();
}

fn solve(input: &Vec<Chain>) -> (usize, usize) {
    let mut points = HashSet::new();
    for c in input {
        let p = follow(c);
        if points.contains(&p) {
            points.remove(&p);
        } else {
            points.insert(p);
        }
    }
    let part1 = points.len();

    for _day in 0..100 {
        let mut np: HashSet<Point> = HashSet::with_capacity(points.len() * 2);
        for cur in points.iter() {
            {
                let n = count_near(&points, &cur);
                if n == 1 || n == 2 {
                    np.insert(*cur);
                }
            }
            let near = points_near(&cur);
            for p in near.iter() {
                if points.contains(p) {
                    continue;
                }
                let n = count_near(&points, p);
                if n == 2 {
                    np.insert(*p);
                }
            }
        }
        points = np;
    }
    return (part1, points.len());
}

fn test() {
    let v = parse("test");
    assert_eq!(follow(&parse_line("nwwswee".to_string())), Point{a: 0, b: 0});
    assert_eq!(solve(&v), (10, 2208));
}

fn main() {
    test();
    let v = parse("input");
    let (p1, p2) = solve(&v);
    println!("{}", p1);
    println!("{}", p2);
}
