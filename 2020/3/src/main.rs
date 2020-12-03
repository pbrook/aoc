use std::fs::File;
use std::io::{self, BufRead};

fn parse_line(s: String) -> Vec<bool> {
    return s.chars().map(|c| c == '#').collect()
}

fn parse(filename: &str) -> Vec<Vec<bool>> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

fn part1(v: &Vec<Vec<bool>>, dx: usize, dy: usize) -> u32 {
    let mut x = 0;
    let mut trees = 0;
    let mut y = 0;
    for row in v {
        if y > 0 {
            y -= 1;
            continue;
        }
        y = dy - 1;
        if row[x] {
            trees += 1;
        }
        x += dx;
        if x >= row.len() {
            x -= row.len();
        }
    }
    return trees;
}

fn part2(v: &Vec<Vec<bool>>) -> u32 {
    let slopes = [(1,1), (3,1), (5,1), (7,1), (1,2)];
    let mut tot = 1;
    for (x, y) in slopes.iter() {
        tot *= part1(v, *x, *y);
    }
    return tot;
}
fn main() {
    let v = parse("input");
    println!("{}", part1(&v, 3, 1));
    println!("{}", part2(&v));
}
