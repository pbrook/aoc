use std::fs::File;
use std::io::{self, BufRead};
use std::cell::RefCell;

#[derive(Debug,Copy,Clone,PartialEq)]
enum Seat {
    Floor,
    Empty,
    Occupied,
}

impl Seat {
    fn from_char(c: char) -> Seat {
        match c {
            '.' => Seat::Floor,
            'L' => Seat::Empty,
            '#' => Seat::Occupied,
            _ => panic!("Bad char '{}'", c),
        }
    }
    #[allow(dead_code)]
    fn to_char(&self) -> char {
        match self {
            Seat::Floor => '.',
            Seat::Empty => 'L',
            Seat::Occupied => '#',
        }
    }
}

type SeatArray = Vec<Vec<Seat>>;

fn parse_line(s: String) -> Vec<Seat>{
    return s.chars().map(|c| Seat::from_char(c)).collect();
}

fn parse(filename: &str) -> SeatArray {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

struct Limits {
    w: usize,
    h: usize,
}

fn count_near(seats: &SeatArray, limits: &Limits, x: usize, y:usize) -> u32 {
    let mut count = 0;
    let x0 = if x > 0 {x-1} else {0};
    let x1 = if x+1 < limits.w {x+2} else {x+1};
    let y0 = if y > 0 {y-1} else {0};
    let y1 = if y+1 < limits.h {y+2} else {y+1};
    for v in y0..y1 {
        for u in x0..x1 {
            if u != x || v != y {
                if seats[v][u] == Seat::Occupied {
                    count += 1;
                }
            }
        }
    }
    return count;
}

fn _dump(seats: &SeatArray) {
    for row in seats.iter() {
        println!("{}", row.iter().map(|s| s.to_char()).collect::<String>());
    }
}

fn step(state: &mut SeatArray, prev: &SeatArray, limits: &Limits) {
    for y in 0..limits.h {
        for x in 0..limits.w {
            let near = count_near(prev, limits, x, y);
            state[y][x] = match prev[y][x] {
                Seat::Floor => Seat::Floor,
                Seat::Empty => {
                    if near == 0 {Seat::Occupied} else {Seat::Empty}
                },
                Seat::Occupied => {
                    //println!("{} {} {}", x, y, near);
                    if near < 4 {Seat::Occupied} else {Seat::Empty}
                },
            };
        }
    }
}

// where F: Fn(&Boat, usize, usize) -> u32,
fn run(v: &SeatArray) -> usize
{
    let tmp0 = RefCell::new(v.clone());
    let tmp1 = RefCell::new(v.clone());
    let limits = &Limits{w: v[0].len(), h: v.len()};

    loop {
        {
            let prev = &*tmp0.borrow();
            let state = &mut *tmp1.borrow_mut();
            step(state, prev, limits);
            /*
            _dump(state);
            println!("");
            */
            if prev == state {
                return state.iter().map(|row| row.iter().filter(|&&c| c == Seat::Occupied).count()).sum();
            }
        }
        tmp0.swap(&tmp1);
    }
}

fn part1(v: &Vec<Vec<Seat>>) -> usize {
    run(v)
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 37);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
