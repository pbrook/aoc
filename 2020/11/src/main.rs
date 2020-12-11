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
    part1: bool,
}

fn count_near(seats: &SeatArray, limits: &Limits, x0: usize, y0: usize) -> u32 {
    let mut count = 0;
    for dx in -1..2 {
        for dy in -1..2 {
            if dx != 0 || dy != 0 {
                let mut x = x0 as isize;
                let mut y = y0 as isize;
                loop {
                    x += dx;
                    y += dy;
                    if x < 0 || x >= limits.w as isize || y < 0 || y >= limits.h as isize {
                        break;
                    }
                    match seats[y as usize][x as usize] {
                        Seat::Occupied => {
                            count += 1;
                            break;
                        },
                        Seat::Empty => break,
                        Seat::Floor => if limits.part1 {break},
                    }
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
    let crowded = if limits.part1 {4} else {5};
    for y in 0..limits.h {
        for x in 0..limits.w {
            let near = count_near(prev, limits, x, y);
            state[y][x] = match prev[y][x] {
                Seat::Floor => Seat::Floor,
                Seat::Empty => {
                    if near == 0 {Seat::Occupied} else {Seat::Empty}
                },
                Seat::Occupied => {
                    if near < crowded {Seat::Occupied} else {Seat::Empty}
                },
            };
        }
    }
}

// where F: Fn(&Boat, usize, usize) -> u32,
fn run(v: &SeatArray, part1: bool) -> usize
{
    let tmp0 = RefCell::new(v.clone());
    let tmp1 = RefCell::new(v.clone());
    let limits = &Limits{w: v[0].len(), h: v.len(), part1: part1};

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

fn test() {
    let v = parse("test");
    assert_eq!(run(&v, true), 37);
    assert_eq!(run(&v, false), 26);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", run(&v, true));
    println!("{}", run(&v, false));
}
