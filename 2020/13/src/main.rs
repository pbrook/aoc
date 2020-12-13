use std::fs::File;
use std::io::{self, BufRead};

struct Timetable {
    time: u64,
    ids: Vec<Option<u64>>,
}

fn parse_line(s: String) -> Vec<Option<u64>> {
    return s.split(',').map(|x| x.parse().ok()).collect();
}

fn parse(filename: &str) -> Timetable {
    let file = File::open(filename).unwrap();
    let mut lines = io::BufReader::new(file).lines();

    let mut next_line = || lines.next().unwrap().ok().unwrap();

    let time: u64 = next_line().parse().unwrap();
    return Timetable{time: time, ids: parse_line(next_line())};
}

// Assume ap is "small"
fn part1(t: &Timetable) -> u64 {
    let mut best = u64::MAX;
    let mut best_id = 0;
    for bus in t.ids.iter().filter_map(|&x| x) {
        let delay = bus - (t.time % bus);
        if delay < best {
            best = delay;
            best_id = bus;
        }
    }
    return best * best_id;
}

#[derive(Debug)]
struct Point {
    period: u64,
    offset: u64,
}

// This works is a.period is "small", a.period and b.period are coprime
// If they are not coprime then the result.period should be GCD(a, b)
fn find_offset(a: Point, b: Point) -> Point {
    // Iterate B until A is in the right place
    let mut a_start = 0;
    let mut b_iter = 0;
    while (a_start + b.offset + a.offset) % a.period != 0 {
        b_iter += 1;
        a_start = (a_start + b.period) % a.period;
    }
    // And calculate the total offset
    let result = b_iter * b.period + b.offset;

    return Point{period:a.period * b.period, offset:result};
}

fn part2(v: &Vec<Option<u64>>) -> u64 {
    let mut result = Point{period: 1, offset: 0};
    for (n, &bus) in v.iter().enumerate() {
        match bus {
            None => (),
            Some(period) => {
                let p = Point{period: period, offset: n as u64};
                result = find_offset(p, result);
            }
        }
    }
    return result.offset;
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 295);
    assert_eq!(part2(&v.ids), 1068781);
    assert_eq!(part2(&parse_line("17,x,13,19".to_string())), 3417);
    assert_eq!(part2(&parse_line("67,7,59,61".to_string())), 754018);
    assert_eq!(part2(&parse_line("67,x,7,59,61".to_string())), 779210);
    assert_eq!(part2(&parse_line("67,7,x,59,61".to_string())), 1261476);
    assert_eq!(part2(&parse_line("1789,37,47,1889".to_string())), 1202161486);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v.ids));
}
