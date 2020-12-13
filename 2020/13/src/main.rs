use std::fs::File;
use std::io::{self, BufRead};

struct Timetable {
    time: u32,
    ids: Vec<u32>,
}

fn parse_line(s: String) -> Vec<u32> {
    return s.split(',').filter_map(|x| x.parse().ok()).collect();
}

fn parse(filename: &str) -> Timetable {
    let file = File::open(filename).unwrap();
    let mut lines = io::BufReader::new(file).lines();

    let mut next_line = || lines.next().unwrap().ok().unwrap();
    let time: u32 = next_line().parse().unwrap();
    return Timetable{time: time, ids: parse_line(next_line())};
}


fn part1(t: &Timetable) -> u32 {
    let mut best = u32::MAX;
    let mut best_id = 0;
    for &bus in &t.ids {
        let delay = bus - (t.time % bus);
        if delay < best {
            best = delay;
            best_id = bus;
        }
    }
    return best * best_id;
}

fn test() {
    let v = parse("test");
    //assert_eq!(part1(&v), 25);
    println!("{}", part1(&v));
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
