use std::fs::File;
use std::io::{self, BufRead};

fn part_index(s: &str) -> u32 {
    let mut base = 0;
    let mut size = 1 << s.len();
    for c in s.chars() {
        size >>= 1;
        if c == 'B' || c == 'R' {
            base += size;
        }
    }
    return base;
}

fn parse(filename: &str) {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();

    let mut min = 1 << 10;
    let mut max = 0;
    let mut val = 0;
    let mut n = 0;

    for seat in lines.map(|v| part_index(v.unwrap().as_str())) {
        if seat < min {
            min = seat;
        }
        if seat > max {
            max = seat;
        }
        n += 1;
        // xor all the seat IDs together. Each block of 4 seats (half a row)
        // will cancel each other out. The problem states that the only 
        // missing seats are at the front and back. If we pad those to a
        // multiple of 4, the result will be the ID that is missing.
        // Credit to jbert for the idea
        val ^= seat;
    }

    // Check all seats ids were contiguous (except the missing one!)
    assert_eq!(n, max - min);
    // Populate the missing seatch in the first/last [half] row
    let mut extra = max + 1;
    while (extra & 3) != 0 {
        val ^= extra;
        extra += 1;
    }
    let mut extra = min;
    while (extra & 3) != 0 {
        extra -= 1;
        val ^= extra;
    }
    println!("{}", max);
    println!("{}", val);
}


fn main() {
    assert_eq!(part_index(&"BFFFBBFRRR"), 567);
    assert_eq!(part_index(&"FFFBBBFRRR"), 119);
    assert_eq!(part_index(&"BBFFBBFRLL"), 820);
    parse("input");
}
