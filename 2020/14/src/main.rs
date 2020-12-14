use std::fs::File;
use std::io::{self, BufRead};

enum Insn {
    Mask(u64, u64), // ones, zeros
    Write(usize, u64), // addr, val
}

fn parse_mask(s: &str) -> Insn {
    let mut ones = 0;
    let mut zeros = 0;
    let mut mask = 1u64 << 35;
    assert!(s.len() == 36);
    for c in s.chars() {
        match c {
            '1' => ones |= mask,
            '0' => zeros |= mask,
            _ => (),
        }
        mask >>= 1;
    }
    return Insn::Mask(ones, zeros);
}

fn parse_line(s: String) -> Insn {
    let eq = s.find('=').unwrap();
    if &s[..4] == "mask" {
        return parse_mask(&s[7..]);
    }
    // mem [...] =
    let addr: usize = s[4..eq-2].parse().unwrap();
    let val: u64 = s[eq+2..].parse().unwrap();
    return Insn::Write(addr, val);
}

fn parse(filename: &str) -> Vec<Insn> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect()
}

fn part1(prog: &Vec<Insn>) -> u64 {
    let mut memsz = 0;
    for i in prog.iter() {
        match *i {
            Insn::Write(addr,_) => memsz = memsz.max(addr),
            _ => (),
        }
    }
    let mut mem = vec![0u64; memsz+1];
    let mut m1 = 0;
    let mut m0 = 0;
    for i in prog {
        match *i {
            Insn::Mask(ones, zeros) => {m1 = ones; m0 = zeros},
            Insn::Write(addr, val) => mem[addr] = (val | m1) & !m0,
        }
    }
    return mem.iter().sum();
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 165);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
}
