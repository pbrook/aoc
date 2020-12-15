use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug,Copy,Clone)]
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
    let addr = s[4..eq-2].parse().unwrap();
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
    for &i in prog.iter() {
        match i {
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

#[derive(Debug,Copy,Clone)]
struct Written {
    mask: u64,
    addr: u64,
    val: u64,
}

// This assumes we have a small number of floating bits in each write
// It will not work for the part1 example data, but works for the part2
// example, and cofortably handles my input (max 9 floating bits per mask)
fn try_sum(w: &Written, rest: &[Written]) -> u64 {
    let mut count = 0;
    let float = w.mask ^ ((1 << 36) - 1);
    let mut bits = 0;
    // Enumerate all the addresses, and test whether they are
    // overwritten by a subsequent write
    loop {
        let addr = bits | w.addr;
        if !rest.iter().any(|r| addr & r.mask == r.addr) {
            count += 1;
        }
        bits = (bits + 1 + w.mask) & float;
        if bits == 0 {
            break;
        }
    }
    return count * w.val;
}

fn part2(prog: &Vec<Insn>) -> u64 {
    let mut writes = Vec::new();
    let mut m0 = 0;
    let mut m1 = 0;
    // For convenience, explicit propagate the mask into each write
    for &i in prog {
        match i {
            Insn::Mask(ones, zeros) => {m1 = ones; m0 = zeros},
            Insn::Write(addr, val) => {
                let mask = m1 | m0;
                let maddr = (addr as u64 | m1) & mask;
                writes.push(Written{mask:mask, addr:maddr, val:val});
            }
        }
    }
    // Now sum all the written values
    let mut sum = 0;
    for (n, w) in writes.iter().enumerate() {
        sum += try_sum(w, &writes[n+1..]);
    }
    return sum;
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 165);
    //assert_eq!(part2(&v), 1735166787584);
    let v = parse("test2");
    assert_eq!(part2(&v), 208);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
