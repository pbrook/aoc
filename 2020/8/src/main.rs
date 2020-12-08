use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug)]
enum Insn {
    NOP(),
    ACC(i32),
    JMP(i32),
}

struct Cpu<'a> {
    pc: usize,
    acc: i32,
    prog: &'a Vec<Insn>,
}

impl Cpu<'_> {
    fn new(prog: &Vec<Insn>) -> Cpu {
        return Cpu {pc: 0, acc: 0, prog:prog};
    }

    fn step(&mut self) {
        let insn = &self.prog[self.pc];
        //println!("#{} {:?}", self.pc, insn);
        self.pc += 1;
        match insn {
            Insn::NOP() => (),
            Insn::ACC(arg) => self.acc += arg,
            Insn::JMP(arg) => self.pc = (self.pc as i32 + arg - 1) as usize,
        }
    }
}

fn parse_line(s: String) -> Insn {
    let arg = s[4..].parse().unwrap();
    match &s[0..3] {
        "nop" => Insn::NOP(),
        "acc" => Insn::ACC(arg),
        "jmp" => Insn::JMP(arg),
        bad => panic!("Bad op {}", bad),
    }
}

fn parse(filename: &str) -> Vec<Insn> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect()
}


fn part1(prog: &Vec<Insn>) -> i32 {
    let mut cpu = Cpu::new(prog);
    let mut seen = vec![false; prog.len()];

    while !seen[cpu.pc] {
        seen[cpu.pc] = true;
        cpu.step();
    }
    return cpu.acc;
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 5);
}
fn main() {
    test();

    let v = parse("input");
    println!("{}", part1(&v));
}
