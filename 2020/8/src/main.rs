use std::fs::File;
use std::io::{self, BufRead};

#[derive(Debug,PartialEq,Copy,Clone)]
enum Insn {
    NOP(i32),
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
            Insn::NOP(_) => (),
            Insn::ACC(arg) => self.acc += arg,
            Insn::JMP(arg) => self.pc = (self.pc as i32 + arg - 1) as usize,
        }
    }
}

fn parse_line(s: String) -> Insn {
    let arg = s[4..].parse().unwrap();
    match &s[0..3] {
        "nop" => Insn::NOP(arg),
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


fn run_prog(prog: &Vec<Insn>) -> Result<i32, i32> {
    let prog_len = prog.len();
    let mut cpu = Cpu::new(prog);
    let mut seen = vec![false; prog_len];

    loop {
        if cpu.pc == prog_len {
            return Ok(cpu.acc);
        }
        if seen[cpu.pc] {
            return Err(cpu.acc);
        }
        seen[cpu.pc] = true;
        cpu.step();
    }
}

fn part1(prog: &Vec<Insn>) -> i32 {
    return run_prog(prog).unwrap_err();
}

fn part2(prog: &mut Vec<Insn>) -> i32 {
    for n in 0..prog.len() {
        let orig_insn = prog[n];
        let new_insn = match orig_insn {
            Insn::NOP(arg) => Insn::JMP(arg),
            Insn::JMP(arg) => Insn::NOP(arg),
            other => other,
        };
        if orig_insn == new_insn {
            continue;
        }
        prog[n] = new_insn;
        match run_prog(prog) {
            Ok(acc) => return acc,
            _ => (),
        }
        prog[n] = orig_insn;
    }
    panic!("Never terminated");
}

fn test() {
    let mut v = parse("test");
    assert_eq!(part1(&v), 5);
    assert_eq!(part2(&mut v), 8);
}

fn main() {
    test();

    let mut v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&mut v));
}
