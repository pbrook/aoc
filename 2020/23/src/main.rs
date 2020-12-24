use std::char;

fn parse(s: &str) -> Vec<u8> {
    return s.chars().map(|c| c.to_digit(10).unwrap() as u8).collect();
}

fn dec(x: u8) -> u8 {
    if x <= 1 {
        return 9;
    }
    return x - 1;
}

fn push_many(v: &mut Vec<u8>, add: &[u8]) {
    for &x in add {
        v.push(x);
    }
}

fn part1(input: &str) -> String {
    let mut v = parse(input);

    for _ in 0..100 {
        //println!("{:?}", v);
        let mut result: Vec<u8> = Vec::with_capacity(9);
        let mut dest = v[0];
        let mut pos = 0;
        while pos < 4 {
            dest = dec(dest);
            for (n, &c) in v.iter().enumerate() {
                if c == dest {
                    pos = n;
                    break;
                }
            }
        }
        push_many(&mut result, &v[4..=pos]);
        push_many(&mut result, &v[1..4]);
        push_many(&mut result, &v[(pos + 1)..]);
        result.push(v[0]);
        v = result;
    }
    while v[0] != 1 {
        v.rotate_left(1);
    }
    return v[1..].iter().map(|&n| char::from_digit(n as u32, 10).unwrap()).collect();
}

struct Shell {
    next: u32,
}

const NUM_SHELLS: u32 = 1000000;

fn dec_p2(n: u32) -> u32 {
    if n == 0 {
        return NUM_SHELLS - 1;
    }
    return n - 1;
}

fn part2(input: &str) -> u64 {
    let mut shells = Vec::with_capacity(NUM_SHELLS as usize);
    macro_rules! shell {
        ($n:expr) => {shells[($n) as usize]};
    }
    let input_len = input.len() as u32;
    for n in 0..NUM_SHELLS {
        shells.push(Shell{next: n + 1});
    }
    let mut init = input.chars().map(|c| c.to_digit(10).unwrap() as u32 - 1);
    let mut cur = init.next().unwrap();
    let mut prev = cur;
    for n in init {
        shell!(prev).next = n;
        prev = n;
    }
    shell!(prev).next = input_len;
    shell!(NUM_SHELLS - 1).next = cur;

    for _ in 0..10000000 {
        let n1 = shell!(cur).next;
        let n2 = shell!(n1).next;
        let n3 = shell!(n2).next;
        let n4 = shell!(n3).next;
        shell!(cur).next = n4;

        let mut ins = dec_p2(cur);
        while n1 == ins || n2 == ins || n3 == ins {
            ins = dec_p2(ins);
        }

        let next = shell!(ins).next;
        shell!(n3).next = next;
        shell!(ins).next = n1;

        cur = n4;
    }
    let s1 = shell!(0).next;
    let s2 = shell!(s1).next;
    return (s1 + 1) as u64 * (s2 + 1) as u64;
}

fn test() {
    if cfg!(benchmark) {
        return;
    }
    let v = "389125467";
    assert_eq!(part1(v), "67384529");
    assert_eq!(part2(v), 149245887792);
}

fn main() {
    test();
    let v = "685974213";
    println!("{}", part1(v));
    println!("{}", part2(v));
}
