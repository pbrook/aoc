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
    next: usize,
    prev: usize,
}

const NUM_SHELLS: usize = 1000000;

fn dec_p2(n: usize) -> usize {
    if n == 0 {
        return NUM_SHELLS - 1;
    }
    return n - 1;
}

fn part2(input: &str) -> u64 {
    let mut shells = Vec::with_capacity(NUM_SHELLS);
    let input_len = input.len();
    shells.push(Shell{next: 1, prev: NUM_SHELLS - 1});
    for n in 1..NUM_SHELLS {
        shells.push(Shell{next: n + 1, prev: n - 1});
    }
    let mut init = input.chars().map(|c| c.to_digit(10).unwrap() as usize - 1);
    let mut cur = init.next().unwrap();
    let mut prev = cur;
    for n in init {
        shells[prev].next = n;
        shells[n].prev = prev;
        prev = n;
    }
    shells[prev].next = input_len;
    shells[input_len].prev = prev;
    shells[cur].prev = NUM_SHELLS - 1;
    shells[NUM_SHELLS - 1].next = cur;

    for _ in 0..10000000 {
        let n1 = shells[cur].next;
        let n2 = shells[n1].next;
        let n3 = shells[n2].next;
        let n4 = shells[n3].next;
        shells[n4].prev = cur;
        shells[cur].next = n4;

        let mut ins = dec_p2(cur);
        while n1 == ins || n2 == ins || n3 == ins {
            ins = dec_p2(ins);
        }

        let next = shells[ins].next;
        shells[n3].next = next;
        shells[next].prev = n3;
        shells[ins].next = n1;
        shells[n1].prev = ins;

        cur = n4;
    }
    let s1 = shells[0].next;
    let s2 = shells[s1].next;
    return (s1 + 1) as u64 * (s2 + 1) as u64;
}

fn test() {
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
