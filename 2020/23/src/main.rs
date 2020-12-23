use std::char;
use std::cell::RefCell;

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

type ShellPtr<'a> = Option<&'a RefCell<Shell<'a>>>;

struct Shell<'a> {
    val: usize,
    next: ShellPtr<'a>,
    prev: ShellPtr<'a>,
}

const NUM_SHELLS: usize = 1000000;

impl Shell<'_> {
    fn new<'a>(val: usize) -> Shell<'a> {Shell{val: val, next:None, prev:None}}
}

fn dec_p2(n: usize) -> usize {
    if n == 0 {
        return NUM_SHELLS - 1;
    }
    return n - 1;
}

fn part2(input: &str) -> u64 {
    let mut shells = Vec::with_capacity(NUM_SHELLS);
    for n in 0..NUM_SHELLS {
        shells.push(RefCell::new(Shell::new(n)));
    }
    let mut init = input.chars().map(|c| c.to_digit(10).unwrap() as usize - 1);
    let n0 = init.next().unwrap();
    let mut cur = &shells[n0];
    let mut prev = cur;
    for n in init.chain(input.len()..NUM_SHELLS) {
        let next = &shells[n];
        next.borrow_mut().prev = Some(prev);
        prev.borrow_mut().next = Some(next);
        prev = next;
    }
    prev.borrow_mut().next = Some(cur);
    cur.borrow_mut().prev = Some(prev);

    for _ in 0..10000000 {
        let n1 = cur.borrow().next;
        let mut n1b = n1.unwrap().borrow_mut();
        let n2 = n1b.next;
        let n2b = n2.unwrap().borrow();
        let n3 = n2b.next;
        let mut n3b = n3.unwrap().borrow_mut();
        let n4 = n3b.next;
        n4.unwrap().borrow_mut().prev = Some(cur);
        cur.borrow_mut().next = n4;

        let mut val = dec_p2(cur.borrow().val);
        while n1b.val == val || n2b.val == val || n3b.val == val {
            val = dec_p2(val);
        }

        let ins = &shells[val];
        n3b.next = ins.borrow().next;
        n3b.next.unwrap().borrow_mut().prev = n3;
        ins.borrow_mut().next = n1;
        n1b.prev = Some(ins);
        cur = n4.unwrap();
    }
    let n1 = shells[0].borrow();
    let n2 = n1.next.unwrap().borrow();
    let n3 = n2.next.unwrap().borrow();
    return (n2.val + 1) as u64 * (n3.val + 1) as u64;
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
