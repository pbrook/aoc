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

fn test() {
    assert_eq!(part1("389125467"), "67384529");
}

fn main() {
    test();
    println!("{}", part1("685974213"));
}
