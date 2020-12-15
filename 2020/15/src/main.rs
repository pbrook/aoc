
fn parse(s: &str) -> Vec<usize> {
    return s.split(',').map(|v| v.parse().unwrap()).collect();
}

fn part1(s: &str) -> usize {
    let mut start = parse(s);
    let mut seen = Vec::new();
    let mut turn = 1;
    let mut val = 0;
    start.reverse();
    while turn != 2020 {
        //println!("{} {}", turn, val);
        match start.pop() {
            Some(n) => val = n,
            None => (),
        }
        let mut next = 0;
        if val >= seen.len() {
            seen.resize(val + 1, 0);
        } else {
            let last = seen[val];
            if last != 0 {
                next = turn - last;
            }
        }
        seen[val] = turn;
        turn += 1;
        val = next;
    }
    return val;
}

fn test() {
    assert_eq!(part1("0,3,6"), 436);
    assert_eq!(part1("1,3,2"), 1);
    assert_eq!(part1("2,1,3"), 10);
    assert_eq!(part1("1,2,3"), 27);
    assert_eq!(part1("2,3,1"), 78);
    assert_eq!(part1("3,2,1"), 438);
    assert_eq!(part1("3,1,2"), 1836);
}

fn main() {
    test();
    println!("{}", part1("2,0,1,7,4,14,18"));
}
