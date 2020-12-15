
fn parse(s: &str) -> Vec<usize> {
    return s.split(',').map(|v| v.parse().unwrap()).collect();
}

fn play(s: &str) -> (usize, usize) {
    let start = parse(s);
    let mut seen = Vec::new();
    let mut turn = 1;
    let mut val = 0;
    let mut p1 = 0;
    let start_max = start.iter().max().unwrap();
    seen.resize(start_max + 1, 0);
    // assume there are no duplicates in the starting sequence
    for n in start {
        seen[n] = turn;
        turn += 1;
    }
    while turn != 30000000 {
        if turn == 2020 {
            p1 = val;
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
    return (p1, val);
}

fn test() {
    assert_eq!(play("0,3,6"), (436, 175594));
    assert_eq!(play("1,3,2"), (1, 2578));
    assert_eq!(play("2,1,3"), (10, 3544142));
    assert_eq!(play("1,2,3"), (27, 261214));
    assert_eq!(play("2,3,1"), (78, 6895259));
    assert_eq!(play("3,2,1"), (438, 18));
    assert_eq!(play("3,1,2"), (1836, 362));
}

fn main() {
    test();
    let (p1, p2) = play("2,0,1,7,4,14,18");
    println!("{}", p1);
    println!("{}", p2);
}
