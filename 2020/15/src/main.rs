
fn parse(s: &str) -> Vec<usize> {
    return s.split(',').map(|v| v.parse().unwrap()).collect();
}

const P1_ROUNDS: u32 = 2020;
const P2_ROUNDS: u32 = 30000000;

fn play(s: &str) -> (u32, u32) {
    let start = parse(s);
    let mut seen = vec![0; P2_ROUNDS as usize];
    let mut turn = 1;
    let mut val = 0;
    let mut p1 = 0;
    // assume there are no duplicates in the starting sequence
    for n in start {
        seen[n] = turn;
        turn += 1;
    }
    while turn != P2_ROUNDS {
        if turn == P1_ROUNDS {
            p1 = val;
        }
        let mut next = 0;
        let last = seen[val as usize];
        if last != 0 {
            next = turn - last;
        }
        seen[val as usize] = turn;
        turn += 1;
        val = next;
    }
    return (p1, val);
}

fn test() {
    if cfg!(benchmark) {
        return;
    }
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
