use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;
use std::rc::Rc;
use std::cell::RefCell;

#[macro_use] extern crate lazy_static;
extern crate regex;

use regex::Regex;

type BagRef = Rc<RefCell<Bag>>;
type BagHash = HashMap<String, BagRef>;

struct BagCount {
    bag: BagRef,
    count: u32,
}

#[derive(Default)]
struct Bag {
    shiny: bool,
    size: Option<u32>,
    holds: Vec<BagCount>,
}

fn find_bag(name: String, h: &mut BagHash) -> BagRef {
    h.entry(name).or_default().clone()
}

fn parse_line(s: String, h: &mut BagHash) -> BagRef {
    lazy_static! {
        static ref NAME_RE: Regex = Regex::new(r"(.*) bags contain").unwrap();
        static ref HOLDS_RE: Regex = Regex::new(r"(\d+) ([^,]*) bag").unwrap();
    }
    let name = NAME_RE.captures(&s).unwrap()[1].to_string();
    let bag = find_bag(name, h);
    bag.borrow_mut().holds =
        HOLDS_RE.captures_iter(&s).map(
        |c| BagCount{count:c[1].parse::<u32>().unwrap(), bag: find_bag(c[2].to_string(), h)}
        ).collect();
    return bag;
}

fn parse(filename: &str) -> (u32, u32) {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut h = HashMap::new();
    for v in lines {
        parse_line(v.unwrap(), &mut h);
    }

    let gold = &h["shiny gold"];
    // could release hashmap here, we just need to be able to interate over
    // all the bags
    let mut scount = 0;
    let mut again = true;
    gold.borrow_mut().shiny = true;

    while again {
        again = false;
        for outer_ref in h.values() {
            let mut outer = outer_ref.borrow_mut();
            if !outer.shiny {
                if outer.holds.iter().any(|inner| inner.bag.borrow().shiny) {
                    outer.shiny = true;
                    again = true;
                    scount += 1;
                }
            }
            if outer.size.is_none() {
                let mut size: Option<u32> = Some(0);

                for held in &outer.holds {
                    let inner = held.bag.borrow();
                    if inner.size.is_none() {
                        size = None;
                        break;
                    }
                    size = size.map(|n| n + (inner.size.unwrap() + 1) * held.count);
                }
                outer.size = size;
            }
        }
        if gold.borrow().size.is_none() {
            again = true;
        }
    }
    return (scount, gold.borrow().size.unwrap());
}


fn test() {
    assert_eq!(parse("test"), (4, 32));
    assert_eq!(parse("test2"), (0, 126));
}

fn main() {
    test();
    let (p1, p2) = parse("input");
    println!("{}", p1);
    println!("{}", p2);
}
