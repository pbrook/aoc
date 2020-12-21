use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashSet;
use std::collections::HashMap;

type SHash = HashSet<String>;

#[derive(Debug,Default)]
struct Food {
    ingredients: SHash,
    allergens: SHash,
}

fn parse_line(s: String) -> Food {
    assert!(s.ends_with(')'));
    let mut res = Food::default();
    let ci = s.find("(contains").unwrap();
    for i in s[..ci].trim().split(' ') {
        res.ingredients.insert(i.to_string());
    }
    for a in s[ci+9..s.len()-1].split(',').map(|s| s.trim()) {
        res.allergens.insert(a.to_string());
    }
    return res;
}

fn parse(filename: &str) -> Vec<Food> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

type Ingredient<'a> = &'a String;
type Allergen<'a> = &'a String;

fn part1(input: &Vec<Food>) -> usize {
    let mut map: HashMap<Allergen, HashSet<Ingredient>> = HashMap::new();
    let mut safe: HashSet<Ingredient> = HashSet::new();

    for food in input {
        let ing: HashSet<Ingredient> = food.ingredients.iter().collect();
        for a in &food.allergens {
            map.entry(a)
                .and_modify(|e| *e = &*e & &ing)
                .or_insert_with(|| ing.clone());
        }
        safe = &safe | &ing;
    }
    while !map.is_empty() {
        let mut known: Vec<Allergen> = Vec::new();
        for (&a, ings) in map.iter() {
            if ings.len() == 1 {
                safe.remove(ings.iter().next().unwrap());
                known.push(a);
            }
        }
        assert!(known.len() > 0);
        for a in known {
            map.remove(a);
        }
        for (_, ings) in map.iter_mut() {
            *ings = &*ings & &safe;
        }
    }
    let mut count = 0;
    for food in input {
        for ing in &food.ingredients {
            if safe.contains(ing) {
                count += 1;
            }
        }
    }
    return count;
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
