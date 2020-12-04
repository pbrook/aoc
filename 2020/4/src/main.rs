use std::fs::File;
use std::io::{self, BufRead};
use std::collections::HashMap;

const MANDATORY_FIELDS: &[&str] = &["byr", "iyr", "eyr", "hgt", "hcl", "ecl", "pid"];
const EYE_COLORS: &[&str] = &["amb", "blu", "brn", "gry", "grn", "hzl", "oth"];

struct Passport {
    values: HashMap<String, String>,
}

fn part1(_key: &str, _val: &str) -> bool {
    true
}

fn year(s: &str, min: u32, max: u32) -> bool {
    if s.len() != 4 {
        return false;
    }
    match s.parse::<u32>() {
        Ok(i) => i >= min && i <= max,
        _ => false,
    }
}

fn valid_height(s: &str) -> bool {
    let n = s.len() - 2;
    if n <= 0 {
        return false;
    }
    let unit = &s[n..];
    let v = s[0..n].parse::<u32>().unwrap();
    if unit == "in" {
        v >= 59 && v <= 76
    } else if unit == "cm" {
        v >= 150 && v <= 193
    } else {
        false
    }
}

fn valid_hcl(s: &str) -> bool {
    if ! s.starts_with('#') {
        return false;
    }
    if s.len() != 7 {
        return false;
    }
    s[1..].chars().all(|c| c.is_ascii_hexdigit())
}
fn valid_pid(s: &str) -> bool {
    if s.len() != 9 {
        return false;
    }
    s.chars().all(|c| c.is_ascii_digit())
}

fn valid_ecl(s: &str) -> bool {
    return EYE_COLORS.iter().any(|c| *c == s);
}

fn part2(key: &str, val: &str) -> bool {
    match key {
        "byr" => year(val, 1920, 2002),
        "iyr" => year(val, 2010, 2020),
        "eyr" => year(val, 2020, 2030),
        "hgt" => valid_height(val),
        "ecl" => valid_ecl(val),
        "hcl" => valid_hcl(val),
        "pid" => valid_pid(val),
        _ => true,
    }
}

impl Passport {
    fn blank() -> Passport {
        return Passport {values:HashMap::new()};
    }

    fn valid(&self) -> bool {
        for &f in MANDATORY_FIELDS {
            if !self.values.contains_key(&f.to_string()) {
                return false;
            }
        }
        return true;
    }

    fn add(&mut self, key:&str, val:&str) {
        self.values.insert(key.to_string(), val.to_string());
    }
}

fn parse(filename: &str, check: fn(&str, &str)->bool) -> u32 {
    let mut result = 0;
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut p = Passport::blank();
    for s in lines.map(|v| v.unwrap()) {
        if s == "" {
            if p.valid() {
                result += 1;
            }
            p = Passport::blank();
            continue
        }
        for kv in s.split(' ') {
            let key = &kv[0..3];
            let raw_val = &kv[3..];
            assert!(raw_val.starts_with(':'));
            let val = &raw_val[1..];
            if check(key, val) {
                p.add(key, val);
            }
        }
    }
    // No blank line after the last entry
    if p.valid() {
        result += 1;
    }
    return result;
}

fn main() {
    assert!(parse("test", part1) == 2);
    assert!(parse("test2", part2) == 4);
    println!("{}", parse("input", part1));
    println!("{}", parse("input", part2));
}
