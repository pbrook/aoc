use std::fs::File;
use std::io::{self, BufRead};

type Slice = Vec<Vec<bool>>;
type Cube = Vec<Slice>;
type HCube = Vec<Cube>;

fn parse_line(s: String) -> Vec<bool>{
    return s.chars().map(|c| c == '#').collect();
}

fn parse(filename: &str) -> Slice {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    return lines.map(|v| parse_line(v.unwrap())).collect();
}

fn read_cell(c: &Cube, x: usize, y: usize, z: usize) -> bool {
    if x <= 1 || y <= 1 {
        return false;
    }
    let nb = c.get(z).and_then(|slice| slice.get(y-2).and_then(|row| row.get(x-2)));
    return *nb.unwrap_or(&false);
}

fn count_near(c: &Cube, x0: usize, y0: usize, z0: usize) -> u32 {
    let mut count = 0;
    for dz in 0..3 {
        let z = if z0 + dz == 0 {1} else {z0 + dz - 1};
        for dy in 0..3 {
            let y = y0 + dy;
            for dx in 0..3 {
                let x = x0 + dx;
                if dx == 1 && dy == 1 && dz == 1 {
                    continue;
                }
                if read_cell(c, x, y, z) {
                    count += 1;
                }
            }
        }
    }
    return count;
}

fn step(c: &Cube) -> Cube {
    let zmax = c.len() + 1;
    let ymax = c[0].len() + 2;
    let xmax = c[0][0].len() + 2;
    let mut result = Vec::with_capacity(zmax);
    for z in 0..zmax {
        let mut slice = Vec::with_capacity(ymax);
        for y in 0..ymax {
            let mut row = Vec::with_capacity(xmax);
            for x in 0..xmax {
                let near = count_near(c, x, y, z); 
                row.push(if read_cell(c, x+1, y+1, z) {
                    near == 2 || near == 3
                } else {
                    near == 3
                })
            }
            slice.push(row);
        }
        result.push(slice);
    }
    return result;
}

fn _dump(c: &Cube) {
    for z in 0..c.len() {
        println!("z={}", z);
        for y in 0..c[0].len() {
            println!("{}", c[z][y].iter().map(|&b| if b {'#'} else {'.'}).collect::<String>());
        }
    }
}

fn part1(input: &Slice) -> usize
{
    let mut world = vec![input.clone()];
    //_dump(&world);
    for _ in 0..6 {
        world = step(&world);
        //_dump(&world);
    }
    let count0 = world[0].iter().flatten().filter(|&&b| b).count();
    let countn = world.iter().flatten().flatten().filter(|&&b| b).count();
    return 2 * countn - count0;
}

fn read_cell4(c: &HCube, x: usize, y: usize, z: usize, w:usize) -> bool {
    if x <= 1 || y <= 1 {
        return false;
    }
    let nb = c.get(w).and_then(|cube| cube.get(z).and_then(|slice| slice.get(y-2).and_then(|row| row.get(x-2))));
    return *nb.unwrap_or(&false);
}

fn count_near4(c: &HCube, x0: usize, y0: usize, z0: usize, w0: usize) -> u32 {
    let mut count = 0;
    for dw in 0..3 {
        let w = if w0 + dw == 0 {1} else {w0 + dw - 1};
        for dz in 0..3 {
            let z = if z0 + dz == 0 {1} else {z0 + dz - 1};
            for dy in 0..3 {
                let y = y0 + dy;
                for dx in 0..3 {
                    let x = x0 + dx;
                    if dx == 1 && dy == 1 && dz == 1 && dw == 1 {
                        continue;
                    }
                    if read_cell4(c, x, y, z, w) {
                        count += 1;
                    }
                }
            }
        }
    }
    return count;
}

fn step4(c: &HCube) -> HCube {
    let wmax = c.len() + 1;
    let zmax = c[0].len() + 1;
    let ymax = c[0][0].len() + 2;
    let xmax = c[0][0][0].len() + 2;
    let mut result = Vec::with_capacity(wmax);
    for w in 0..wmax {
        let mut cube = Vec::with_capacity(zmax);
        for z in 0..zmax {
            let mut slice = Vec::with_capacity(ymax);
            for y in 0..ymax {
                let mut row = Vec::with_capacity(xmax);
                for x in 0..xmax {
                    let near = count_near4(c, x, y, z, w); 
                    row.push(if read_cell4(c, x+1, y+1, z, w) {
                        near == 2 || near == 3
                    } else {
                        near == 3
                    })
                }
                slice.push(row);
            }
            cube.push(slice);
        }
        result.push(cube);
    }
    return result;
}

fn _dump4(c: &HCube) {
    for w in 0..c.len() {
    for z in 0..c[0].len() {
        println!("z={}, w={}", z, w);
        for y in 0..c[0][0].len() {
            println!("{}", c[w][z][y].iter().map(|&b| if b {'#'} else {'.'}).collect::<String>());
        }
    }
    }
}

fn part2(input: &Slice) -> usize
{
    let mut world = vec![vec![input.clone()]];
    //_dump4(&world);
    for _ in 0..6 {
        world = step4(&world);
        //_dump4(&world);
    }
    let mut count1 = 0;
    let mut count2 = 0;
    let mut count4 = 0;
    for w in 0..world.len() {
        for z in 0..world[0].len() {
            let count = world[w][z].iter().flatten().filter(|&&b| b).count();
            if w == 0 && z == 0 {
                count1 += count;
            } else if w != 0 && z != 0 {
                count4 += count;
            } else {
                count2 += count;
            }
        }
    }
    return count1 + 2 * count2 + 4 * count4;
}

fn test() {
    let v = parse("test");
    assert_eq!(part1(&v), 112);
    assert_eq!(part2(&v), 848);
}

fn main() {
    test();
    let v = parse("input");
    println!("{}", part1(&v));
    println!("{}", part2(&v));
}
