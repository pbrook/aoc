use std::fs::File;
use std::io::{self, BufRead};
use std::cell::RefCell;
use std::rc::Rc;

const TILE_SIZE: usize = 10;

type TileRef = Rc<RefCell<Tile>>;
type TileLink = Option<(TileRef, u8)>;

#[derive(Debug)]
struct Tile {
    id: u64,
    data: Vec<u16>,
    link: [TileLink; 4],
}

fn parse_line(s: String) -> u16 {
    let mut val = 0;
    assert!(s.len() == TILE_SIZE);
    for c in s.chars() {
        val <<= 1;
        match c {
            '#' => val |= 1,
            '.' => (),
            _ => panic!("bad tile pixel: {}", c),
        }
    }
    return val;
}

fn parse(filename: &str) -> Vec<TileRef> {
    let file = File::open(filename).unwrap();
    let lines = io::BufReader::new(file).lines();
    let mut tiles = Vec::new();
    let mut data = Vec::new();
    let mut id = 0;
    for s in lines.map(|v| v.unwrap()) {
        if s.ends_with(':') {
            assert!(id == 0);
            id = s[5..s.len()-1].parse().unwrap();
            continue;
        }
        assert!(id != 0);
        if s.len() == 0 {
            assert!(data.len() == TILE_SIZE);
            let t = Tile{id: id, data: data, link: Default::default()};
            tiles.push(Rc::new(RefCell::new(t)));
            data = Vec::new();
            id = 0;
            continue;
        }
        data.push(parse_line(s));
    }
    return tiles;
}

/*
      0/4
     +---+
 3/7 |<4>| 1/5
     +---+
      2/6
*/
struct Edge {
    tile: TileRef,
    rot: u8,
    val: u16,
}

fn canon_edge(t: &TileRef, rot: u8, val: u16) -> Edge {
    let mut e = Edge{tile: Rc::clone(t), rot, val: val};
    let flip = val.reverse_bits() >> (16 - TILE_SIZE);
    if flip < val {
        e.val = flip;
        e.rot ^= 4;
    }
    return e;
}

fn link_tile(from: &Edge, other: &Edge) {
    let flip = (from.rot ^ other.rot ^ 4) & 4;
    let mut t = from.tile.borrow_mut();
    let linkp = &mut t.link[(from.rot & 3) as usize];
    assert!(linkp.is_none());
    *linkp = Some((Rc::clone(&other.tile), (other.rot & 3) | flip));
}

#[derive(Clone)]
struct TileWalker {
    tile: TileRef,
    rot: u8,
}

fn _dump_tile(t: &Tile) {
    for l in &t.link {
        match l {
            None => print!{"None"},
            Some((tl, tr)) => print!("({}, {})", tl.borrow().id, tr),
         }
    }
    println!("");
}

impl TileWalker {
    fn new(start: &TileRef, rot: u8) -> TileWalker {
        TileWalker{tile: Rc::clone(start), rot: rot}
    }
    fn walk(&self, dir: u8) -> Option<TileWalker> {
        let t = self.tile.borrow();
        //println!(">{} {} {}", dir, t.id, self.rot);
        //_dump_tile(&*t);
        let mut r = (dir + 8 - self.rot) & 3;
        if (self.rot & 4) != 0 && (r & 1) == 1 {
            r ^= 2;
        }
        match &t.link[r as usize] {
            None => None,
            Some((lt, lr)) => {
                let flip = (self.rot ^ lr) & 4;
                let mut new_rot = flip | ((dir + 2 + 8 - lr) & 3);
                if flip != 0 && (lr & 1) == 1 {
                    new_rot ^= 2;
                }
                Some(TileWalker{tile: Rc::clone(lt), rot: flip | new_rot})
            }
        }
    }
    fn get_bits(&self) -> [u8; 8] {
        let t = self.tile.borrow();
        assert!(TILE_SIZE == 10);
        let mut val = [0; 8];
        for (count, &bits) in t.data[1..=8].iter().enumerate() {
            if (self.rot & 1) == 1 {
                for bnum in 0..8 {
                    val[bnum] |= (((bits >> (8-bnum)) & 1) << count) as u8;
                }
            } else {
                val[count] = ((bits >> 1) & 0xff) as u8;
            }
        }
        let mut flip_h = false;
        let mut flip_v = false;
        if (self.rot & 4) != 0 {
            if (self.rot & 1) == 0 {
                flip_h = true;
            } else {
                flip_v = true;
            }
        }
        if (self.rot & 2) != 0 {
            flip_h = !flip_h;
            flip_v = !flip_v;
        }

        if flip_v {
            val.reverse();
        }
        if flip_h {
            for p in val.iter_mut() {
                *p = p.reverse_bits();
            }
        }
        return val;
    }
}

fn rot_corner(corner: &TileRef, flip: bool) -> u8 {
    let t = corner.borrow();
    if flip {
        if t.link[0].is_none() {
            if t.link[1].is_none() {
                return 4;
            } else {
                return 7;
            }
        } else {
            if t.link[1].is_none() {
                return 5;
            } else {
                return 6;
            }
        }
    } else {
        if t.link[0].is_none() {
            if t.link[3].is_none() {
                return 0;
            } else {
                return 3;
            }
        } else {
            if t.link[3].is_none() {
                return 1;
            } else {
                return 2;
            }
        }
    }
}

fn monster(flip: bool) -> Vec<u128> {
    let txt = [
        "                  # ",
        "#    ##    ##    ###",
        " #  #  #  #  #  #   ",
    ];
    let mut result = Vec::new();
    for s in &txt {
        let mut bits: u128 = 0;
        for c in s.chars() {
            bits <<= 1;
            if c == '#' {
                bits |= 1;
            }
        }
        if flip {
            bits = bits.reverse_bits() >> (128 - s.len());
        }
        result.push(bits);
    }
    return result;
}

fn _dump_pic(pic: &Vec<u128>) {
    for row in pic {
        let mut mask = 1<< 23;
        for _ in 0..24 {
            print!("{}", if (row & mask) != 0 {'#'} else {'.'});
            mask >>= 1;
        }
        println!("");
    }
}

fn count_monsters(pic: &Vec<u128>, nessie: &Vec<u128>) -> u32 {
    let mut count = 0;
    for start in 0..pic.len() - 3 {
        for n in 0..128 {
            if nessie.iter().zip(&pic[start..]).all(|(&m, &p)| m & (p >> n) == m) {
                //println!("M {} {}", start, n);
                count += 1;
            }
        }
    }
    return count;
}

fn render(corner: &TileRef, flip: bool) -> Vec<u128> {
    let mut w = TileWalker::new(corner, rot_corner(corner, flip));
    const TILE_BITS: usize = 8;
    assert!(TILE_BITS == 8);
    let mut pic = Vec::new();
    loop {
        let mut rows = [0; TILE_BITS];
        let mut p = w.clone();
        loop {
            {
                let bits = p.get_bits();
                for n in 0..TILE_BITS {
                    rows[n] <<= TILE_BITS;
                    rows[n] |= bits[n] as u128;
                }
            }
            match p.walk(1) {
                None => break,
                Some(next) => p = next,
            }
        }
        for &r in &rows {
            pic.push(r);
        }
        match w.walk(2) {
            None => break,
            Some(next) => w = next,
        }
    }

    //_dump_pic(&pic);
    return pic;
}

fn find_corners(tiles: &Vec<TileRef>) -> Vec<&TileRef> {
    let mut edges = Vec::with_capacity(tiles.len()*4);
    const HIGHBIT: u16 = 1 << (TILE_SIZE - 1);

    for t in tiles {
        let mut left = 0;
        let mut right = 0;
        let data = &t.borrow().data;

        edges.push(canon_edge(t, 0, data[0]));
        edges.push(canon_edge(t, 6, data[TILE_SIZE - 1]));
        for v in data {
            left >>= 1;
            left |= v & HIGHBIT;
            right <<= 1;
            right |= v & 1;
        }
        edges.push(canon_edge(t, 1, right));
        edges.push(canon_edge(t, 3, left));
    }

    edges.sort_unstable_by_key(|e| e.val);
    let mut skip = false;
    let mut prev = &edges[0];
    for e in &edges[1..] {
        if skip {
            assert!(e.val != prev.val);
            skip = false;
        } else {
            if e.val == prev.val {
                link_tile(e, prev);
                link_tile(prev, e);
                skip = true;
            }
        }
        prev = e;
    }
    let corners: Vec<_> = tiles.iter().filter(|t|
        t.borrow().link.iter().filter(|l| l.is_none()).count() == 2
        ).collect();
    assert_eq!(corners.len(), 4);
    return corners;
}

fn search(corner: &TileRef, flip: bool) -> u32 {
    let pic = &render(corner, flip);
    let mut nessie = monster(false);
    let mut count = Vec::new();
    count.push(count_monsters(pic, &nessie));
    nessie.reverse();
    count.push(count_monsters(pic, &nessie));
    nessie = monster(true);
    count.push(count_monsters(pic, &nessie));
    nessie.reverse();
    count.push(count_monsters(pic, &nessie));
    assert!(count.iter().filter(|&&n| n != 0).count() <= 1);

    let ncount: u32 = count.iter().sum();
    if ncount == 0 {
        return 0;
    }
    let nsize: u32 = nessie.iter().map(|&v| v.count_ones()).sum();
    let picsize: u32 = pic.iter().map(|&v| v.count_ones()).sum();
    return picsize - nsize * ncount;
}

fn solve(tiles: &Vec<TileRef>) -> (u64, u32) {
    let corners = find_corners(tiles);

    let part1 = corners.iter().map(|t| t.borrow().id).product();

    let n1 = search(&corners[0], true);
    let n2 = search(&corners[0], false);

    assert!(n1 == 0 || n2 == 0);
    
    return (part1, n1 + n2);
}

fn test() {
    let v = parse("test");
    assert_eq!(solve(&v), (20899048083289, 273));
}

fn main() {
    test();
    let v = parse("input");
    let (p1, p2) = solve(&v);
    println!("{}", p1);
    println!("{}", p2);
}
