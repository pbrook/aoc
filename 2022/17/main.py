#! /usr/bin/env python3

import itertools

def s2n(s):
    n = 0
    for i, c in enumerate(s):
        if c == '#':
            n |= 1 << i
        else:
            assert c == '.'
    return n

class Shape():
    def __init__(self, pattern):
        self.p = []
        for s in pattern.split():
            self.p.append(s2n(s))
        self.width = len(s)
        self.p.reverse()

    def fit(self, pit, sx, sy):
        if sx < 0 or sx + self.width > PW or sy < 0:
            return False
        for y, pn in enumerate(pit[sy:sy + len(self.p)]):
            if (pn & (self.p[y] << sx)) != 0:
                return False
        return True

    def __repr__(self):
        return f"<{self.p}>"

def makeshapes():
    pattern = """
####

.#.
###
.#.

..#
..#
###

#
#
#
#

##
##
"""
    ns = [Shape(p) for p in pattern.split("\n\n")]
    return (itertools.cycle(ns), len(ns))

def parse(filename):
    with open(filename, "r") as f:
        m = [(1 if c == '>' else -1) for c in f.read().strip()]
    return (itertools.cycle(m), len(m))

PW = 7

def dump(pit):
    for n in reversed(pit):
        s = "".join(('#' if ((n & (1 << i)) != 0) else '.') for i in range(7))
        print(s)

class World:
    def __init__(self, filename):
        self.shapes, self.shapecount = makeshapes()
        self.moves, self.movecount = parse(filename)
        self.movepos = 0
        self.pit = []

    def run(self, count):
        pit = self.pit
        for _ in range(count):
            sx = 2
            sy = len(pit) + 3
            s = next(self.shapes)
            while True:
                nx = sx + next(self.moves)
                self.movepos = (self.movepos + 1) % self.movecount
                if s.fit(pit, nx, sy):
                    sx = nx
                if s.fit(pit, sx, sy - 1):
                    sy -= 1
                else:
                    break
            for n in s.p:
                n <<= sx
                if len(pit) <= sy:
                    pit.append(n)
                else:
                    pit[sy] |= n
                sy += 1
            #print(_)
            #dump(pit)
        return len(pit)

def run(filename):
    w = World(filename)
    part1 = w.run(2022)
    cur = part1
    results = []
    total = 2022
    for _ in range(10):
        prev = cur
        mp = w.movepos
        tp = total
        while True:
            total += w.shapecount
            cur = w.run(w.shapecount)
            if mp == w.movepos:
                break;
        results.append((total - tp, cur - prev))
    assert all(r == results[0] for r in results)

    block_s, block_h = results[0]

    remain = 1000000000000 - total
    nblocks = remain // block_s
    offset = remain % block_s
    if offset > 0:
        cur = w.run(offset)
    part2 = cur + nblocks * block_h

    return part1, part2

assert run("test1") == (3068, 1514285714288)

print(run("input"))
