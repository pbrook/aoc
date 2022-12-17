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
    return itertools.cycle(ns)

def parse(filename):
    with open(filename, "r") as f:
        m = [(1 if c == '>' else -1) for c in f.read().strip()]
        return itertools.cycle(m)

PW = 7

def dump(pit):
    for n in reversed(pit):
        s = "".join(('#' if ((n & (1 << i)) != 0) else '.') for i in range(7))
        print(s)

def part1(filename):
    shapes = makeshapes()
    moves = parse(filename)
    pit = []
    for _ in range(2022):
        sx = 2
        sy = len(pit) + 3
        s = next(shapes)
        while True:
            nx = sx + next(moves)
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

assert part1("test1") == 3068

print(part1("input"))
