#! /usr/bin/env python3

import itertools

class Elf:
    def __init__(self):
        self.newpos = None

def parse(filename):
    m = {}
    with open(filename, "r") as f:
        for y, line in enumerate(f):
            line = line.strip()
            for x, c in enumerate(line):
                if c == '#':
                    m[x, y] = Elf()
    return m

def pdir(pos, dx, dy):
    x, y = pos
    if dx == 0:
        for dx in [-1, 0, 1]:
            yield (x+dx, y+dy)
    else:
        assert dy == 0
        for dy in [-1, 0, 1]:
            yield (x+dx, y+dy)

def bounds(m):
    x0 = 0
    x1 = 0
    y0 = 0
    y1 = 0
    for x, y in m.keys():
        x0 = min(x, x0)
        x1 = max(x, x1)
        y0 = min(y, y0)
        y1 = max(y, y1)
    return (range(x0, x1+1), range(y0, y1+1))

def dump(m):
    xr, yr = bounds(m)
    for y in yr:
        print("".join('#' if (x, y) in m else '.' for x in xr))


def run(filename):
    ortho = [(0, -1), (0, 1), (-1, 0), (1, 0)]

    m = parse(filename)
    #dump(m)
    part1 = 0
    for count in itertools.count(1):
        proposed = set()
        conflict = set()
        for pos, e in m.items():
            e.newpos = None
            x, y = pos
            skip = True
            for dx in [-1, 0, 1]:
                for dy in [-1, 0, 1]:
                    if dx == 0 and dy == 0:
                        continue
                    if (x+dx, y+dy) in m:
                        skip = False
            if skip:
                continue
            #print(f"{pos}->")
            for dx, dy in ortho:
                #print(f" {dx} {dy} {list(pdir(pos, dx, dy))}")
                if all(np not in m for np in pdir(pos, dx, dy)):
                    e.newpos = (x+dx, y+dy)
                    #print(f" {e.newpos}")
                    if e.newpos in proposed:
                        conflict.add(e.newpos)
                    else:
                        proposed.add(e.newpos)
                    break
        nm = {}
        for pos, e in m.items():
            np = e.newpos
            if np is not None and np not in conflict:
                pos = np
            nm[pos] = e
        m = nm
        ortho = ortho[1:] + [ortho[0]]
        #print(count)
        #dump(m)
        if count == 10:
            xr, yr = bounds(m)
            part1 = len(xr) * len(yr) - len(m)
        if len(proposed) == 0:
            break
    print(part1, count)
    return (part1, count)

assert run("test0") == (0, 4)
assert run("test1") == (110, 20)

print(run("input"))
