#! /usr/bin/env python3

BIG = 9999999

ortho = [(1, 0), (-1, 0),  (0, 1), (0, -1)]

class Grid:
    def __init__(self, filename):
        self.grid = []
        with open(filename, "r") as f:
            for row in f:
                self.grid.append([c for c in row])
        self.part1 = BIG
        self.best = {}

    def find(self, sym):
        for y, row in enumerate(self.grid):
            for x, c in enumerate(row):
                if c == sym:
                    return (x, y)
        assert False

    def probe(self, sym):
        stepcount = {}
        distance = {}
        x, y = self.find(sym)
        pending = [(x, y, 0)]
        while pending:
            x, y, steps = pending.pop()
            prev = distance.get((x, y), BIG)
            if steps < prev:
                distance[x, y] = steps
                c = self.grid[y][x]
                if c == '.' or steps == 0:
                    for dx, dy in ortho:
                        pending.append((x+dx, y+dy, steps+1))
                elif c != '#':
                    stepcount[c] = steps
        return stepcount

    def link(self, sym, counts):
        sc = self.probe(sym)
        counts[sym] = sc
        for s in sc.keys():
            if s not in counts:
                self.link(s, counts)


    def remove(self, counts, sym):
        for other, od in counts[sym].items():
            del counts[other][sym]
            for target, td in counts[sym].items():
                if other == target:
                    continue
                if target not in counts[other] or counts[other][target] > od + td:
                    counts[other][target] = od + td;
        del counts[sym]

    def walk1(self, counts, sym, steps, pfx = ""):
        #print(f"Try {pfx}")
        if steps >= self.part1:
            #print(f"toobig")
            return
        key = (frozenset(pfx), sym)
        if self.best.get(key, BIG) <= steps:
            #print(f"Dup {pfx}")
            return
        self.best[key] = steps
        #print(len(counts), counts)
        nc = dict((k, v.copy()) for k, v in counts.items())
        if sym.isalpha() and sym.upper() in nc:
            self.remove(nc, sym.upper())
            #print(f"{nc=}")
        targets = nc[sym]
        #print(f"{targets=}")
        self.remove(nc, sym)
        #print(f"{nc=}")
        if len(nc) == 0:
            #print(f"solved {steps}")
            self.part1 = steps
        for c, dist in targets.items():
            if c.isupper():
                continue
            self.walk1(nc, c, steps+dist, pfx + c)

    def walk(self, counts, syms, steps, pfx = ""):
        #print(f"Try {syms} {pfx}")
        key = (frozenset(pfx), syms)
        if self.best.get(key, BIG) <= steps:
            #print(f"Dup {syms} {pfx}")
            return
        self.best[key] = steps
        for sym in syms:
            targets = counts[sym]
            for c, dist in targets.items():
                if c.isupper():
                    continue
                if steps+dist >= self.part1:
                    #print(f"toobig")
                    return
                nc = dict((k, v.copy()) for k, v in counts.items())
                if c.upper() in nc:
                    self.remove(nc, c.upper())
                #print(f"{sym} {c} {dist}")
                self.remove(nc, sym)
                #print(f"{nc=}")
                if len(nc) == len(syms):
                    #print(f"solved {steps}")
                    self.part1 = steps + dist
                self.walk(nc, syms.replace(sym, c), steps+dist, pfx + sym)

def part1(filename):
    g = Grid(filename)
    counts = {}
    g.link('@', counts)
    g.walk1(counts, '@', 0)
    #print(filename, g.part1)
    return g.part1

def part2(filename):
    g = Grid(filename)
    counts = {}
    x, y = g.find('@')
    if all(g.grid[y+dy][x+dx] == '.' for dx, dy in ortho):
        for dx, dy in ortho:
            g.grid[y+dy][x+dx] = '#'
        for n, (dx, dy) in enumerate([(1,1),(-1,1),(1,-1),(-1,-1)]):
            g.grid[y+dy][x+dx] = str(n+1)
    else:
        for n in range(4):
            x, y = g.find('@')
            g.grid[y][x] = str(n+1)
    for n in range(4):
        g.link(str(n+1), counts)
    #print(counts)
    g.walk(counts, '1234', 0)
    #print(filename, g.part1)
    return g.part1

assert part1("test0") == 8
assert part1("test1") == 86
assert part1("test2") == 132
assert part1("test3") == 136
assert part1("test4") == 81

assert part2("test5") == 8
assert part2("test6") == 24
assert part2("test7") == 32
assert part2("test8") == 72

print(part1("input"))
print(part2("input"))
