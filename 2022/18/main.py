#! /usr/bin/env python3

def parse(filename):
    with open(filename, "r") as f:
        return set(tuple(map(int, line.strip().split(','))) for line in f)

def add(pos, dim, n):
    return pos[:dim] + (pos[dim] + n,) + pos[dim+1:]

def near(pos):
    for dim in range(3):
        yield add(pos, dim, 1)
        yield add(pos, dim, -1)

def cool(filename):
    lava = parse(filename)
    low = [(min(p[i] for p in lava)-1) for i in range(3)]
    high = [(max(p[i] for p in lava)+1) for i in range(3)]
    part1 = 0
    for pos in lava:
        part1 += sum(1 for p in near(pos) if p not in lava)

    air = set()
    start = tuple(low)
    air.add(start)
    pending = [start]
    part2 = 0
    while pending:
        pos = pending.pop()
        for p in near(pos):
            if any(pos[dim] < low[dim] or pos[dim] > high[dim] for dim in range(3)):
                continue
            if p in lava:
                part2 += 1
            elif p not in air:
                air.add(p)
                pending.append(p)

    return part1, part2

assert cool("test1") == (10, 10)
assert cool("test2") == (64, 58)

print(cool("input"))
