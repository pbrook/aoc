#! /usr/bin/env python3

def parse(filename):
    with open(filename, "r") as f:
        return set(tuple(map(int, line.strip().split(','))) for line in f)

def add(pos, dim, n):
    return pos[:dim] + (pos[dim] + n,) + pos[dim+1:]

def part1(filename):
    area = 0
    lava = parse(filename)
    for pos in lava:
        for dim in range(3):
            if add(pos, dim, 1) not in lava:
                area += 1
            if add(pos, dim, -1) not in lava:
                area += 1
    return area

assert part1("test1") == 10
assert part1("test2") == 64

print(part1("input"))
