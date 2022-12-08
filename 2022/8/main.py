#! /usr/bin/env python3

class Tree:
    def __init__(self, height):
        self.height = height
        self.visible = False

def scan(rows, cols):
    for y in range(cols):
        yield ((x, y) for x in range(rows))
        yield ((rows - (x + 1), y) for x in range(rows))
    for x in range(rows):
        yield ((x, y) for y in range(cols))
        yield ((x, cols - (y + 1)) for y in range(cols))

def part1(filename):
    forest = {}
    x = 0
    y = 0
    with open(filename, "r") as f:
        for line in f:
            line = line.strip()
            for x, c in enumerate(line):
                forest[(x, y)] = Tree(int(c))
            y += 1
    rows = x + 1
    cols = y
    for ray in scan(cols, rows):
        tall = -1
        for pos in ray:
            height = forest[pos].height
            if height > tall:
                forest[pos].visible = True
                tall = height
    return sum(1 for t in forest.values() if t.visible)


assert part1("test1") == 21

print(part1("input"))
