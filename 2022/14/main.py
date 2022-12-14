#! /usr/bin/env python3

def signum(x):
    if x > 0:
        return 1
    if x < 0:
        return -1
    return 0

class Cave:
    def __init__(self, filename):
        self.grid = {}
        with open(filename, "r") as f:
            for line in f:
                p = ((int(x), int(y)) for x,y in (s.split(',') for s in line.split('->')))
                start = next(p)
                for pos in p:
                    self.rock(start, pos)
                    start = pos

    def rock(self, start, end):
        x0, y0 = start
        x1, y1 = end
        dx = signum(x1 - x0)
        dy = signum(y1 - y0)
        x = x0
        y = y0
        while x != x1 or y != y1:
            self.grid[(x, y)] = '#'
            x += dx
            y += dy
        self.grid[(x, y)] = '#'

    def dump(self):
        x0 = min(x for x, _ in self.grid)
        x1 = max(x for x, _ in self.grid)
        y0 = min(y for _, y in self.grid)
        y1 = max(y for _, y in self.grid)
        for y in range(y0, y1+1):
            line = ""
            for x in range(x0, x1+1):
                line += self.grid.get((x, y), '.')
            print(line)

    def pour(self):
        count = 0
        part1 = 0
        maxy = max(y for _, y in self.grid)
        start = (500, 0)
        y = 0
        while start not in self.grid:
            x, y = start
            while y <= maxy:
                moved = False
                for dx, dy in [(0, 1), (-1, 1), (1, 1)]:
                    if (x + dx, y + dy) not in self.grid:
                        x += dx
                        y += dy
                        moved = True
                        break
                if not moved:
                    break
            if moved and part1 == 0:
                part1 = count
            self.grid[(x, y)] = 'o'
            count += 1
        return (part1, count)

def regolith(filename):
    c = Cave(filename)
    n = c.pour()
    #c.dump()
    return n

#print(regolith("test1"))
assert regolith("test1") == (24, 93)

print(regolith("input"))
