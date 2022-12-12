#! /usr/bin/env python3

BIG = 99999999

class Point:
    def __init__(self, c):
        self.height = ord(c) - ord('a')
        self.distance = BIG

class Hill:
    def __init__(self, filename):
        self.grid = {}
        with open(filename, "r") as f:
            for y, line in enumerate(f):
                line = line.strip()
                for x, c in enumerate(line):
                    if c == 'S':
                        self.start = (x, y)
                        c = 'a'
                    elif c == 'E':
                        self.end = (x, y)
                        c = 'z'
                    self.grid[(x, y)] = Point(c)
    def ortho(self, pos):
        x, y = pos
        for newpos in [(x, y+1), (x, y-1), (x+1,y), (x-1, y)]:
            if newpos in self.grid:
                yield newpos

    def walk(self):
        pending = [self.end]
        p = self.grid[self.end]
        p.distance = 0

        while pending:
            pos = pending.pop()
            p = self.grid[pos]
            step = p.distance + 1
            x, y = pos
            for newpos in [(x, y+1), (x, y-1), (x+1, y), (x-1, y)]:
                np = self.grid.get(newpos)
                if np is None:
                    continue
                if np.height < p.height - 1:
                    continue
                if step >= np.distance:
                    continue
                np.distance = step
                pending.append(newpos)
        return self.grid[self.start].distance

    def part2(self):
        return min(p.distance for p in self.grid.values() if p.height == 0)

def climb(filename):
    h = Hill(filename)
    part1 = h.walk()
    return (part1, h.part2())

assert climb("test1") == (31, 29)

print(climb("input"))
