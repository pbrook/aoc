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
        pending = [(self.end, 25, 0)]
        while pending:
            pos, height, step = pending.pop()
            p = self.grid[pos]
            if p.height < height - 1:
                continue
            if p.distance <= step:
                continue
            #print(f"{pos} {height} {step}")
            p.distance = step
            step += 1
            x, y = pos
            for newpos in [(x, y+1), (x, y-1), (x+1, y), (x-1, y)]:
                if newpos in self.grid:
                    pending.append((newpos, p.height, step))
        return self.grid[self.start].distance

    def part2(self):
        return min(p.distance for p in self.grid.values() if p.height == 0)

def climb(filename):
    h = Hill(filename)
    part1 = h.walk()
    return (part1, h.part2())

assert climb("test1") == (31, 29)

print(climb("input"))
