#! /usr/bin/env python3

class Tree:
    def __init__(self, height):
        self.height = height
        self.visible = False

class Forest:
    def __init__(self, filename):
        self.grid = {}
        x = 0
        y = 0
        with open(filename, "r") as f:
            for line in f:
                line = line.strip()
                for x, c in enumerate(line):
                    self.grid[(x, y)] = Tree(int(c))
                y += 1
        self.cols = x + 1
        self.rows = y

    def height(self, pos):
        return self.grid[pos].height

    def scan(self):
        for y in range(self.rows):
            yield ((x, y) for x in range(self.cols))
            yield ((self.cols - (x + 1), y) for x in range(self.cols))
        for x in range(self.cols):
            yield ((x, y) for y in range(self.rows))
            yield ((x, self.rows - (y + 1)) for y in range(self.rows))

    def treeline(self, ray):
        pos = next(ray)
        tall = self.height(pos)
        n = 1
        for pos in ray:
            if self.height(pos) >= tall:
                break
            n += 1
        return n

    def look_from(self, x0, y0):
        total = 1
        total *= self.treeline((x, y0) for x in range(x0, self.cols - 1))
        total *= self.treeline((x, y0) for x in range(x0, 0, -1))
        total *= self.treeline((x0, y) for y in range(y0, self.rows - 1))
        total *= self.treeline((x0, y) for y in range(y0, 0, -1))
        return total

    def part1(self):
        for ray in self.scan():
            tall = -1
            for pos in ray:
                height = self.height(pos)
                if height > tall:
                    self.grid[pos].visible = True
                    tall = height
        return sum(1 for t in self.grid.values() if t.visible)

    def part2(self):
        best = 0
        for x in range(1, self.cols - 1):
            for y in range(1, self.rows - 1):
                n = self.look_from(x, y)
                if n > best:
                    best = n
        return best

def treehouse(filename):
    wood = Forest(filename)
    return (wood.part1(), wood.part2())

assert treehouse("test1") == (21, 8)

print(treehouse("input"))
