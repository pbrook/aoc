#! /usr/bin/env python3

ortho = [(1, 0), (-1, 0), (0, 1), (0, -1), (0, 0)]
face = {
    '>': (1, 0),
    '<': (-1, 0),
    'v': (0, 1),
    '^': (0, -1),
}

def dist(x, y, target):
    return abs(x-target[0]) + abs(y-target[1])

class Storm():
    def __init__(self, filename):
        ns = []
        self.sdir = []
        with open(filename, "r") as f:
            s = f.read().rstrip().split('\n')
            assert s[0][:3] == '#.#' and all(c == '#' for c in s[0][3:])
            assert s[-1][-3:] == '#.#' and all(c == '#' for c in s[-1][:-3])
            for y, line in enumerate(s[1:-1]):
                line = line.strip()
                assert line[0] == '#' and line[-1] == '#'
                for x, c in enumerate(line[1:-1]):
                    if c == '.':
                        continue
                    self.sdir.append(face[c])
                    ns.append((x, y))
        self.ar = [ns]
        self.width = len(s[0]) - 2
        self.height = len(s) - 2

    def get(self, n):
        while n >= len(self.ar):
            ns = []
            for (sx, sy), (dx, dy) in zip(self.ar[-1], self.sdir):
                pos = ((sx + dx) % self.width, (sy + dy) % self.height)
                ns.append(pos)
            self.ar.append(ns)
        return self.ar[n]

    def walk(self, start, target, steps):
        while True:
            start_step = steps + 1
            while start in self.get(start_step):
                start_step += 1
            done = set()
            x, y = start
            stack = [(dist(x, y, target) + start_step, x, y, start_step)]
            while stack:
                stack.sort(reverse=True)
                _, x, y, steps = stack.pop()
                #print(x, y, steps, _, len(stack))
                steps += 1

                newpos = set()
                for dx, dy in ortho:
                    if (x + dx) in range(self.width) and (y+dy) in range(self.height):
                        newpos.add((x+dx, y+dy))
                if len(newpos) == 0:
                    continue
                newpos.difference_update(self.get(steps))
                for x, y in newpos:
                    limit = dist(x, y, target)
                    #print(f" {x} {y} {limit}")
                    if limit == 0:
                        return steps + 1
                    else:
                        if (x, y, steps) not in done:
                            done.add((x, y, steps))
                            stack.append((limit+steps, x, y, steps))
            #print(f"fail {start_step}")

def run(filename):
    s = Storm(filename)
    goal = (s.width - 1), (s.height - 1)
    part1 = s.walk((0,0), goal, 0)
    mid = s.walk(goal, (0,0), part1)
    part2 = s.walk((0,0), goal, mid)
    #print(part1, mid, part2)
    return part1, part2

assert run("test1") == (10, 30)
assert run("test2") == (18, 54)

print(run("input"))
