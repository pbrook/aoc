#! /usr/bin/env python3

N = 24

class Blueprint:
    def __init__(self, line):
        self.cost = {}
        _, s = line.split(':')
        self.max = {"geode":9999}
        for r in s.split('.'):
            ar = r.split()
            if not ar:
                continue
            assert (ar[0], ar[2], ar[3]) == ('Each', 'robot', 'costs')
            cost = [(ar[i+1], int(ar[i])) for i in range(4, len(ar), 3)]
            self.cost[ar[1]] = cost
            for rr, n in cost:
                self.max[rr] = max(n, self.max.get(rr, 0))
    def __repr__(self):
        return f"<{self.cost}>"
    def tick(self, bots, res, time, ignore=None):
        if ignore is None:
            ignore = set()
        best = 0
        nr = dict((r, n + res[r]) for r, n in bots.items())
        time -= 1
        if time == 0:
            return nr["geode"]
        for name, cost in self.cost.items():
            if name in ignore:
                continue
            if bots[name] >= self.max[name]:
                continue
            if any(res[r] < n for r, n in cost):
                continue
            ignore.add(name)
            nrb = nr.copy()
            for r, n in cost:
                nrb[r] -= n
            nb = bots.copy()
            nb[name] += 1
            if time >= N:
                print(f"  {time} {name}")
            count = self.tick(nb, nrb, time)
            if count > best:
                best = count
        if "geode" not in ignore:
            if time >= N:
                print(f"  {time} -")
            count = self.tick(bots, nr, time, ignore)
            if count > best:
                best = count
        return best

def parse(filename):
    with open(filename, "r") as f:
        return [Blueprint(line) for line in f]


def part1(filename):
    blueprints = parse(filename)
    nothing = dict((x, 0) for x in blueprints[0].cost.keys())
    bots = nothing.copy()
    bots["ore"] = 1
    quality = 0
    for i, bp in enumerate(blueprints):
        count = bp.tick(bots=bots, res=nothing, time=24)
        print(i+1, count)
        quality += (i+1) * count
    return quality

def part2(filename):
    blueprints = parse(filename)
    nothing = dict((x, 0) for x in blueprints[0].cost.keys())
    bots = nothing.copy()
    bots["ore"] = 1
    prod = 1
    for i, bp in enumerate(blueprints[1:3]):
        count = bp.tick(bots=bots, res=nothing, time=32)
        print(i+1, count)
        prod *= count
    return prod

print(part1("test1"))
print(part1("input"))
