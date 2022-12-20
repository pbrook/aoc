#! /usr/bin/env python3

N = 32

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
        for name in ["geode", "obsidian", "clay", "ore"]:
            if name in ignore:
                continue
            if bots[name] >= self.max[name]:
                continue
            cost = self.cost[name]
            if any(res[r] < n for r, n in cost):
                continue
            nrb = nr.copy()
            for r, n in cost:
                nrb[r] -= n
            nb = bots.copy()
            ignore.add(name)
            nb[name] += 1
            if time >= N:
                print(f"  {time} {name}")
            count = self.tick(nb, nrb, time)
            if count > best:
                best = count
            if name == "geode":
                break
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


def mine(filename):
    blueprints = parse(filename)
    nothing = dict((x, 0) for x in blueprints[0].cost.keys())
    bots = nothing.copy()
    bots["ore"] = 1
    part1 = 0
    for i, bp in enumerate(blueprints):
        count = bp.tick(bots=bots, res=nothing, time=24)
        #print(i+1, count)
        part1 += (i+1) * count
    part2 = 1
    for i, bp in enumerate(blueprints[:3]):
        count = bp.tick(bots=bots, res=nothing, time=32)
        #print(i+1, count)
        part2 *= count
    print(part1, part2)
    return part1, part2

# This takes several times longer to run than my input (90s v.s. 16s)!
assert mine("test1") == (33, 56 * 62)

print(mine("input"))
