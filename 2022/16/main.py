#! /usr/bin/env python3

import collections

class Valve:
    def __init__(self, line):
        ar = line.split()
        self.name = ar[1]
        self.rate = int(ar[4].split('=')[1].strip(';'))
        self.tunnel = [s.strip(',') for s in ar[9:]]
        self.jumpmap = {}

    def traverse(self):
        jm = self.jumpmap
        jm[self.name] = 0
        work = [self.name]
        while len(work) > 0:
            vn = work.pop()
            steps = jm[vn] + 1
            for dest in rooms[vn].tunnel:
                if dest not in jm or jm[dest] > steps:
                    jm[dest] = steps
                    work.append(dest)
        del jm[self.name]

n = 0

def walk(name, visited, time, vol):
    global n
    n += 1
    global rooms
    if time >= 30:
        return vol
    visited.append(name)
    v = rooms[name]
    time += 1
    vol += v.rate * (30 - time)
    best = vol
    for dest, dist in v.jumpmap.items():
        if dest in visited:
            continue
        newvol = walk(dest, visited, time + dist, vol)
        if newvol > best:
            best = newvol;
    visited.pop()
    return best

class Minion_:
    def __init__(self, pos, wait):
        self.pos = pos
        self.wait = wait

    def clone(self):
        return Minion(self.pos, self.wait)

    def __repr__(self):
        return f"<{self.pos},{self.wait}>"

Minion = collections.namedtuple("Minion", ["pos", "wait"])

def walk2(state, visited, time):
    if len(visited) == 3:
        print(f"{time} {visited}")

    delay = min(s.wait for s in state)
    if delay >= time:
        return 0

    state = tuple(Minion(m.pos, m.wait - delay) for m in state)
    time -= delay

    who, where = next((i, m.pos) for i, m in enumerate(state) if m.wait == 0)

    v = rooms[where]
    vol = v.rate * time
    best = 0
    extra = 0

    for dest, dist in v.jumpmap.items():
        if dest in visited:
            continue
        extra += 1
        nv = visited | frozenset((dest,))
        ns = state[:who] + (Minion(dest, dist+1),) + state[who+1:]
        newvol = walk2(ns, nv, time)
        if newvol > best:
            best = newvol;
    if extra == 0:
        ns = state[:who] + (Minion(dest, 99),) + state[who+1:]
        newvol = walk2(ns, visited, time)
        if newvol > best:
            best = newvol;
    return best + vol


def flow(filename):
    global rooms
    rooms = {}
    with open(filename, "r") as f:
        for line in f:
            v = Valve(line)
            rooms[v.name] = v

    for v in rooms.values():
        v.traverse()
        for dest in list(v.jumpmap.keys()):
            if rooms[dest].rate == 0:
                del v.jumpmap[dest]

    part1 = walk2((Minion("AA", 0),), frozenset(), 30)
    part2 = 1707
    part2 = walk2((Minion("AA", 0),Minion("AA", 0)), frozenset(), 26)
    #print(part1, part2)
    return (part1, part2)
    #return walk("AA", [], -1, 0)

#assert flow("test1") == (1651, 1707)

print(flow("input"))

