#! /usr/bin/env python3

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

class Minion:
    def __init__(self, pos, wait):
        self.pos = pos
        self.wait = wait

    def clone(self):
        return Minion(self.pos, self.wait)

    def __repr__(self):
        return f"<{self.pos},{self.wait}>"

def walk2(state, visited, time, vol, stop):
    if len(visited) == 4:
        print(f"{time} {visited}")
    ns = tuple(s.clone() for s in state)
    while all(s.wait > 0 for s in ns):
        for s in ns:
            s.wait -= 1
        time += 1
    if time >= stop:
        return vol

    s = next(s for s in ns if s.wait == 0)
    v = rooms[s.pos]
    vol += v.rate * (stop - time)
    best = vol
    extra = 0
    #print(f"{time} {vol} {state} {visited}")
    for dest, dist in v.jumpmap.items():
        if dest in visited:
            continue
        extra += 1
        visited.append(dest)
        s.pos = dest
        s.wait = dist + 1
        newvol = walk2(ns, visited, time, vol, stop)
        if newvol > best:
            best = newvol;
        visited.pop()
    if extra == 0:
        s.wait = 99
        newvol = walk2(ns, visited, time, vol, stop)
        if newvol > best:
            best = newvol;
    return best


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

    part1 = walk2((Minion("AA", 0),), [], 0, 0, 30)
    part2 = walk2((Minion("AA", 0),Minion("AA", 0)), [], 0, 0, 26)
    #part2 = 1707
    return (part1, part2)
    #return walk("AA", [], -1, 0)

assert flow("test1") == (1651, 1707)

print(flow("input"))

