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

def walk(name, visited, time, vol):
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

    return walk("AA", [], -1, 0)

#assert flow("test1") == 1651

print(flow("input"))
