#! /usr/bin/env python3

class Side:
    def __init__(self):
        self.adj = [None]*4
    def __repr__(self):
        return f"<{self.adj}>"

ortho = [(1, 0), (0, 1), (-1, 0), (0, -1)]
def rotl(i):
    return (i-1)%4
def rotr(i):
    return (i+1)%4
def rotb(i):
    return (i+2)%4

def part2(net):
    for (x, y), f in net.items():
        for i, (dx, dy) in enumerate(ortho):
            other = (x+dx, y+dy)
            if other in net:
                f.adj[i] = (other, i)
    while any(any(a is None for a in f.adj) for f in net.values()):
        for pos, f in net.items():
            for i in range(4):
                l = f.adj[rotl(i)]
                r = f.adj[i]
                if l is not None and r is not None:
                    lf, li = net[l[0]], l[1]
                    rf, ri = net[r[0]], r[1]
                    li = rotr(li)
                    ri = rotl(ri)
                    if lf.adj[li] is None:
                        lf.adj[li] = (r[0], rotb(ri))
                    if rf.adj[ri] is None:
                        rf.adj[ri] = (l[0], rotb(li))


class Tile:
    def __init__(self, c):
        self.c = c
        self.adj = [None]*4

def parsepath(path):
    n = 0
    for c in path:
        if c in 'LR':
            if n != 0:
                yield n
                n = 0
            yield c
        else:
            n = (n * 10) + int(c)
    if n != 0:
        yield n

def move(pos, face):
    x, y = pos
    dx, dy = ortho[face]
    return (x+dx, y+dy)

def part1(net):
    for (x, y), f in net.items():
        for i, (dx, dy) in enumerate(ortho):
            other = (x+dx, y+dy)
            if other not in net:
                other = (x, y)
                while True:
                    newpos = (other[0]-dx, other[1]-dy)
                    if newpos not in net:
                        break
                    other = newpos
            f.adj[i] = (other, i)


def tile_side(pos, i, sz):
    x0, y0 = pos
    x0 *= sz
    y0 *= sz
    if i == 0:
        for y in range(y0, y0+sz):
            yield (x0 + sz-1, y)
    elif i == 1:
        for x in range(x0+sz, x0, -1):
            yield (x-1 , y0 + sz-1)
    elif i == 2:
        for y in range(y0+sz, y0, -1):
            yield (x0, y-1)
    elif i == 3:
        for x in range(x0, x0+sz):
            yield (x, y0)
    else:
        assert False

def link(m, net, sz):
    for pos, f in net.items():
        for i in range(4):
            op, oi = f.adj[i]
            ts0 = tile_side(pos, i, sz)
            ts1 = reversed(list(tile_side(op, rotb(oi), sz)))
            for p0, p1 in zip(ts0, ts1):
                m[p0].adj[i] = (p1, oi)

def run(filename):
    m = {}
    with open(filename, "r") as f:
        s = f.read().rstrip()
    s, path = s.split('\n\n')
    for y, line in enumerate(s.split('\n')):
        for x,c in enumerate(line):
            if c == ' ':
                continue
            m[x, y] = Tile(c)

    maxx = max(x for x,_ in m.keys())
    maxy = max(y for _,y in m.keys())

    if maxx > 50:
        sz = 50
    else:
        sz = 4

    result = []
    for part in [part1, part2]:
        net = {}
        for x in range(maxx // sz + 1):
            for y in range(maxy // sz + 1):
                if (x*sz, y*sz) in m:
                    net[x,y] = Side()

        part(net)

        for (x,y), t in m.items():
            for i, (dx, dy) in enumerate(ortho):
                other = (x+dx, y+dy)
                if other in m:
                    t.adj[i] = (other, i)

        link(m, net, sz)

        top = min(x for x,y in m.keys() if y == 0)
        pos = (top, 0)
        face = 0
        for op in parsepath(path):
            #print(pos, face)
            if isinstance(op, int):
                for _ in range(op):
                    np, nf = m[pos].adj[face]
                    if m[np].c == '#':
                        break
                    pos = np
                    face = nf
            elif op == 'L':
                #print(" L")
                face = rotl(face)
            elif op == 'R':
                #print(" R")
                face = rotr(face)
            else:
                assert False
        result.append(1000 * (pos[1]+1) + 4*(pos[0]+1) + face)
    return result

assert run("test1") == [6032, 5031]

print(run("input"))
