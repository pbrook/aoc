#! /usr/bin/env python3

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

def move(pos, face):
    x, y = pos
    if face == 0:
        return (x+1, y)
    elif face == 1:
        return (x, y+1)
    elif face == 2:
        return (x-1, y)
    elif face == 3:
        return (x, y-1)
    else:
        assert False

def part1(filename):
    m = {}
    with open(filename, "r") as f:
        s = f.read().rstrip()
    s, path = s.split('\n\n')
    for y, line in enumerate(s.split('\n')):
        for x,c in enumerate(line):
            if c == ' ':
                continue
            m[x, y] = Tile(c)
    top = min(x for x,y in m.keys() if y == 0)
    pos = (top, 0)
    face = 0
    for op in parsepath(path):
        #print(pos, face)
        if isinstance(op, int):
            for _ in range(op):
                pos1 = move(pos, face)
                #print(f" {pos1}")
                if pos1 not in m:
                    #print(" reverse")
                    while True:
                        pos2 = move(pos1, (face + 2) % 4)
                        if pos2 not in m:
                            #print(" wrap")
                            break
                        pos1 = pos2
                if m[pos1].c == '#':
                    #print(" stall")
                    break;
                #print(f" step {pos1}")
                pos = pos1
        elif op == 'L':
            #print(" L")
            face = (face + 3) % 4
        elif op == 'R':
            #print(" R")
            face = (face + 1) % 4
        else:
            assert False
    return 1000 * (pos[1]+1) + 4*(pos[0]+1) + face

assert part1("test1") == 6032

print(part1("input"))
