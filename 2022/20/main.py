#! /usr/bin/env python3

class Num:
    def __init__(self, i, line):
        self.pos = i
        self.n = int(line)
    def __repr__(self):
        return  f"<{self.pos}:{self.n}>"

def parse(filename):
    with open(filename, "r") as f:
        return [Num(i, line) for i, line in enumerate(f)]

def signum(x):
    if x > 0:
        return 1
    elif x < 0:
        return -1
    return 0

def mix(inp, rounds):
    ll = len(inp)
    for _ in range(rounds):
        for v1 in inp:
            out = dict((v.pos, v) for v in inp)
            #print([(out[pos].n) for pos in range(ll)])
            a = v1.pos
            b = (a + v1.n) % (ll - 1)
            v1.pos = b
            dn = signum(a - b)
            #print(a, b, dn)
            if dn == 0:
                continue
            for pos in range(b, a, dn):
                out[pos].pos += dn
            pos = b

    out = dict((v.pos, v) for v in inp)
    for v in inp:
        if v.n == 0:
            break
    #print(v.n, v.pos)
    tot = sum(out[(v.pos+pos) % ll].n for pos in [1000, 2000, 3000])
    return tot

def part1(filename):
    inp = parse(filename)
    return mix(inp, 1)

def part2(filename):
    inp = parse(filename)
    for v in inp:
        v.n *= 811589153
    return mix(inp, 10)

assert part1("test1") == 3
assert part2("test1") == 1623178306

print(part1("input"))
print(part2("input"))
