#! /usr/bin/env python3

def priority(c):
    n = 1 + ord(c.lower()) - ord('a')
    if c.isupper():
        n += 26
    return n

def lines(filename):
    with open(filename, "r") as f:
        for line in f:
            yield line.strip()

def packing(filename):
    total = 0
    for line in lines(filename):
        items = line.strip()
        assert len(items) % 2 == 0
        clen = len(items) // 2
        a = set(items[:clen])
        b = set(items[clen:])
        common = a & b
        assert len(common) == 1
        c = common.pop()
        total += priority(c)
    return total

def group(i, n):
    return zip(*[i]*n)

def badges(filename):
    total = 0
    for a, b, c in group(lines(filename), 3):
        common = set(a) & set(b) & set(c)
        assert len(common) == 1
        c = common.pop()
        total += priority(c)
    return total

assert packing("test1") == 157
assert badges("test2") == 18
assert badges("test3") == 52

print(packing("input"))
print(badges("input"))
