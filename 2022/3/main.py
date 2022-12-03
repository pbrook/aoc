#! /usr/bin/env python3

def priority(c):
    n = 1 + ord(c.lower()) - ord('a')
    if c.isupper():
        n += 26
    return n

def packing(filename):
    total = 0
    with open(filename, "r") as f:
        for line in f:
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


assert packing("test1") == 157

print(packing("input"))
