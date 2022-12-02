#! /usr/bin/env python3

def snacksum(filename):
    total = 0
    with open(filename) as f:
        for l in f:
            s = l.strip()
            if s == '':
                yield total
                total = 0
            else:
                total += int(s)
    yield total

def snacks(filename):
    snacks = sorted(snacksum(filename), reverse=True)
    return snacks[0], sum(snacks[0:3])

assert snacks("test1") == (24000, 45000)

print(snacks("input"))
