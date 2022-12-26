#! /usr/bin/env python3

s2i = {
    '=': -2,
    '-': -1,
    '0': 0,
    '1': 1,
    '2': 2,
    }

i2s = dict((i, s) for s, i in s2i.items())

def snafu(filename):
    total = []
    with open(filename, "r") as f:
        for line in f:
            line = line.strip()
            while len(total) < len(line):
                total.append(0)
            for i, c in enumerate(reversed(line)):
                total[i] += s2i[c]
    i = 0
    while i+1 < len(total) or total[i] != 0:
        if i+1 == len(total):
            total.append(0)
        while total[i] < -2:
            total[i+1] -= 1
            total[i] += 5
        while total[i] > 2:
            total[i+1] += 1
            total[i] -= 5
        i += 1
    return "".join(i2s[n] for n in reversed(total)).lstrip('0')

assert snafu("test1") == '2=-1=0'

print(snafu("input"))
