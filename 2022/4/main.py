#! /usr/bin/env python3

def getrange(s):
    x, y = s.split("-")
    return (int(x), int(y))

def contains(a, b):
    return a[0] <= b[0] and a[1]>= b[1]

def part1(filename):
    total = 0
    with open(filename, "r") as f:
        for line in f:
            astr, bstr = line.strip().split(",")
            a = getrange(astr)
            b = getrange(bstr)
            if contains(a, b) or contains(b, a):
                total += 1
    return total

assert part1("test1") == 2

print(part1("input"))
            

