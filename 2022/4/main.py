#! /usr/bin/env python3

def getrange(s):
    x, y = s.split("-")
    return (int(x), int(y))

def contains(a, b):
    return a[0] <= b[0] and a[1]>= b[1]


def part1(a, b):
    return contains(a, b) or contains(b, a)

def part2(a, b):
    return not (a[0] > b[1] or a[1] < b[0])

def scan(filename, part):
    total = 0
    with open(filename, "r") as f:
        for line in f:
            astr, bstr = line.strip().split(",")
            a = getrange(astr)
            b = getrange(bstr)
            if part(a, b):
                total += 1
    return total

assert scan("test1", part1) == 2
assert scan("test1", part2) == 4

print(scan("input", part1))
print(scan("input", part2))
            

