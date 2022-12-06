#! /usr/bin/env python3

def sop(filename, n):
    with open(filename, "r") as f:
        buffer = f.read().strip()

    pos = 0
    while True:
        s = set(buffer[pos:pos+n])
        if len(s) == n:
            return pos + n
        pos += 1

def part1(filename):
    return sop(filename, 4)

def part2(filename):
    return sop(filename, 14)

assert part1("test1") == 7
assert part1("test2") == 5
assert part1("test3") == 6
assert part1("test4") == 10
assert part1("test5") == 11

print(part1("input"))

assert part2("test1") == 19
assert part2("test2") == 23
assert part2("test3") == 23
assert part2("test4") == 29
assert part2("test5") == 26

print(part2("input"))
