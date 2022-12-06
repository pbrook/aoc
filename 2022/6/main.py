#! /usr/bin/env python3

def sop(filename):
    with open(filename, "r") as f:
        buffer = f.read().strip()

    n = 4
    while True:
        s = set(buffer[n-4:n])
        if len(s) == 4:
            return n
        n += 1

assert sop("test1") == 7
assert sop("test2") == 5
assert sop("test3") == 6
assert sop("test4") == 10
assert sop("test5") == 11

print(sop("input"))
