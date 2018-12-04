#! /usr/bin/env python3
# Takes a few minutes to run, but is very memory efficient :-)

with open("input.txt") as f:
    data = list(int(x) for x in f.readlines())

def gen_freq(count = -1):
    """Yields the first COUNT frequencies (-1 == infinite)"""
    freq = 0
    while True:
        for delta in data:
            if count == 0:
                return
            freq += delta
            yield freq
            count -= 1

def scan():
    for n, freq in enumerate(gen_freq()):
        # Brute force scan to see if we have seen this before
        for prev in gen_freq(n):
            if freq == prev:
                return freq

print(scan())
