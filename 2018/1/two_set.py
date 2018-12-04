#! /usr/bin/env python3

with open("input.txt") as f:
    data = list(int(x) for x in f.readlines())

def gen_freq():
    freq = 0
    while True:
        for delta in data:
            yield freq
            freq += delta

def scan():
    seen = set()
    for freq in gen_freq():
        if freq in seen:
            return freq;
        seen.add(freq)

print(scan())
