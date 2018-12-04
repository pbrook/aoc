#! /usr/bin/env python3

with open("input.txt") as f:
    data = list(x.strip() for x in f.readlines())

def find_runs():
    double = 0
    triple = 0
    for box in data:
        s = sorted(box)
        run = 1;
        seen_2 = False
        seen_3 = False
        prev = ''
        for c in s:
            if c == prev:
                run += 1
            else:
                if run == 2:
                    seen_2 = True
                elif run == 3:
                    seen_3 = True
                run = 1
            prev = c
        if run == 2:
            seen_2 = True
        elif run == 3:
            seen_3 = True
        if seen_2:
            double += 1
        if seen_3:
            triple += 1
        print(box, double, triple)
    return double, triple


c2, c3 = find_runs()
print(c2*c3)
