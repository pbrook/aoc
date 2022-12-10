#! /usr/bin/env python3

def signal(cycle, x):
    if cycle not in [20, 60, 100, 140, 180, 220]:
        return 0
    return cycle * x

def cpu(filename):
    total = 0
    cycle = 1
    x = 1
    with open(filename, "r") as f:
        for line in f:
            total += signal(cycle, x)
            insn = line.split()
            if insn[0] == "addx":
                cycle += 1
                total += signal(cycle, x)
                x += int(insn[1])
            cycle += 1
    total += signal(cycle, x)
    return total

assert cpu("test1") == 13140

print(cpu("input"))
