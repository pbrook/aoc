#! /usr/bin/env python3

import ocr

class Cpu:
    def __init__(self, filename):
        self.signal = 0
        self.cycle = 1
        self.x = 1
        self.screen = []
        self.scanline = ""
        with open(filename, "r") as f:
            for line in f:
                self.step()
                insn = line.split()
                if insn[0] == "addx":
                    self.step()
                    self.x += int(insn[1])

    def step(self):
        if self.cycle in [20, 60, 100, 140, 180, 220]:
            self.signal += self.cycle * self.x
        n = len(self.scanline)
        if abs(n - self.x) <= 1:
            self.scanline += '#'
        else:
            self.scanline += '.'
        if n == 39:
            self.screen.append(self.scanline)
            self.scanline = ""
        self.cycle += 1

    def part1(self):
        return self.signal

    def part2(self):
        return self.screen

s = [
    "##..##..##..##..##..##..##..##..##..##..",
    "###...###...###...###...###...###...###.",
    "####....####....####....####....####....",
    "#####.....#####.....#####.....#####.....",
    "######......######......######......####",
    "#######.......#######.......#######.....",
    ]

c = Cpu("test1")
assert c.part1() == 13140
assert c.part2() == s

c = Cpu("input")
print("\n".join(c.part2()))
print(c.part1())
print(ocr.ocr(c.part2()))
