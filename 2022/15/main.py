#! /usr/bin/env python3

def atoi(s):
    i = 0
    while s[i] == '-' or s[i].isdigit():
        i += 1
    return int(s[:i])

class Sensor:
    def __init__(self, line):
        ar = line.split('=')
        self.x = atoi(ar[1])
        self.y = atoi(ar[2])
        self.bx = atoi(ar[3])
        self.by = atoi(ar[4])

    def interval(self, y0):
        dist = abs(self.x - self.bx) + abs(self.y - self.by)
        dy = abs(self.y - y0)
        dist -= dy
        if dist < 0:
            return None
        return (self.x - dist, self.x + dist)

def part1(filename, y0):
    with open(filename, "r") as f:
        sens = [Sensor(line) for line in f]

    intervals = [iv for iv in (s.interval(y0) for s in sens) if iv is not None]
    intervals.sort(key=lambda x: x[0])
    count = 0
    x = intervals[0][0]
    for start, end in intervals:
        if start > x:
            x = start
        if end >= x:
            count += end + 1 - x
            x = end + 1
    bx = set(s.bx for s in sens if s.by == y0)
    return count - len(bx)



assert part1("test1", 10) == 26

print(part1("input", 2000000))
