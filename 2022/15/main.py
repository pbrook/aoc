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

def scan(filename, y0):
    with open(filename, "r") as f:
        sens = [Sensor(line) for line in f]

    part2 = -1
    part1 = -1
    for y in range(y0 * 2 + 1):
        intervals = [iv for iv in (s.interval(y) for s in sens) if iv is not None]
        intervals.sort(key=lambda x: x[0])
        count = 0
        x = intervals[0][0] - 1
        for start, end in intervals:
            skip = start - (x + 1)
            if skip > 0:
                if start > 0 and x < y0 * 2:
                    assert skip == 1
                    assert part2 == -1
                    part2 = (x + 1) * 4000000 + y
                    if part1 >= 0:
                        break
                x = start
            if end >= x:
                count += end - x
                x = end
        if y == y0:
            bx = set(s.bx for s in sens if s.by == y0)
            part1 = count - len(bx)
            if part2 >= 0:
                break

    return (part1, part2)



assert scan("test1", 10) == (26, 56000011)

print(scan("input", 2000000))
