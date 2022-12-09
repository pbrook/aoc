#! /usr/bin/env python3

def signval(x):
    if x == 0:
        return 0
    elif x > 0:
        return 1
    else:
        return -1

class Knot():
    def __init__(self, other):
        self.x = 0
        self.y = 0
        self.other = other

    def move(self, d):
        if d == "R":
            self.x += 1
        elif d == "L":
            self.x -= 1
        elif d == "U":
            self.y += 1
        elif d == "D":
            self.y -= 1
        else:
            assert False
        self.pull()

    def pull(self):
        other = self.other
        if other is None:
            return
        dx = self.x - other.x
        dy = self.y - other.y
        if abs(dx) > 1 or abs(dy) > 1:
            other.x += signval(dx)
            other.y += signval(dy)
            other.pull()
    def __repr__(self):
        return f"<{self.x},{self.y}>"

def rope_walk(filename, rope_len):
    tail = Knot(None)
    head = tail
    for _ in range(rope_len):
        head = Knot(head)
    trail = set([(0,0)])
    with open(filename, "r") as f:
        for line in f:
            d, n = line.split()
            for _ in range(int(n)):
                head.move(d)
                trail.add((tail.x, tail.y))
    return len(trail)

assert rope_walk("test1", 1) == 13
assert rope_walk("test2", 9) == 36

print(rope_walk("input", 1))
print(rope_walk("input", 9))
