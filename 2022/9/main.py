#! /usr/bin/env python3

def signval(x):
    if x == 0:
        return 0
    elif x > 0:
        return 1
    else:
        return -1

def rope_walk(filename):
    tail_x = 0
    tail_y = 0
    dx = 0
    dy = 0
    trail = set([(tail_x, tail_y)])
    with open(filename, "r") as f:
        for line in f:
            d, n = line.split()
            for _ in range(int(n)):
                if d == "R":
                    dx += 1
                elif d == "L":
                    dx -= 1
                elif d == "U":
                    dy += 1
                elif d == "D":
                    dy -= 1
                else:
                    assert false
                if abs(dx) > 1 or abs(dy) > 1:
                    s = signval(dx)
                    tail_x += s
                    dx -= s
                    s = signval(dy)
                    tail_y += s
                    dy -= s
                    trail.add((tail_x, tail_y))
    return len(trail)

assert rope_walk("test1") == 13

print(rope_walk("input"))
