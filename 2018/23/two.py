#! /usr/bin/env python3


class NanoBot():
    def __init__(self, x, y, z, r):
        self.x = x
        self.y = y
        self.z = z
        self.r = r

def distance(x, xmin, xmax):
    if x < xmin:
        return xmin - x
    if x > xmax:
        return x - xmax
    return 0

def bisect(x0, y0, z0, x1, y1, z1):
    global best
    nb = 0
    if x1 < x0 or y1 < y0 or z1 < z0:
        return
    for b in bots:
        d = distance(b.x, x0, x1)
        d += distance(b.y, y0, y1)
        d += distance(b.z, z0, z1)
        if d <= b.r:
            nb += 1
    if nb < target:
        return
    if x0 == x1 and y0 == y1 and z0 == z1:
        d = abs(x0) + abs(y0) + abs(z0)
        if d < best:
            best=d
        return

    xm = (x0 + x1) // 2
    ym = (y0 + y1) // 2
    zm = (z0 + z1) // 2
    bisect(x0, y0, z0, xm, ym, zm)
    bisect(xm+1, y0, z0, x1, ym, zm)
    bisect(x0, ym+1, z0, xm, y1, zm)
    bisect(xm+1, ym+1, z0, x1, y1, zm)
    bisect(x0, y0, zm+1, xm, ym, z1)
    bisect(xm+1, y0, zm+1, x1, ym, z1)
    bisect(x0, ym+1, zm+1, xm, y1, z1)
    bisect(xm+1, ym+1, zm+1, x1, y1, z1)


def main():
    global bots
    global target, best

    filename="input.txt"
    bots = []
    x0 = 999999999; y0 = 999999999; z0 = 999999999
    x1 = -999999999; y1 = -999999999; z1 = -999999999
    with open(filename) as f:
        for line in f:
            pstr, rstr = line.split()
            x, y, z = (int(n) for n in pstr[5:-2].split(','))
            bots.append(NanoBot(x, y, z, int(rstr[2:])))
            if x < x0:
                x0 = x
            if y < y0:
                y0 = y
            if z < z0:
                z0 = z
            if x > x1:
                x1 = x
            if y > y1:
                y1 = y
            if z > z1:
                z1 = z

    target = len(bots)
    best=999999999
    while best==999999999:
        bisect(x0, y0, z0, x1, y1, z1)
        target -= 1
    print(best)

main()
