#! /usr/bin/env python3

import PIL.Image

def parseNum(s):
    return int(s.strip("()"))

def parse(s):
    a = s.split()
    return (parseNum(a[2]), parseNum(a[3]), parseNum(a[4]))
def main():
    with open("map") as f:
        data = f.read()

    data = data[1:-2]
    da = data.split(',')
    points = list(parse(d) for d in da)
    x0 = x1 = 0
    y0 = y1 = 0
    for (x, y, _) in points:
        x0 = min(x0, x)
        x1 = max(x1, x)
        y0 = min(y0, y)
        y1 = max(y1, y)
    print("x:", x0, x1, "y:", y0, y1)

    im = PIL.Image.new("RGB", (x1 + 1 - x0, y1 + 1 - y0))
    for (x, y, t) in points:
        if t == 0:
            color = (0, 255, 0)
        elif t == 1:
            color = (255, 255, 255)
        else:
            color = (255, 0, 0)
        im.putpixel((x - x0, y - y0), color)
    im.save("test.png")

main()
