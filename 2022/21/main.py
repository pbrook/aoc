#! /usr/bin/env python3

class Monkey:
    def __init__(self, line):
        self.name, expr = line.split(':')
        expr = expr.split()
        if len(expr) == 1:
            expr = int(expr[0])
        else:
            assert len(expr) == 3
            assert expr[1] in '+-/*'
        self.expr = expr

    def eval(self):
        if isinstance(self.expr, int):
            return self.expr
        a = mm[self.expr[0]].eval()
        b = mm[self.expr[2]].eval()
        op = self.expr[1]
        if op == '+':
            res = a + b
        elif op == '-':
            res = a - b
        elif op == '*':
            res = a * b
        elif op == '/':
            if a % b == 0:
                res = a // b
            else:
                res = a / b
        return res

    def __repr__(self):
        return  f"<{self.expr}>"

def parse(filename):
    global mm
    mm = {}
    with open(filename, "r") as f:
        for line in f:
            m = Monkey(line)
            mm[m.name] = m

def shout(filename):
    parse(filename)
    root = mm["root"]
    part1 = root.eval()
    root.expr[1] = '-'
    humn = mm["humn"]
    humn.expr = -1
    g0 = -1
    g1 = 1
    while True:
        humn.expr = g0
        r0 = root.eval()
        humn.expr = g1
        r1 = root.eval()
        p = r0 * r1
        if p <= 0:
            break
        g0 *= 2
        g1 *= 2
    while True:
        g = (g0 + g1) // 2
        humn.expr = g
        r = root.eval()
        if r == 0:
            break
        if (r * r0) > 0:
            g0 = g
        else:
            g1 = g
    return part1, g

def part2(filename):
    parse(filename)

assert shout("test1") == (152, 301)

print(shout("input"))
