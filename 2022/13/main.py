#! /usr/bin/env python3

import itertools

def parse_list(s):
    result = []
    assert s[0] == '['
    s = s[1:]
    while s[0] != ']':
        if s[0] == '[':
            val, s = parse_list(s)
        else:
            sval = ""
            while s[0].isdigit():
                sval += s[0]
                s = s[1:]
            val = int(sval)
        result.append(val)
        if s[0] == ',':
            s = s[1:]
    return (result, s[1:])

def compare(a, b):
    if not isinstance(a, list):
        a = [a]
    if not isinstance(b, list):
        b = [b]

    for left, right in itertools.zip_longest(a, b):
        if left is None:
            return True
        if right is None:
            return False
        if isinstance(left, list) or isinstance(right, list):
            rc = compare(left, right)
            if rc is not None:
                return rc
        else:
            if left < right:
                return True
            elif left > right:
                return False
    return None

class Packet:
    def __init__(self, s):
        self.val, s = parse_list(s)
        assert s == ""

    def __lt__(self, other):
        rc = compare(self.val, other.val)
        assert rc is not None
        return rc

def part1(filename):
    total = 0
    with open(filename, "r") as f:
        s = f.read().rstrip()
    d1 = Packet("[[2]]")
    d2 = Packet("[[6]]")
    msg = [d1, d2]
    for n, p in enumerate(s.split("\n\n")):
        a, b = (Packet(x) for x in p.split("\n"))
        if a < b:
            total += n + 1
        msg.extend([a, b])
    msg.sort()
    p1 = msg.index(d1) + 1
    p2 = msg.index(d2) + 1
    return total, p1 * p2

assert part1("test1") == (13, 140)

print(part1("input"))
