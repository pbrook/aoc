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

def part1(filename):
    total = 0
    with open(filename, "r") as f:
        s = f.read().rstrip()
    for n, p in enumerate(s.split("\n\n")):
        astr, bstr = p.split("\n")
        a, _ = parse_list(astr)
        b, _ = parse_list(bstr)
        rc = compare(a, b)
        assert rc is not None
        if rc:
            total += n + 1
    return total

assert part1("test1") == 13

print(part1("input"))
