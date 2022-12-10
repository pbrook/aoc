#! /usr/bin/env python3

import sys

# topleft, topright, botleft, botright, holes
# 0=empty 1 = vert, 2=horiz, 4=any
template = [
        ("A", "0011"),
        ("E", "3232"),
        ("F", "3210"),
        ("H", "1111"), # KX
        ("I", "2222"),
        ("L", "1032"),
        ("T", "2200"), # JY
        ("J", "0400"),
        ("J", "2400"), # Y
        ("P", "3010"),
        ("R", "3014"),
        ("K", "1414"), # X
        ("M", "4411"),
        ("N", "4114"),
        ("W", "1144"),
        ("Z", "2442"),
        ("X", "4444"),
        ("U", "1100"), # VY
        ("V", "1100"), # Y
        ("Y", "4400"),

        ("B", "3030"), # D
        ("D", "3030"),

        ("S", "0000"), # OC
        ("C", "0000"), # O
        ("O", "0000"),
        ("G", "0004"), # Q
        ("Q", "0004"),
]

def special(letter, c):
    if c == 'H':
        return ' ' not in letter[-1]
    if c == 'T':
        return ' ' not in (s[0] for s in letter)
    if c == 'J':
        if letter[0][0] == ' ':
            return True
        return ' ' not in (s[0] for s in letter)
    if c == 'K':
        return ' ' not in letter[0]
    if c == 'U':
        n = sum(1 for s in letter if s[-1] == '#')
        return n >= len(letter) // 2
    if c == 'V':
        n = sum(1 for s in letter if s[-2] == '#')
        return n > 1
    if c == 'B':
        return '#' in letter[1][1:-1]
    if c == 'S':
        return '#' in letter[len(letter)//2][1:-1]
    return True

def corner(letter, x, y):
    if letter[x][y] == ' ':
        return 0
    n = 0
    if x == 0:
        dx = 1
    else:
        dx = -2
    if y == 0:
        dy = 1
    else:
        dy = -2
    if letter[x][dy] == '#':
        n |= 1
    if letter[dx][y] == '#':
        n |= 2
    if n == 0:
        n = 4
    return n

def match_corner(seen, expect):
    if expect == '4':
        return seen != 0
    return seen == int(expect)

def process(letter):
    tl = corner(letter, 0, 0)
    tr = corner(letter, -1, 0)
    bl = corner(letter, 0, -1)
    br = corner(letter, -1, -1)

    for c, pattern in template:
        match = (
            match_corner(tl, pattern[0]) and
            match_corner(tr, pattern[1]) and
            match_corner(bl, pattern[2]) and
            match_corner(br, pattern[3]))
        if match:
            match = special(letter, c)
        if match:
            return c
    return '?'

def ocr(data):
    result = ""
    data = list(data)
    line_len = max(len(line.rstrip()) for line in data) 
    col = [""] * line_len
    for line in data:
        for i, c in enumerate(line[:line_len]):
            if c in '#X':
                c = '#'
            else:
                c = ' '
            col[i] += c
    letter = []
    for s in col:
        if '#' in s:
            letter.append(s)
        else:
            if letter != []:
                result += process(letter)
            letter = []
    if letter != []:
        result += process(letter)
    return result

def main():
    data = sys.stdin.readlines()
    print(ocr(data))

if __name__ == "__main__":
    main()
