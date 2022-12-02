#! /usr/bin/env python3

def part1(them, me):
    return me

def part2(them, me):
    return (them + me + 2) % 3

def play(line, part):
    them_c, me_c = line.split()
    them = ord(them_c) - ord('A')
    me = ord(me_c) - ord('X')

    me = part(them, me)

    if them == me: # Draw
        points = 3
    elif ((them + 1) % 3) == me: # Win
        points = 6
    else: # Loose
        points = 0
    return points + me + 1

def rps(filename, part):
    score = 0
    with open(filename, "r") as f:
        for line in f:
            score += play(line, part)
    return score

assert rps("test1", part1) == 15
assert rps("test1", part2) == 12

print(rps("input", part1))
print(rps("input", part2))

