#! /usr/bin/env python3

def play(line):
    them_c, me_c = line.split()
    them = ord(them_c) + 1 - ord('A')
    me = ord(me_c) + 1 - ord('X')

    if them == me: # Draw
        points = 3
    elif ((them + 1) % 3) == (me % 3): # Win
        points = 6
    else: # Loose
        points = 0
    return points + me

def rps(filename):
    score = 0
    with open(filename, "r") as f:
        for line in f:
            score += play(line)
    return score

assert rps("test1") == 15

print(rps("input"))
