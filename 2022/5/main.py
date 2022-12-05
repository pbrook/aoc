#! /usr/bin/env python3

def dump(stacks):
    depth = max([len(s) for s in stacks])
    while depth > 0:
        depth -= 1
        line = ''
        for s in stacks:
            if depth >= len(s):
                line += "    "
            else:
                line += f"[{s[depth]}] "
        print(line)
    print("".join(f" {n+1}  " for n in range(len(stacks))))

def part1(stacks, n, ifrom, ito):
    for _ in range(n):
        stacks[ito].append(stacks[ifrom].pop())

def part2(stacks, n, ifrom, ito):
    stacks[ito] += stacks[ifrom][-n:]
    del stacks[ifrom][-n:]

def top(stacks):
    #dump(stacks)
    return "".join(s[-1] for s in stacks)

def crates(filename, part):
    stacks = [[]]
    with open(filename, "r") as f:
        for line in f:
            line = line.rstrip()
            if line == '':
                for s in stacks:
                    s.reverse()
                #dump(stacks)
                setup = False
                break
            if line[1] == '1':
                continue
            n = 0
            x = 0
            while x < len(line):
                if line[x] == '[':
                    while n >= len(stacks):
                        stacks.append([])
                    stacks[n].append(line[x+1])
                n += 1
                x += 4
        # Move crates
        for line in f:
            insn = line.split()
            assert len(insn) == 6 and insn[0] == "move"
            n = int(insn[1])
            ifrom = int(insn[3]) - 1
            ito = int(insn[5]) - 1

            part(stacks, n, ifrom, ito)

            #dump(stacks)
    return top(stacks)

assert crates("test1", part1) == "CMZ"
assert crates("test1", part2) == "MCD"

print(crates("input", part1))
print(crates("input", part2))
