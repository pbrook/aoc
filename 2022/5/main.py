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

def part1(filename):
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
            for _ in range(n):
                stacks[ito].append(stacks[ifrom].pop())
            #dump(stacks)
    #dump(stacks)
    return "".join(s[-1] for s in stacks)

assert part1("test1") == "CMZ"

print(part1("input"))
