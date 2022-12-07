#! /usr/bin/env python3

class File:
    def __init__(self, parent, size):
        self.parent = parent
        self._size = size
    def size(self):
        return self._size
    def part1(self):
       yield from ()

class Dir:
    def __init__(self, parent):
        self.parent = parent
        self.contents = {}
    def size(self):
        size = 0
        for c in self.contents.values():
            size += c.size()
        return size
    def part1(self):
        size = self.size()
        if size <= 100000:
            yield size
        for c in self.contents.values():
            yield from c.part1()

def part1(filename):
    root = Dir(None)
    with open(filename, "r") as f:
        for line in f:
            line = line.strip()
            if line.startswith("$ cd"):
                dirname = line[5:]
                if dirname == '/':
                    curdir = root
                elif dirname == '..':
                    curdir = curdir.parent
                else:
                    curdir = curdir.contents[dirname]
            elif line.startswith("$ ls"):
                pass
            elif line.startswith("dir "):
                _, name = line.split()
                curdir.contents[name] = Dir(curdir)
            else:
                size, name = line.split()
                curdir.contents[name] = File(curdir, int(size))
    return sum(root.part1())

assert part1("test1") == 95437

print(part1("input"))
