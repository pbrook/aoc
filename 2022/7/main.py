#! /usr/bin/env python3

class File:
    def __init__(self, parent, size):
        self.parent = parent
        self._size = size
    def size(self):
        return self._size
    def dirsize(self):
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
    def dirsize(self):
        yield self.size()
        for c in self.contents.values():
            yield from c.dirsize()

def walk(filename):
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
    sz = list(root.dirsize())
    p1 = sum(s for s in sz if s < 100000)
    target = (root.size() + 30000000) - 70000000
    p2 = min(s for s in sz if s >= target)
    return (p1, p2)

assert walk("test1") == (95437, 24933642)

print(walk("input"))
