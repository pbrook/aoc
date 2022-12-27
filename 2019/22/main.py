#! /usr/bin/env python3
def parse1(filename):
    """
    deal n = (-(n+1)) mod ps
    cut n = (n - x)  mod ps
    increment n = (n*x) mod ps
    """
    with open(filename, "r") as f:
        for line in f:
            ar = line.strip().split()
            if ar[0] == "cut":
                yield (1, -int(ar[1]))
            else:
                assert ar[0] == "deal"
                if ar[2] == "new":
                    yield (-1, -1)
                else:
                    assert ar[2] == "increment"
                    yield (int(ar[3]), 0)

# given y = ax % n
# find b such that x = by % n
# set y=1; solve ax % n = 1
def inverse(a, n):
    if a == 1:
        return 1
    #print(a, n)
    # smallest x that gives ax > n
    x = n // a + 1
    while True:
        r = (a * x) % n
        #print(f" {x} {r}")
        if r == 1:
            return x
        #print(f"  +{(n-r)//a + 1}")
        x += (n - r) // a + 1

def part1(filename, decksize, n):
    for a, b in parse1(filename):
        n = (n * a + b) % decksize
    return n

# x1 = (a.x + b) % N
# x2 = (c.x1 + d) % N
#    = (c(a.x+b) + d) %N
#    = (c.a.x + c.b+d) % N
def combine(a, b, c, d, N):
    return (c * a) % N, (c * b + d) % N

def part2(filename, N, x, count=1):
    insn = list(parse1(filename))
    a0 = 1
    b0 = 0
    for a, b in reversed(insn):
        if a == -1:
            assert b == -1
        elif a > 1:
            assert b == 0
            a = inverse(a, N)
        else:
            b = -b
        a0, b0 = combine(a0, b0, a, b, N)
    a = 1
    b = 0
    mask = 1
    while mask <= count:
        if (mask & count) != 0:
            a, b = combine(a, b, a0, b0, N)
        a0, b0 = combine(a0, b0, a0, b0, N)
        mask <<= 1
    x = (a * x + b) % N
    return x

def test(filename, expected):
    actual = "".join(str(part2(filename, 10, i)) for i in range(10))

p1 = part1("input", 10007, 2019)
print(p1)

test("test0", "0369258147")
test("test1", "3074185296")
test("test2", "6307418529")
test("test3", "9258147036")

assert part2("input", 10007, p1) == 2019
print(part2("input", 119315717514047, 2020, 101741582076661))
