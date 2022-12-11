#! /usr/bin/env python3

def parse_expr(s):
    if s == "* old":
        return lambda x: x*x

    op, val = s.split()
    val = int(val)
    if op == '*':
        return lambda x: x * val
    if op == '+':
        return lambda x: x + val
    assert False
    
class Monkey:
    def __init__(self, s, n):
        assert n == int(s[0].removeprefix("Monkey ").removesuffix(":"))
        itemstr = s[1].removeprefix("  Starting items:")
        self.n = n
        self.items = [int(x) for x in itemstr.split(',')]
        self.op = parse_expr(s[2].removeprefix("  Operation: new = old "))
        self.test = int(s[3].removeprefix("  Test: divisible by "))
        self.to_true = int(s[4].removeprefix("    If true: throw to monkey "))
        self.to_false = int(s[5].removeprefix("    If false: throw to monkey "))
        self.count = 0

    def __repr__(self):
        return f"Monkey{self.n}<{self.items}>"

def play(filename, rounds, worry_div):
    with open(filename, "r") as f:
        s = f.read().rstrip().split("\n")

    monkeys = []
    while s:
        monkeys.append(Monkey(s, len(monkeys)))
        s = s[7:]

    modulo = 1
    for m in monkeys:
        modulo *= m.test

    for _ in range(rounds):
        for m in monkeys:
            items = m.items
            m.count += len(items)
            m.items = []
            for item in items:
                item = (m.op(item) // worry_div) % modulo
                if item % m.test == 0:
                    dest = m.to_true
                else:
                    dest = m.to_false
                monkeys[dest].items.append(item)
    c = sorted((m.count for m in monkeys), reverse = True)
    return c[0] * c[1]

assert play("test1", 20, 3) == 10605
assert play("test1", 10000, 1) == 2713310158

print(play("input", 20, 3))
print(play("input", 10000, 1))
