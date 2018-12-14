#! /usr/bin/env python3

import sys

class Node():
    def __init__(self, val):
        self.val=val
        self.next=None

def insert(val):
    global last,xsum,count
    n=Node(val)
    n.next=last.next
    last.next=n
    last=n
    xsum=(xsum % 100000)*10 + val
    count += 1
    #print(xsum)
    if xsum == 607331:
        print(count-6)
        sys.exit(0)

def main():
    global last,xsum,count
    a=Node(3)
    b=Node(7)
    a.next=b
    b.next=a
    last=b
    xsum=37
    count=2
    while True:
        newval=a.val+b.val
        if newval >= 10:
            insert(1)
            newval -= 10
        insert(newval)
        for _ in range(a.val + 1):
            a=a.next
        for _ in range(b.val + 1):
            b=b.next
main()
