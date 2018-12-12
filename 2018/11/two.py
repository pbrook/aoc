#! /usr/bin/env python3

serial=9005
#serial=18

max=0

def calc_power(x, y):
    rack = x + 10
    p1 = (rack * y + serial) * rack
    return ((p1 % 1000) // 100) - 5

power = [[calc_power(x + 1, y + 1) for y in range(300)] for x in range(300)]
total = [([0] * 300) for x in range(300)]

maxval = 0

def loop(size):
  global maxval, best
  limit = 301 - size
  for x in range(limit):
    sum=0
    nx = x + size - 1
    ny = 0
    while ny < size - 1:
        sum += power[nx][ny]
        ny += 1
    for y in range(limit):
      total[x][y] += sum
      sum += power[nx][ny] - power [nx][y]
      ny += 1

  for y in range(limit):
    sum=0
    ny = y + size - 1
    nx = 0
    while nx < size:
        sum += power[nx][ny]
        nx += 1
    for x in range(limit):
      val = total[x][y] + sum
      total[x][y] = val
      if val > maxval:
        maxval = val
        best=(x + 1, y + 1, size)
        print(best, maxval)
      if nx < 300:
        sum += power[nx][ny]
      sum -= power[x][ny]
      nx += 1

for size in range(1, 301):
  loop(size)

print(best, maxval)
