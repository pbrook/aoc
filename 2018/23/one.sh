#! /bin/bash

f=input.txt
#f=small.txt

wd=/tmp/aoc23
mkdir -p $wd

tr 'pos=<>,r' '        ' < $f | sort -rk 4 > $wd/inp

read x0 y0 z0 r0 < $wd/inp

count=0
while read x y z r ; do
  ((dx=(x>x0)?x-x0:x0-x))
  ((dy=(y>y0)?y-y0:y0-y))
  ((dz=(z>z0)?z-z0:z0-z))
  if ((dx+dy+dz <= r0)) ; then
    ((count++))
  fi
done < $wd/inp
echo $count
