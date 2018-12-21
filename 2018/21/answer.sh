#! /bin/bash

declare -A seen

r2=0
n=0

for ((;;)) ; do
  ((r4=r2|0x10000))
  ((r2=0x6682d5))
  for ((;;)) ; do
    ((r2 += (r4 & 0xff) ))
    ((r2 = (r2 * 0x1016b) & 0xffffff))
    if ((r4 < 0x100)) ; then
      if [ -z "$prev" ] ; then
        echo $r2
      fi
      if [ -n "${seen[$r2]}" ] ; then
        echo $prev
        exit 0
      fi
      seen[$r2]=y
      prev=$r2
      break
    fi
    ((r4 >>= 8))
  done
done
