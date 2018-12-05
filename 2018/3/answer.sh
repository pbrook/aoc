#! /bin/bash

cat input.txt | sed -e 's/[^0-9]/ /g' | ( \
zero=$(printf "%01000d" 0)
declare -a cloth
for y in $(seq 0 999) ; do
  cloth[$y]=$zero
done
while read id x0 y0 w h ; do
  x1=$(($x0 + $w))
  for y in $(seq $y0 $(($y0 + $h - 1))) ; do
    row=${cloth[$y]}
    left=${row::$x0}
    mid=${row:$x0:$w}
    right=${row:$x1}
    mid=${mid//1/2}
    mid=${mid//0/1}
    cloth[$y]=$left$mid$right
  done
done

echo -n ${cloth[@]} | sed -e 's/[^2]//g' | wc -c

cat input.txt | sed -e 's/[^0-9]/ /g' | ( \
while read id x0 y0 w h ; do
  if ! for y in $(seq $y0 $(($y0 + $h - 1))) ; do
    row=${cloth[$y]}
    echo ${row:$x0:$w}
  done | grep -q 2 ; then
    echo $id
    break
  fi
done
))
