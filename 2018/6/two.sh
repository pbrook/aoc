#! /bin/bash

wd=/tmp/aoc6
rm -rf $wd
mkdir -p $wd

tot=0
np=0
while read x y ; do
  x=${x%,}
  echo $x >> $wd/x_raw
  echo $y >> $wd/y_raw
  tot=$(($tot + $x + $y))
  np=$(($np + 1))
done < input.txt
#target=32
target=10000

sort -n $wd/x_raw > $wd/x
sort -n $wd/y_raw > $wd/y

left=$(head -n1 $wd/x)
right=$(tail -n1 $wd/x)
top=$(head -n1 $wd/y)
bot=$(tail -n1 $wd/y)
tot=$(($tot - ($np * ($left + $top)) ))

build_deltas() {
  delta=-$np
  read next_x
  for x in $(seq $1 $2); do
    while [ $x = "$next_x" ] ; do
      read next_x
      delta=$((delta + 2))
    done
    echo $delta
  done
}

build_deltas $left $right < $wd/x > $wd/dx

build_deltas $top $bot < $wd/y > $wd/dy

nr=0
while read delta ; do
  dist[$nr]=$tot
  tot=$(($tot + $delta))
  nr=$(($nr + 1))
done < $wd/dy

safe=0
while read delta ; do
  for n in ${!dist[@]} ; do
    d=${dist[$n]}
    if [ $d -lt $target ] ; then
      safe=$(($safe + 1))
    fi
    dist[$n]=$(($d + $delta))
  done
done < $wd/dx
echo $safe

rm -rf $wd
