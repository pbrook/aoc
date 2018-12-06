#! /bin/bash

# Takes a couple of minutes to run

wd=/tmp/aoc6
rm -rf $wd
mkdir -p $wd

n=0
while read x y ; do
  x=${x%,}
  echo $n $x $y
  n=$(($n + 1))
done < input.txt > $wd/inp

left=$(cut -d' ' -f 2 $wd/inp | sort -n | head -n1)
right=$(cut -d' ' -f 2 $wd/inp | sort -n | tail -n1)
top=$(cut -d' ' -f 3 $wd/inp | sort -n | head -n1)
bot=$(cut -d' ' -f 3 $wd/inp | sort -n | tail -n1)

nearest() {
  local x0=$1 y0=$2
  local n x y dx dy md
  local best=9999
  while read n x y ; do
    dy=$(($y - $y0))
    dx=$(($x0 - $x))
    md=$(( ( ($dx >= 0) ? $dx : -$dx) + ( ($dy >= 0) ? $dy : -$dy) ))
    if [ $md -lt $best ] ; then
      best=$md
      bestn=$n
    elif [ $md = $best ] ; then
      bestn=
    fi
  done < $wd/inp
  echo $bestn
}

inf() {
  local n=$(nearest $1 $2)
  if [ -n "$n" ] ; then
    count[$n]=0
  fi
}

declare -a count
for x in $(seq $left $right) ; do
  echo $x
  for y in $(seq $top $bot) ; do
    n=$(nearest $x $y)
    if [ -n "$n" ] ; then
      count[$n]=$((${count[$n]} + 1))
    fi
  done
done
for x in $(seq $left $right) ; do
  inf $x $top
  inf $x $bot
done
for y in $(seq $top $bot) ; do
  inf $left $y
  inf $right $y
done

while read n x y ; do
  echo ${count[$n]} $n
done < $wd/inp | sort -nr | head -n1

rm -rf $wd
