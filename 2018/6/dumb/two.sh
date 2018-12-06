#! /bin/bash

# Takes a couple of minutes to run

wd=/tmp/aoc6
rm -rf $wd
mkdir -p $wd

while read x y ; do
  x=${x%,}
  echo $x $y
done < input.txt > $wd/inp
target=10000

left=$(cut -d' ' -f 1 $wd/inp | sort -n | head -n1)
right=$(cut -d' ' -f 1 $wd/inp | sort -n | tail -n1)
top=$(cut -d' ' -f 2 $wd/inp | sort -n | head -n1)
bot=$(cut -d' ' -f 2 $wd/inp | sort -n | tail -n1)

all_md() {
  local x0=$1 y0=$2
  local x y dx dy md
  local tot=0
  while read x y ; do
    dy=$(($y - $y0))
    dx=$(($x0 - $x))
    md=$(( ( ($dx >= 0) ? $dx : -$dx) + ( ($dy >= 0) ? $dy : -$dy) ))
    echo +$md
  done < $wd/inp
}

safe=0
# Assume safe region does not include all locations
for x in $(seq $left $right) ; do
  for y in $(seq $top $bot) ; do
    td=$(($(all_md $x $y)))
    if [ $td -lt $target ] ; then
      safe=$(($safe + 1))
    fi
  done
done
echo $safe

rm -rf $wd
