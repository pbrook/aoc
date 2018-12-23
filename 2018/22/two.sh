#! /bin/bash

f=input.txt
#f=small.txt

wd=/tmp/aoc22
mkdir -p $wd

declare -a row
declare -A grid best

# This is a hack.  Pick a value big enough to dind any indirect routes
# Worst case could be a detour 3* the direct distance
border=50

load() {
  read _dummy depth
  read _dummy pos
  width=${pos%,*}
  height=${pos#*,}
}

load < $f

# A worst case bound could be calculated before the first probe reaches the
# target, reducing the need for the border hack
result=99999

erode() {
  ((el=(gi+depth) % 20183))
  ((row[$x]=el))
  ((type=el%3))
  # 1 torch
  # 2 climbing
  # 4 neither
  case $type in
    0) tool=3 ;;
    1) tool=6 ;;
    2) tool=5 ;;
  esac
  grid[$x:$y]=$tool
}

if ((width > height)); then
  limit=$width
else
  limit=$height
fi
((limit += border))
y=0
for ((x=0;x<=width+border;x++)) ; do
  ((gi=x * 16807))
  erode
done

for ((y=1;y<=limit;y++)) ; do
  ((gi=48271*y))
  x=0
  erode
  for ((x=1;x<=width+border;x++)) ; do
    if ((x == width && y == height)) ; then
      gi=0
    else
      ((gi=el*row[$x]))
    fi
    erode
  done
done

try() {
  local loc=$1:$2:$3 tick=$4
  local prev=${best[$loc]}
#echo "try $loc $tick/$prev"
  if [ -z "$prev" ] || [ $prev -gt $tick ] ; then
    #echo "best: $loc $tick"
    best[$loc]=$tick
    echo $2 $1 $3 $tick >> $wd/worklist
    if [ $loc = $target ] && [ $tick -lt $result ] ; then
      result=$tick
    fi
  fi
}

walk() {
  local x=$1 y=$2
  local dest=${grid[$x:$y]}
#echo walk $x $y $tool $dest
  if (( (tool & dest) == 0)) ; then
    return
  fi
  try $x $y $tool $tick
}

switch_tool() {
  local src=${grid[$x0:$y0]}
  local newtool=$((src ^ tool))
  try $x0 $y0 $newtool $((tick+7))
}

echo 0 0 1 0 > $wd/worklist
target=$width:$height:1
best[0:0:1]=0

while [ -f $wd/worklist ] ; do
  sort -n < $wd/worklist > $wd/pending
  rm $wd/worklist
  head -n1 $wd/pending
#  cat $wd/pending
#  read
  nearest=9999
  while read y0 x0 tool tick; do
    if ((tick > result)) ; then
      continue
    fi
    prev=${best[$x0:$y0:$tick]}
    if [ -n "$prev" ] && [ $prev -lt $tick ] ; then
      continue
    fi
    if ((x0 + y0 <= nearest)) ; then
      ((nearest=x0+y0))
      switch_tool
      ((tick++))
      if ((x0 > 0)) ; then
        walk $((x0-1)) $y0
      fi
      if ((y0 > 0)) ; then
        walk $x0 $((y0-1))
      fi
      walk $((x0+1)) $y0
      walk $x0 $((y0+1))
    else
      echo $y0 $x0 $tool $tick >> $wd/worklist
    fi
  done < $wd/pending
done

echo $result

rm -rf $wd
