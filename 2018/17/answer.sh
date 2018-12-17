#! /bin/bash

f=input.txt
#f=small.txt

wd=/tmp/aoc17
rm -rf $wd
mkdir -p $wd


declare -A grid

hline() {
  for ((x=v1; x <= v2 ; x++)) ; do
    grid[$x:$u]='#'
  done
}

vline() {
  if (( ymax < v2 )) ; then
    ymax=$v2
  fi
  if (( ymin > v1 )) ; then
    ymin=$v1
  fi
  for ((y=v1; y <= v2 ; y++)) ; do
    grid[$u:$y]='#'
  done
}

scan() {
  ((wet++))
#echo scan $x:$y $wet
  c="${grid[$x:$y1]}"
  if [ -z "$c" ] ; then
    drop $x $y1
    c="${grid[$x:$y1]}"
  fi
  if [ "$c" = "|" ] ; then
    fill='|'
    limit=$x
    return
  fi
  c="${grid[$x1:$y]}"
  if [ -n "$c" ] ; then
    if [ "$c" != '#' ] ; then
      fill='|'
    fi
    limit=$x
    return
  fi
}

drop() {
  local x=$1 y=$2
  local top=$y
  local x1 y1 middle limit left
  local again fill
  for ((;;)); do
    if ((y > ymax)) ; then
      return
    fi
    if ((y >= ymin)) ; then
      ((wet++))
    fi
#echo "drop $x:$y ${grid[$x:$y]} $wet"
    grid[$x:$y]='|'
    y1=$((y+1))
    c=${grid[$x:$y1]}
    if ! [ -z "$c" ] ; then
      break
    fi
    y=$y1
  done
  if [ "$c" = '|' ] ; then
    return
  fi
  middle=$x
  for ((;y>=top;)) ; do
    fill='~'
    limit=
    x=$middle
    ((wet--))
    while [ -z "$limit" ] ; do
      ((x1=x-1))
      scan
      ((x--))
    done
    left=$limit
    limit=
    x=$middle
    ((wet--))
    while [ -z "$limit" ] ; do
      ((x1=x+1))
      scan
      ((x++))
    done
#echo "fill $fill $y $left-$limit"
    if [ "$fill" = '~' ] ; then
      ((full+=limit + 1 - left))
    fi
    for ((x=left;x<=limit;x++)) ; do
      grid[$x:$y]=$fill
    done
    if [ "$fill" = '|' ] ; then
      break;
    fi
    ((y--,y1--))
  done
}

wet=0
full=0
ymin=9999
ymax=0
tr '=,.' '   ' < $f > $wd/inp
while read c1 u c2 v1 v2 ; do
  if [ $c1 = x ] ; then
    vline
  else
    hline
  fi
done < $wd/inp

#echo $ymin $ymax
drop 500 0
echo $wet
echo $full
