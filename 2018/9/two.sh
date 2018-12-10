#! /bin/bash

wd=/tmp/aoc9
rm -rf $wd
mkdir -p $wd

f=input.txt
#f=medium.txt
#f=small.txt

looper() {
  limit=$(($limit - 22))
  player=0
  current=1
  for ((n=0 ; n < 6; n++)) ; do
    read val
    echo $val
    echo $current
    current=$(($current + 1))
  done >> $wd/loop
  while [ $current -le $limit ]; do
    for ((n=6; n < 18; n++)) ; do
      read val
      echo $val
      echo $current
      current=$(($current + 1))
    done >> $wd/loop
    read addend
    echo $current >> $wd/loop
    current=$(($current + 1))
    next=$(($current+4))
    for ((n=19; n < 22; n++)) ; do
      read val
      echo $val
      echo $next
      next=$(($next+1))
      echo $current
      current=$(($current + 1))
      echo $next
      next=$(($next+1))
    done >> $wd/loop
    player=$((($player + 23) % $players))
    score[$player]=$((${score[$player]} + $addend + $current))
    [ $(($current % 2300)) = 0 ] && echo "#$current"
    current=$next
  done

  for ((player=0; player < $players; player++)) ; do
    echo ${score[$player]} $(($player + 1))
  done | sort -nr | head -n1
}

read players _2 _3 _4 _5 _6 limit _8 < $f

limit=$(($limit * 100))
echo 0 > $wd/loop
tail -n +1 -F $wd/loop | looper
rm -rf $wd
