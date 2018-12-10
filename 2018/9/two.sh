#! /bin/bash

wd=/tmp/aoc9
rm -rf $wd
mkdir -p $wd

f=input.txt
#f=medium.txt
#f=small.txt

getn() {
  current=${m_next[$current]}
}

getp() {
  current=${m_prev[$current]}
}

setn() {
  m_next[$1]=$2
}

setp() {
  m_prev[$1]=$2
}

insert() {
  local orig=$current
  getn
  setn $orig $lowest
  setp $current $lowest
  setn $lowest $current
  setp $lowest $orig
  current=$lowest
}

backtrack() {
  local remove prev
  getp
  getp
  getp
  getp
  getp
  getp
  local next=$current
  getp
  local remove=$current
  getp
  #echo add $player $lowest $remove
  score[$player]=$((${score[$player]} + $remove + $lowest))
  setp $next $current
  setn $current $next
  current=$next
}

pregame() {
  setn 0 0
  setp 0 0
  player=0
  lowest=1
  current=0
  while [ $lowest -le 46 ] ; do
    if [ $(($lowest % 23)) = 0 ] ; then
      #echo "#$lowest"
      backtrack
    else
      getn 
      insert
    fi
    lowest=$(($lowest + 1))
    player=$((($player + 1) % $players))
  done
}


read players _2 _3 _4 _5 _6 limit _8 < input.txt

declare -A m_prev m_next m_count score
pregame
first=$current
while true ; do
  getn
  echo $current
  [ $current = $first ] && break
done > $wd/loop

looper() {
  declare -a input
  for ((n=0 ; n < 6; n++)) ; do
    read val
    input[$n]=$val
  done

  limit=$(($limit - 22))
  player=$(($player-1))
  current=$lowest
  while [ $current -le $limit ]; do
    for ((n=6; n < 22 ; n++)) ; do
      read val
      input[$n]=$val
    done
    for ((n=0; n < 18; n++)) ; do
      echo ${input[$n]}
      echo $current
      current=$(($current + 1))
    done >> $wd/loop
    addend=${input[18]}
    echo $current >> $wd/loop
    current=$(($current + 1))
    out=0
    for ((n=19; n < 22; n++)) ; do
      input[$out]=${input[$n]}
      out=$((out+1))

      input[$out]=$current
      out=$((out+1))
      current=$(($current + 1))
    done >> $wd/loop
    player=$((($player + 23) % $players))
    score[$player]=$((${score[$player]} + $addend + $current))
    [ $(($current % 2300)) = 0 ] && echo "#$current"
    current=$(($current + 1))
  done

  for ((player=0; player < $players; player++)) ; do
    echo ${score[$player]} $(($player + 1))
  done | sort -nr | head -n1
}

limit=$(($limit * 100))
tail -n +1 -F $wd/loop | looper
rm -rf $wd
