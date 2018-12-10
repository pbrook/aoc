#! /bin/bash


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
  score[$player]=$((${score[$player]} + $remove + $lowest))
  setp $next $current
  setn $current $next
  current=$next
}

game() {
  # For any significant array length, associatinve array are much faster then numeric ones!
  local -A m_prev m_next m_count score
  local player n
  setn 0 0
  setp 0 0
  player=0
  lowest=1
  current=0
  while [ $lowest -le $limit ] ; do
    if [ $(($lowest % 23)) = 0 ] ; then
      #echo "#$lowest"
      backtrack
    else
      getn 
      insert
    fi
    #echo $lowest $player ${score[$player]}
    lowest=$(($lowest + 1))
    player=$((($player + 1) % $players))
  done
  for ((player=0; player < $players; player++)) ; do
    echo ${score[$player]} $(($player + 1))
  done | sort -nr | head -n1
}


cut -d' ' -f 1,7 < $f | while read players limit ; do
limit=$(($limit))
    game
done
