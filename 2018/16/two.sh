#! /bin/bash

wd=/tmp/aoc16
rm -rf $wd
mkdir -p $wd

declare -a opmask
declare -a opmap

declare -a reg

for ((n=0;n<16;n++)) ; do
  opmask[n]=$((0xffff))
done

try_op() {
  if (( expect == $2 )) ; then
    ((count++))
  else
    ((opmask[$1] &= ~(1<<op) ))
  fi
}

execute() {
  local ai=$a bi=$b
  local val
  local ar=${reg[$a]} br=${reg[$b]}

  case "${opmap[$op]}" in
    0) val=$((ar+br)) ;;
    1) val=$((ar+bi)) ;;
    2) val=$((ar*br)) ;;
    3) val=$((ar*bi)) ;;
    4) val=$((ar&br)) ;;
    5) val=$((ar&bi)) ;;
    6) val=$((ar|br)) ;;
    7) val=$((ar|bi)) ;;
    8) val=$((ar)) ;;
    9) val=$((ai)) ;;
    10) val=$((ai > br)) ;;
    11) val=$((ar > bi)) ;;
    12) val=$((ar > br)) ;;
    13) val=$((ai == br)) ;;
    14) val=$((ar == bi)) ;;
    15) val=$((ar == br)) ;;
    *) echo "Bad opcode ($op=${opmap[$op]})" ;;
  esac
  reg[$c]=$val
}

simulate() {
  local ai=$a bi=$b
  local val
  local ar=${reg[$a]} br=${reg[$b]}
  local expect=${res[$c]}
  local count=0

  try_op 0 $((ar+br))
  try_op 1 $((ar+bi))
  try_op 2 $((ar*br))
  try_op 3 $((ar*bi))
  try_op 4 $((ar&br))
  try_op 5 $((ar&bi))
  try_op 6 $((ar|br))
  try_op 7 $((ar|bi))
  try_op 8 $((ar))
  try_op 9 $((ai))
  try_op 10 $((ai > br))
  try_op 11 $((ar > bi))
  try_op 12 $((ar > br))
  try_op 13 $((ai == br))
  try_op 14 $((ar == bi))
  try_op 15 $((ar == br))

  if (( count  >= 3 )) ; then
    ((three++))
  fi
}

main() {
  while true ; do
    read start reg[0] reg[1] reg[2] reg[3]
    if [ -z "$start" ] ; then
      break
    fi
    read op a b c
    read _dummy res[0] res[1] res[2] res[3]
    read _dummy
    simulate
  done
  echo $three
  declare -a opmap
  for ((num_ops=16;num_ops>0;)) ; do
    for ((op=0;op<16; op++)); do
      mask=${opmask[op]}
      if (( mask == 0 || (mask & (mask-1) ) != 0)) ; then
        continue
      fi
      ((num_ops--))
      for ((n=0;n<16;n++)) ; do
        ((opmask[n]&=~mask))
      done
      for ((n=0; mask != 1;n++,mask>>=1)); do
        :
      done
      #echo "op $n = $op"
      opmap[n]=$op
    done
  done
  read
  reg=(0 0 0 0)
  while read op a b c ; do
    execute
  done
  echo ${reg[0]}
}
three=0
tr '[],:' '    ' < input.txt > $wd/inp
main < $wd/inp
rm -rf $wd
