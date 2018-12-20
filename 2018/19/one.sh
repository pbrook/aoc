#! /bin/bash

f=input.txt
#f=small.txt

wd=/tmp/aoc19
rm -rf $wd
mkdir -p $wd

declare -a code reg=(0 0 0 0 0 0)

execute() {
  local op=$1 ai=$2 bi=$3 c=$4
  local val
  local ar=${reg[ai]} br=${reg[bi]}

  #printf "ip:%2d %-12s reg:%s\n" $ip "$*" "${reg[*]}"
  case "$op" in
    addr) val=$((ar+br)) ;;
    addi) val=$((ar+bi)) ;;
    mulr) val=$((ar*br)) ;;
    muli) val=$((ar*bi)) ;;
    banr) val=$((ar&br)) ;;
    bani) val=$((ar&bi)) ;;
    borr) val=$((ar|br)) ;;
    bori) val=$((ar|bi)) ;;
    setr) val=$((ar)) ;;
    seti) val=$((ai)) ;;
    gtir) val=$((ai > br)) ;;
    gtri) val=$((ar > bi)) ;;
    gtrr) val=$((ar > br)) ;;
    eqir) val=$((ai == br)) ;;
    eqri) val=$((ar == bi)) ;;
    eqrr) val=$((ar == br)) ;;
    *)
      echo "Bad op"
      ;;
  esac
  reg[c]=$val
}

parse() {
  read _dummy ipr
  code_len=0
  while read line ; do
    code[code_len]=$line
    ((code_len++))
  done
}

parse < $f
ip=0
while ((ip < code_len)) ; do
  reg[ipr]=$ip
  execute ${code[ip]}
  ((ip=reg[ipr]+1))
done
