#! /bin/bash

wd=/tmp/aoc16
rm -rf $wd
mkdir -p $wd

try_op() {
  if (( expect == $2 )) ; then
    ((count++))
  fi
}

run() {
  local ai=$a bi=$b
  local val
  local ar=${reg[$a]} br=${reg[$b]}
  local expect=${res[$c]}
  local count=0

  try_op addr $((ar+br))
  try_op addi $((ar+bi))
  try_op mulr $((ar*br))
  try_op muli $((ar*bi))
  try_op banr $((ar&br))
  try_op bani $((ar&bi))
  try_op borr $((ar|br))
  try_op bori $((ar|bi))
  try_op seti $((ar))
  try_op seti $((ai))
  try_op gtir $((ai > br))
  try_op gtri $((ar > bi))
  try_op gtrr $((ar > br))
  try_op eqir $((ai == br))
  try_op eqri $((ar == bi))
  try_op eqrr $((ar == br))

  if (( count  >= 3 )) ; then
    ((three++))
  fi
}

three=0
tr '[],:' '    ' < input.txt > $wd/inp
while true ; do
  read start reg[0] reg[1] reg[2] reg[3]
  if [ -z "$start" ] ; then
    break
  fi
  read op a b c
  read _dummy res[0] res[1] res[2] res[3]
  read _dummy
  run
done < $wd/inp
echo $three
