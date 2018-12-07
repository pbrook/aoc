#! /bin/bash

wd=/tmp/aoc7
rm -rf $wd
mkdir -p $wd

if true ; then
  f=input.txt
  num_workers=5
  startup=60
else
  startup=0
  f=small.txt
  num_workers=2
fi

declare -A deps
cut -d' ' -f2,8 < $f > $wd/inp

while read dep part ; do
  deps[$part]="${deps[$part]}$dep"
  # make sure all parts are in the array
  deps[$dep]="${deps[$dep]}"
done < $wd/inp

for part in ${!deps[@]} ; do
  echo "$part:${deps[$part]}"
done | sort > $wd/todo

declare -a work
declare -a timer

part_time() {
  local part=$1
  n=$(expr index ABCDEFGHIJKLMNOPQRSTUVWXYZ $part)
  echo $(($startup + $n + $tick))
}

newtick=0
while [ $newtick != 9999 ] ; do
  tick=$newtick
#echo "Tick $tick ${work[@]}"
#cat $wd/todo
  for n in $(seq $num_workers); do
    part=${work[$n]}
    if [ -n "$part" ] ; then
      if [ ${timer[$n]} -gt $tick ] ; then
        continue
      fi
#echo "$n:Finish $part"
      sed -i -e "s/$part//;/^:-/d" $wd/todo
      work[$n]=""
      timer[$n]=""
    fi
  done
  for n in $(seq $num_workers); do
    [ -n "${work[$n]}" ] && continue
    part=$(grep -m 1 ':$' < $wd/todo)
    if [ -z "$part" ] ; then
      break
    fi
    part=${part::1}
    work[$n]=$part
    timer[$n]=$(part_time $part)
#echo "$n:Start $part (${timer[$n]})"
    sed -i -e "s/^$part:/&-/" $wd/todo
  done
  newtick=9999
  for t in ${timer[@]} ; do
    if [ -n "$t" ] && [ "$t" -lt $newtick ] ; then
      newtick=$t
    fi
  done
done

echo $(($tick))

rm -rf $wd
