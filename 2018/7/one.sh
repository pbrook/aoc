#! /bin/bash

wd=/tmp/aoc7
rm -rf $wd
mkdir -p $wd

declare -A deps
cat input.txt | cut -d' ' -f2,8 > $wd/inp

while read dep part ; do
  deps[$part]="${deps[$part]}$dep"
  # make sure all parts are in the array
  deps[$dep]="${deps[$dep]}"
done < $wd/inp

for part in ${!deps[@]} ; do
  echo "$part:${deps[$part]}"
done | sort > $wd/todo

order=""
while true; do
  part=$(cat $wd/todo | grep -m 1 ':$')
  [ -z "$part" ] && break
  part=${part::1}
  order="$order$part"
  sed -i -e "s/$part//;/^:$/d" $wd/todo
done

echo $order

rm -rf $wd
