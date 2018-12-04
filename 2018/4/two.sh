#! /bin/bash

asleep=
id=

wd=/tmp/aoc4
rm -rf $wd
mkdir -p $wd

sort < input.txt | cut -c 16-17,19- | \
while read now pad1 action pad2 ; do
  now=${now#0}
  case $action in
    "#"*)
      id=${action:1}
      ;;
    asleep)
      asleep=$now
      ;;
    up)
      for x in $(seq $asleep $(($now - 1))) ; do
        echo $x $id
      done
      ;;
  esac
done | sort -n | uniq -c | sort -nr | head -n1 | \
while read num min id ; do
  echo "$(($min * $id))"
done

rm -rf $wd
