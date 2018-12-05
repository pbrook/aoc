#! /bin/bash

alphabet() {
  for x in $(seq 65 90) ; do
    printf "\x$(printf "%x" $x) "
  done
}

make_pattern() {
  echo -n 's,('
  pipe=""
  for c in $(alphabet) ; do
    echo -n "$pipe$c${c,}|${c,}$c"
    pipe="|"
  done
  echo -n '),,g'
}

pattern=$(make_pattern)

react() {
  sed -E -e "$1:1;$pattern;t1"
}

pre=$(react < input.txt)
echo -n ${pre} | wc -c

for c in $(alphabet) ; do
  echo -n $pre | react "s,[$c${c,}],,g;" | wc -c
done | sort -n | head -n1
