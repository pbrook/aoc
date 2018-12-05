#! /bin/bash

alphabet() {
  for x in $(seq 65 90) ; do
    c=$(printf "\x$(printf "%x" $x)")
    echo $c $(echo $c | tr A-Z a-z)
  done
}

make_pattern() {
  echo -n 's,(0'
  alphabet | while read upper lower ; do
    echo -n "|$upper$lower|$lower$upper"
  done
  echo -n '),,g'
}

pattern=$(make_pattern)

react() {
  echo -n $(cat input.txt) | sed -E -e "s,[$1$2],,g;:1;$pattern;t1" | wc -c
}

alphabet | while read upper lower ; do
  react $upper $lower
done | sort -n | head -n1

