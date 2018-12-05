#! /bin/bash

alphabet() {
  for x in $(seq 65 90) ; do
    c=$(printf "\x$(printf "%x" $x)")
    echo $c $(echo $c | tr A-Z a-z)
  done
}

make_pattern() {
  echo -n 's,('
  pipe=""
  alphabet | while read upper lower ; do
    echo -n "$pipe$upper$lower|$lower$upper"
    pipe="|"
  done
  echo -n '),,g'
}

pattern=$(make_pattern)

react() {
  sed -E -e "$1:1;$pattern;t1"
}

pre=$(react < input.txt)
alphabet | while read upper lower ; do
  echo -n $pre | react "s,[$upper$lower],,g;" | wc -c
done | sort -n | head -n1

