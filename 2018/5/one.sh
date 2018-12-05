#! /bin/bash

pattern='('
make_pattern() {
  echo -n 's,(0'
  for x in $(seq 65 90) ; do
    upper=$(printf "\x$(printf "%x" $x)")
    lower=$(echo $upper | tr A-Z a-z)
    echo -n "|$upper$lower|$lower$upper"
  done
  echo -n '),,g'
}

pattern=$(make_pattern)
echo -n $(cat input.txt) | sed -E -e ":1;$pattern;t1" | wc -c
