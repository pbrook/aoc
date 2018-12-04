#! /bin/bash

asleep=
id=
declare -a totals

wd=/tmp/aoc4
rm -rf $wd
mkdir -p $wd

best() {
sort < input.txt | cut -c 16-17,19- | \
(
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
      t=$(($now - $asleep))
      seq $asleep $(($now - 1)) >> $wd/$id.log
      totals[$id]=$((${totals[$id]} + $t))
      ;;
  esac
done
for id in "${!totals[@]}" ; do
  echo ${totals[$id]} ${id}
done 
) | sort -nr | head -n1 | while read t id ; do
  echo $id
done
}

id=$(best)
min=$(cat $wd/$id.log | sort -n | uniq -c | sort -nr | head -n1 | awk '{print $2}')
echo $(($id * $min))
rm -rf $wd
