#! /bin/bash

asleep=
id=
declare -a totals

wd=/tmp/aoc4
rm -rf $wd
mkdir -p $wd

best() {
sort < input.txt | cut -c 16-17,19- | ( \
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
      totals[$id]=$((${totals[$id]} + $t))
      for x in $(seq $asleep $(($now - 1))) ; do
        echo $x $id
      done >> $wd/$id.log
      ;;
  esac
done 
for id in "${!totals[@]}" ; do
  echo ${totals[$id]} ${id}
done | sort -nr | head -n1 | while read t id ; do
  echo $id
done
)
}

id=$(best)
min=$(cat $wd/$id.log | sort -n | uniq -c | sort -nr | head -n1 | awk '{print $2}')
echo $(($id * $min))

cat $wd/*.log | sort -n | uniq -c | sort -nr | head -n1 | \
while read num min id ; do
  echo "$(($min * $id))"
done

rm -rf $wd
