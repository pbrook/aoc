#! /bin/bash

serial=9005
#serial=42

row0[0]=0
row1[0]=0
row2[0]=0

max=0

for ((y=1 ; $y <= 300; y++)) ; do
  echo "#$y"
  row2=(${row1[@]})
  row1=(${row0[@]})
  unset row0
  row0[0]=0

  prev=0
  pprev=0
  for ((x=1 ; $x <= 300 ; x++)) ; do
    rack=$(($x+10))
    p1=$(($rack * $y))
    p2=$(($p1 + $serial))
    p3=$(($p2 * $rack))
    p4=$(( ($p3 % 1000) / 100))
    p5=$(($p4 - 5))

    sum=$(($p5 + $prev + $pprev))
    row0[$x]=$sum
    sum=$((${row2[$x]} + ${row1[$x]} + $sum))
    #echo "$(($x-2)),$(($y-2)) $sum"
    if [ $sum -gt $max ] ; then
      echo $x $y $sum:$p1 $p2 $p3 $p4 $p5
      max=$sum
      best="$(($x - 2)),$(($y - 2))"
    fi
    pprev=$prev
    prev=$p5
  done
done
echo "$best ($max)"
