#! /bin/bash

serial=9005
#serial=18

max=0

declare -A power

calc() {
  rack=$(($1 + 10))
  p1=$(( ($rack * $2 + $serial) * $rack))
  power[$1:$2]=$(( ($p1 % 1000) / 100 - 5))
}

for ((y=1 ; $y <= 300 ; y++)) ; do
  for ((x=1 ; $x <= 300 ; x++)) ; do
    calc $x $y
  done
done

power() {
  result=${table[$1:$2]}
}

loop() {
  start=0
  limit=$((300 - $size))
  for ((y=1 ; $y <= $size ; y++)) ; do
    for ((x=1 ; $x <= $size ; x++)) ; do
      start=$(($start + ${power[$x:$y]}))
    done
  done

  for ((y=1 ; $y <= $limit ; y++)) ; do
    sum=$start
    for ((x=1 ; $x <= $limit ; x++)) ; do
      if [ $sum -gt $max ] ; then
        max=$sum
        best="$x,$y,$size"
        echo $best $max
      fi
      for ((n=0 ; $n < $size; n++)) ; do
        sum=$(($sum + ${power[$(($x + $size)):$(($y + $n))]} - ${power[$x:$(($y + $n))]}))
      done
    done
    if [ $sum -gt $max ] ; then
      max=$sum
      best="$x,$y,$size"
      echo $best $max
    fi
    for ((n=1 ; $n <= $size; n++)) ; do
      start=$(($start + ${power[$n:$(($y + $size))]} - ${power[$n:$y]}))
    done
  done
}

for ((size=1 ; $size < 300 ; size++)) ; do
  echo "#$size $(date)"
  loop
done

echo "$best ($max)"
