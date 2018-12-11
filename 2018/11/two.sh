#! /bin/bash

# Bash arrays start to get a bit slow with 90k elements, so this takes a few
# seconds per iteration (total runtime ~20min)

# The algorithm is O(N^3) - Each of N square sizes requires and O(1) pass over
# the N*N area.  In practice larger squares require less work, so it speeds up
# towards the end

serial=9005
#serial=18

max=0

declare -A power total

calc() {
  rack=$(($1 + 10))
  p1=$(( ($rack * $2 + $serial) * $rack))
  val=$(( ($p1 % 1000) / 100 - 5))
  power[$1:$2]=$val
  total[$1:$2]=$val
}

for ((y=1 ; $y <= 300 ; y++)) ; do
  for ((x=1 ; $x <= 300 ; x++)) ; do
    calc $x $y
  done
done

loop() {
  limit=$((301 - $size))
  for ((x=1 ; $x <= $limit ; x++)) ; do
    sum=0
    nx=$(($x+$size-1))
    for ((ny=1 ; $ny < $size; ny++)); do
      sum=$(($sum + ${power[$nx:$ny]}))
    done
    for ((y=1 ; $y <= $limit ; y++)) ; do
      e="$x:$y"
      total[$e]=$((${total[$e]} + $sum))
      #echo $e $nx:$ny
      sum=$(($sum + ${power[$nx:$ny]} - ${power[$nx:$y]}))
      ny=$(($ny+1))
    done
  done
  for ((y=1 ; $y <= $limit ; y++)) ; do
    sum=0
    ny=$(($y+$size-1))
    for ((nx=1 ; $nx <= $size; nx++)); do
      sum=$(($sum + ${power[$nx:$ny]}))
    done
    for ((x=1; $x <= $limit ; x++)) ; do
      e="$x:$y"
      val=$((${total[$e]} + $sum))
      total[$e]=$val
      if [ $val -gt $max ] ; then
        max=$val
        best="$x,$y,$size"
        echo $best $max
      fi
      sum=$(($sum + ${power[$nx:$ny]} - ${power[$x:$ny]}))
      nx=$(($nx+1))
    done
  done
}

for ((size=2 ; $size < 300 ; size++)) ; do
  echo "#$size $(date)"
  loop
done

echo "$best ($max)"
