#! /bin/bash

f=input.txt
#f=small.txt

declare -a row

load() {
  read _dummy depth
  read _dummy pos
  width=${pos%,*}
  height=${pos#*,}
}

load < $f

risk=0

erode() {
  ((el=(gi+depth) % 20183))
  ((row[$x]=el))
  ((risk += el%3))
}

for ((x=0;x<=width;x++)) ; do
  ((gi=x * 16807))
  erode
done

for ((y=1;y<=height;y++)) ; do
  ((gi=48271*y))
  x=0
  erode
  for ((x=1;x<=width;x++)) ; do
    ((gi=el*row[$x]))
    erode
  done
done
((risk -= row[$width] % 3))
gi=0
x=$width
y=$height
erode

echo $risk
