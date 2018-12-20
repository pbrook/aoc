#! /bin/bash

# Reverse engineered program code

#target=945
target=10551345
tot=0
for ((i=1;i<=target;i++)) ; do
  if (( (target % i) == 0)) ; then
    ((tot += i))
  fi
done
echo $tot
