#! /bin/bash

limit=607331
#limit=2018

declare -A list

list[0]=3
list[1]=7
n=2

a=0
b=1

while (( n < (limit + 10) )) ; do
  if ((n % 10000 == 0)) ; then
    echo "#$n"
  fi
  ((av=list[$a],bv=list[$b],sum=av+bv))
  #echo "$sum $n $a($av) $b($bv)"
  if [ $sum -ge 10 ] ; then
    ((list[$n]=1,n++,sum-=10))
  fi
  ((list[$n]=sum,n++))
  ((a=(a+av+1)%n))
  ((b=(b+bv+1)%n))
done
for ((n=limit;n<limit+10;n++)) ; do
  echo -n ${list[$n]}
done
echo
