#! /bin/bash

# Painfully slow because bash doesn't have a sane way of implementing (big)
# linked lists, but gets the right answer eventually (2-3 hours)
#limit=607331
#limit=2018

declare -A list

list[0]="37"
ll=1
rest=""
n=2

na=0
nb=0
a="37"
b="7"

insert() {
  c="$1"
  #echo "$n $c"
  ((n++))
  rest="$rest$c"
  match="${match:1}$c"
  if [ $match = 607331 ] ; then
    echo $((n-6))
    exit 0
  fi
  if ((n % 1000 == 0)) ; then
    echo "#$n $ll $na $nb"
    if (( ${#rest} > 2000 )) ; then
      echo stash
      list[$ll]=$rest
      rest=""
      ((ll++))
    fi
  fi
}

match=000000
while true; do
  av=${a::1}
  bv=${b::1}
  ((sum=av+bv))
  #echo "$sum $n $a($av) $b($bv)"
  if (( $sum >= 10 )) ; then
    insert 1
    ((sum-=10))
  fi
  insert $sum
  for ((av++;av>0;av--)) ; do
    a="${a:1}"
    if (( ${#a} == 0 )) ; then
      ((na++))
      if ((na == ll)) ; then
        echo wrapa
        if [ -z "$rest" ] ; then
          na=0
          a=${list[0]}
        else
          list[$ll]=$rest
          a=$rest
          rest=""
          ((ll++))
        fi
      else
        a=${list[$na]}
      fi
    fi
  done
  for ((bv++;bv>0;bv--)) ; do
    b="${b:1}"
    if (( ${#b} == 0 )) ; then
      ((nb++))
      if ((nb == ll)) ; then
        echo wrapb
        if [ -z "$rest" ] ; then
          nb=0
          b=${list[0]}
        else
          list[$ll]=$rest
          b=$rest
          rest=""
          ((ll++))
        fi
      else
        b=${list[$nb]}
      fi
    fi
  done
done
