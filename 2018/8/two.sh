#! /bin/bash

f=input.txt
#f=small.txt

parse() {
    local nodes meta n val sum=0
    read nodes
    read meta
#echo Node 1>&2
    if [ $nodes = 0 ] ; then
#echo child 1>&2
        for (( ; $meta > 0 ; meta-- )) ; do
            read val
            sum=$(($val + $sum))
        done
    else
        local -a nodeval
        for ((n=1 ; n <= $nodes ; n++)) ; do
            nodeval[$n]=$(parse)
#echo $n ${nodeval[$n]} 1>&2
        done
        for (( ; $meta > 0 ; meta-- )) ; do
            read val
#echo "meta $val ${nodeval[$val]}" 1>&2
            sum=$((${nodeval[$val]} + $sum))
        done
    fi
    echo $sum
}

tr ' ' '\n' < $f | parse
