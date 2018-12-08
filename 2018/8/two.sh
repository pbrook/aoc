#! /bin/bash

f=input.txt
#f=small.txt

parse() {
    local nodes meta n val nodeval sum
    read nodes
    read meta
    sum=0
#echo Node 1>&2
    if [ $nodes = 0 ] ; then
#echo child 1>&2
        for n in $(seq $meta) ; do
            read val
            sum=$(($val + $sum))
        done
    else
        declare -a nodeval
        for n in $(seq $nodes) ; do
            nodeval[$n]=$(parse)
#echo $n ${nodeval[$n]} 1>&2
        done
        for n in $(seq $meta) ; do
            read val
#echo "meta $val ${nodeval[$val]}" 1>&2
            sum=$((${nodeval[$val]} + $sum))
        done
    fi
    echo $sum
}

echo $(($(
for x in $(cat $f) ; do
    echo $x
done | parse
)+0))
