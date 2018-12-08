#! /bin/bash

f=input.txt
#f=small.txt

sum=0

parse() {
    local nodes meta val
    read nodes
    read meta
    for (( ; $nodes > 0 ; nodes-- )) ; do
        parse
    done
    for (( ; $meta > 0 ; meta-- )) ; do
        read val
        sum=$(($sum + $val))
    done
}

tr ' ' '\n' < $f | (
parse
echo $sum
)
