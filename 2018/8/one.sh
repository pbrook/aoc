#! /bin/bash

f=input.txt
#f=small.txt

parse() {
    local nodes meta n val
    read nodes
    read meta
    for n in $(seq $nodes) ; do
        parse
    done
    for n in $(seq $meta) ; do
        read val
        echo +$val
    done
}

echo $(($(
for x in $(cat $f) ; do
    echo $x
done | parse
)))
