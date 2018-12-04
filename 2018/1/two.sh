#!/bin/bash

freq=0
declare -A seen
while ! [ -f /tmp/stop.$$ ] ; do
    cat input.txt
done | \
while read delta ; do
    freq=$(($freq+$delta))
    if [ -n "${seen[$freq]}" ] ; then
        echo $freq
        touch /tmp/stop.$$
        cat > /dev/null
        exit
    fi
    seen[$freq]=y
done
