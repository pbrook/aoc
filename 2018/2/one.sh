#! /bin/bash

double=0
triple=0
while read box; do
    seen_3=false
    ob=$box
    while true ; do
        if echo $box | grep -qE '(.).*\1.*\1.*\1' ; then
            c=$(echo $box | sed -E 's,.*(.).*\1.*\1.*,\1.*\1,')
            box=$(echo $box | sed -e "s/$c//g")
        elif echo $box | grep -qE '(.).*\1.*\1' ; then
            c=$(echo $box | sed -E 's,.*(.).*\1.*\1.*,\1,')
            box=$(echo $box | sed -e "s/$c//g")
            seen_3=true
        else
            break
        fi
    done
    if $seen_3 ; then
        triple=$(($triple+1))
    fi
    if echo $box | grep -qE '(.).*\1' ; then
        double=$(($double+1))
    fi
done < input.txt

echo $(($triple * $double))
