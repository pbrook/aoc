#! /bin/bash

x=$(head -n1 input.txt)
len=$((${#x}-1))
for x in $(seq $len); do
    case x in
        1)
            cf=2-
            ;;
        $len)
            cf=-$x
            ;;
        *)
            cf=-$x,$(($x+2))-
            ;;
    esac
    cat input.txt | cut -c $cf | sort | uniq -c | awk '/2/{print $2}'
done
