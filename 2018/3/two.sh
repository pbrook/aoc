d=/dev/shm/cloth
rm -rf $d
mkdir $d

cat input.txt | sed -e 's/[^0-9]/ /g' | \
while read id x0 y0 w h ; do
    for x in $(seq $x0 $(($x0 + $w - 1))) ; do
        for y in $(seq $y0 $(($y0 + $h - 1))) ; do
            if [ -f $d/$x:$y ] ; then
                echo X > $d/$x:$y
            else
                echo $id > $d/$x:$y
            fi
        done
    done
done
echo pass1
cat input.txt | sed -e 's/[^0-9]/ /g' | \
while read id x0 y0 w h ; do
    n=false
    for x in $(seq $x0 $(($x0 + $w - 1))) ; do
        for y in $(seq $y0 $(($y0 + $h - 1))) ; do
            if [ $(cat $d/$x:$y) = 'X' ]; then
                n=true
                break;
            fi
        done
        $n && break
    done
    if ! $n ; then
        echo $id
        break
    fi
done
rm -rf $d
