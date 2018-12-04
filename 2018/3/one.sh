d=/tmp/cloth
rm -rf $d
mkdir $d

cat input.txt | sed -e 's/[^0-9]/ /g' | \
while read id x0 y0 w h ; do
    for x in $(seq $x0 $(($x0 + $w - 1))) ; do
        for y in $(seq $y0 $(($y0 + $h - 1))) ; do
            if [ -f $d/$x:$y ] ; then
                echo 1 > $d/$x:$y
            else
                echo 0 > $d/$x:$y
            fi
        done
    done
done
find /tmp/cloth -type f | xargs cat | grep 1 | wc -l
rm -rf /tmp/cloth
