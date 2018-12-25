#! /bin/bash

f=input.txt
#f=small2.txt

wd=/tmp/aoc25
mkdir -p $wd

tr ',' ' ' < $f > $wd/inp


load() {
  rm -rf $wd/cons
  mkdir -p $wd/cons
  read line
  echo $line > $wd/cons/0
  n=1
  while read x0 y0 z0 w0 ; do
    c=""
    for f in $wd/cons/* ; do
      while read x y z w ; do
        ((d=(x>x0)?x-x0:x0-x))
        ((d+=(y>y0)?y-y0:y0-y))
        ((d+=(z>z0)?z-z0:z0-z))
        ((d+=(w>w0)?w-w0:w0-w))
        if ((d<=3)) ; then
          if [ -z "$c" ] ; then
            c=$f
          else
            cat $f >> $c
            rm $f
          fi
          break
        fi
      done < $f
    done
    if [ -z "$c" ] ; then
      c=$wd/cons/$n
      ((n++))
    fi
    echo $x0 $y0 $z0 $w0 >> $c
  done < $wd/inp
}

load < $wd/inp

ls $wd/cons | wc -l

#rm -rf $wd
