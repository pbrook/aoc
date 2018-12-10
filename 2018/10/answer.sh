#! /bin/bash

f=input.txt
#f=small.txt

width=79
height=10
wd=/tmp/aoc10
rm -rf $wd
mkdir -p $wd

sed -e 's/[^- 0-9]//g' $f > $wd/inp

declare -a sky
blank="$(printf "%${width}s")"

when=0
step=1
while true; do
  x_max=0
  x_min=9999
  y_max=0
  y_min=9999
  minstep=1
  when=$(($when + $step))
  while read x y u v ; do
    x=$(($x + ($u * $step)))
    y=$(($y + ($v * $step)))
    echo $x $y $u $v
    [ $x -gt $x_max ] && x_max=$x
    [ $y -gt $y_max ] && y_max=$y
    [ $x -lt $x_min ] && x_min=$x
    [ $y -lt $y_min ] && y_min=$y
    if [ $x -lt 0 ] ; then
      ns=$(( ($x) / -($u) ))
      [ $ns -gt  $minstep ] && minstep=$ns
    fi
  done < $wd/inp > $wd/outp
  mv $wd/outp $wd/inp
  if [ $minstep -gt 10 ] ; then
    step=$minstep
  else
    step=1
  fi

  echo ${when}s $x_min $y_min $x_max $y_max

  if [ $(($y_max - $y_min)) -gt $(($height - 1)) ] ; then
    continue
  fi

  for ((y=0 ; y < $height ; y++)) ; do
    sky[$y]="$blank"
  done
  while read x y u v ; do
    x=$(($x-$x_min))
    y=$(($y-$y_min))
    if [ $y -ge 0 ] && [ $y -lt $width ] ; then
      if [ $x -ge 0 ] && [ $x -lt $width ] ; then
        row="${sky[$y]}"
        sky[$y]="${row::$x}#${row:$(($x+1))}"
      fi
    fi
  done < $wd/inp
  for ((y=0 ; y < $height ; y++)) ; do
    echo "${sky[$y]}"
  done
  break
  read || break
done
