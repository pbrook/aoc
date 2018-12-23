#! /bin/bash

f=input.txt
#f=small2.txt

wd=/tmp/aoc23
mkdir -p $wd

tr 'pos=<>,r' '        ' < $f | sort -rk 4 > $wd/inp

big=999999999
count=0
((x0=big,y0=big,z0=big))
((x1=-big,y1=-big,z1=-big))
while read x y z r ; do
  (( x < x0 )) && x0=$x
  (( y < y0 )) && y0=$y
  (( z < z0 )) && z0=$z
  (( x > x1 )) && x1=$x
  (( y > y1 )) && y1=$y
  (( z > z1 )) && z1=$z
  ((count++))
done < $wd/inp

distance() {
  # val min max
  if (( $1 < $2 )) ; then
    ((d += $2 - $1))
  elif (($1 > $3)) ; then
    ((d += $1 - $3))
  fi
}

bisect() {
  local x0=$1 y0=$2 z0=$3 x1=$4 y1=$5 z1=$6
  local xm ym zm xp yp zp
  local nb=0

  if ((x1 < x0 || y1 < y0 || z1 < z0)) ; then
    return
  fi
  while read x y z r ; do
    d=0
    distance $x $x0 $x1
    distance $y $y0 $y1
    distance $z $z0 $z1
    if (( d <= r )) ; then
      ((nb++))
    fi
  done < $wd/inp
  #echo bisect $nb/$target $x0 $y0 $z0 $x1 $y1 $z1
  if ((nb < target)) ; then
    return
  fi
  if ((x0 == x1 && y0 == y1 && z0 == z1)) ; then
    ((d = (x0>=0)?x0:-x0))
    ((d += (y0>=0)?y0:-y0))
    ((d += (z0>=0)?z0:-z0))
    #echo "best $d/$target $x0 $y0 $z0"
    if ((d < best)) ; then
      best=$d
    fi
    return
  fi
  ((xm=(x0+x1)/2,xp=xm+1))
  ((ym=(y0+y1)/2,yp=ym+1))
  ((zm=(z0+z1)/2,zp=zm+1))
  bisect $x0 $y0 $z0 $xm $ym $zm
  bisect $xp $y0 $z0 $x1 $ym $zm
  bisect $x0 $yp $z0 $xm $y1 $zm
  bisect $xp $yp $z0 $x1 $y1 $zm
  bisect $x0 $y0 $zp $xm $ym $z1
  bisect $xp $y0 $zp $x1 $ym $z1
  bisect $x0 $yp $zp $xm $y1 $z1
  bisect $xp $yp $zp $x1 $y1 $z1
}

target=$count
best=$big
while (( best == big )) ; do
  echo "trying $target"
  bisect $x0 $y0 $z0 $x1 $y1 $z1
  ((target--))
done
echo $best
