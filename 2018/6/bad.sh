#! /bin/bash

# This isn't even close to working
exit 0
wd=/tmp/aoc6
rm -rf $wd
mkdir -p $wd


n=0
while read x y ; do
  x=${x%,}
  echo $x $y
done < small.txt > $wd/inp

left=$(cut -d' ' -f 1 $wd/inp | sort -n | head -n1)
right=$(cut -d' ' -f 1 $wd/inp | sort -n | tail -n1)
top=$(cut -d' ' -f 2 $wd/inp | sort -n | head -n1)
bot=$(cut -d' ' -f 2 $wd/inp | sort -n | tail -n1)

mand() {
  # Divide the area into 4 quadrants. Find the nearest point in each quadrant,
  # and the nearest point in the half quadrants adjoining the border
  #D|A
  #-+-
  #C|B
  local x0=$1 y0=$2
  local n x y dx dy md
  rm -f $wd/*.md
  while read x y ; do
    dy=$(($y - $y0))
    dx=$(($x0 - $x))
    md=$(( ( ($dx >= 0) ? $dx : -$dx) + ( ($dy >= 0) ? $dy : -$dy) ))
    if [ $md = 0 ] ; then
      continue
    fi
    # The number of spaces we claim in this direction
    md=$(( ($md - 1) / 2))
    echo MD: $dx $dy $md 1>&2
    if [ $dx -ge 0 ] && [ $dy -ge 0 ] ; then
      echo $md >> $wd/A.md
      [ $dx -ge $dy ] && echo $md >> $wd/AB.md
      [ $dx -ge $dy ] && echo $md >> $wd/DA.md
    fi
    if [ $dx -ge 0 ] && [ $dy -le 0 ] ; then
      dy=$((-$dy))
      echo $md >> $wd/B.md
      [ $dx -ge $dy ] && echo $md >> $wd/AB.md
      [ $dx -ge $dy ] && echo $md >> $wd/BC.md
    fi
    if [ $dx -le 0 ] && [ $dy -le 0 ] ; then
      dx=$((-$dx))
      dy=$((-$dy))
      echo $md >> $wd/C.md
      [ $dx -ge $dy ] && echo $md >> $wd/CD.md
      [ $dx -ge $dy ] && echo $md >> $wd/BC.md
    fi
    if [ $dx -le 0 ] && [ $dy -ge 0 ] ; then
      dx=$((-$dx))
      echo $md >> $wd/D.md
      [ $dx -ge $dy ] && echo $md >> $wd/CD.md
      [ $dx -ge $dy ] && echo $md >> $wd/DA.md
    fi
  done < $wd/inp
  for n in A B C D AB BC CD DA ; do
    if [ -f $wd/$n.md ] ; then
      near[$n]=$(sort $wd/$n.md | head -n1)
    fi
  done
}

border() {
  # Project points in an adjacent quadrant onto the border
  local a=${near[$1]} b=${near[$2]}
  if [ -z "$a" ] ; then
    echo $b
  elif [ -z "$b" ] ; then
    echo $a
  else
    echo $(( $a < $b ? $a : $b ))
  fi
}

quadrant() {
  # Area of a quadrant
  # Includes the left border, but not the right
  local I=$1 J=$2 K=$3
  left=${near[$I$J]}
  right=${near[$J$K]}
  if [ -z "$left" ] || [ -z "$right" ] ; then
    echo x
    return
  fi
  mid=${near[$J]}
  echo Q: $left $right $mid 1>&2
  border=$left
  if [ $left = 0 ] || [ $right = 0 ] ; then
    seg=0
  elif [ -z "$mid" ] ; then
    # No point in this quadrant
    seg=$(($left * $right))
  else
    # Project the point in this quadrant to the border
    if [ $left -gt $mid ] ; then
      left=$mid
    fi
    if [ $right -gt $mid ] ; then
      right=$mid
    fi
    # Project a rectangle from the border
    seg=$(($left * $right))
    # Subtract the triangular area excluded by the point in this quadrant
    corner=$((($left + $right) - $mid))
    if [ $corner -gt 0 ] ; then
      seg=$(($seg - ( ($corner * ($corner + 1)) / 2 ) ))
    fi
  fi
  echo $(($seg + $border))
  echo $(($seg + $border)) 1>&2
}

while read x y ; do
  unset near
  declare -A near
  mand $x $y
  echo "Next Point"
  echo "indx:"${!near[@]} 1>&2
  echo "Near:"${near[@]} 1>&2
  a=$(quadrant D A B)
  b=$(quadrant A B C)
  c=$(quadrant B C D)
  d=$(quadrant C D A)
  case "$a$b$c$d" in
    *x*)
      echo infinite
      ;;
    *)
      echo $((1 + $a + $b + $c + $d))
      ;;
  esac
done < $wd/inp

rm -rf $wd
