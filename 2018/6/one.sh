#! /bin/bash

wd=/tmp/aoc6
rm -rf $wd
mkdir -p $wd


n=0
while read x y ; do
  x=${x%,}
  echo $x $y
done < input.txt > $wd/inp

left=$(cut -d' ' -f 1 $wd/inp | sort -n | head -n1)
right=$(cut -d' ' -f 1 $wd/inp | sort -n | tail -n1)
top=$(cut -d' ' -f 2 $wd/inp | sort -n | head -n1)
bot=$(cut -d' ' -f 2 $wd/inp | sort -n | tail -n1)

mand() {
  # Divide into 4 quadrants, plus 4 additional groups for locations
  # exactly on the border.  Pick the nearest location in each group
  # Value stored is the number of locations we can claim in that direction
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
    md=$(( ($md - 1) / 2))
    if [ $dx -gt 0 ] ; then
      if [ $dy -gt 0 ] ; then
        echo $md >> $wd/A.md
      elif [ $dy -lt 0 ] ; then
        echo $md >> $wd/B.md
      else
        echo $md >> $wd/AB.md
      fi
    elif [ $dx -lt 0 ] ; then
      if [ $dy -gt 0 ] ; then
        echo $md >> $wd/D.md
      elif [ $dy -lt 0 ] ; then
        echo $md >> $wd/C.md
      else
        echo $md >> $wd/CD.md
      fi
    else # dx = 0
      if [ $dy -gt 0 ] ; then
        echo $md >> $wd/DA.md
      elif [ $dy -lt 0 ] ; then
        echo $md >> $wd/BC.md
      fi
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
  left=$(border $I $I$J)
  right=$(border $K $J$K)
  mid=${near[$J]}
  if [ -z "$mid" ] ; then
    # No point in this quadrant
    if [ -z "$left" ] || [ -z "$right" ] ; then
      echo x
    else
      echo $(( ($left - 1) * $right))
    fi
  else
    # Project the point in this quadrant to the border
    if [ -z "$left" ] || [ $left -gt $mid ] ; then
      left=$mid
    fi
    if [ -z "$right" ] || [ $right -gt $mid ] ; then
      right=$mid
    fi
    # Project a rectangle from the border
    box=$(($left * $right))
    # Subtract the triangular area excluded by the point in this quadrant
    corner=$((($left + $right) - ($mid + 1)))
    if [ $corner -gt 0 ] ; then
      box=$(($box - ($corner * ($corner + 1) ) ))
    fi
    # And remove the right border
    echo $(($box - $right))
  fi
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
