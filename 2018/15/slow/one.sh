#! /bin/bash

f=input.txt
#f=small.txt

wd=/tmp/aoc15
rm -rf $wd
mkdir -p $wd

declare -A grid type_count distance

add_thing() {
  thing_x[thing_count]=$x
  thing_y[thing_count]=$y
  thing_hp[thing_count]=200
  thing_type[thing_count]=$1
  ((type_count[$1]++))
  ((thing_count++))
}

render() {
  local x y c n
  echo "round:$round G:${type_count['G']} E:${type_count['E']}"
  for ((n=0;n<thing_count;n++)) ; do
:    echo "$n ${thing_type[n]} ${thing_x[n]}:${thing_y[n]} ${thing_hp[n]}"
  done
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      echo -n "${grid[$x:$y]}"
    done
    echo
  done
}

win() {
  echo "Ended after $round rounds"
  hp=0
  for ((n=0;n<thing_count;n++)) ; do
    if ((thing_hp[n] > 0)) ; then
      ((hp+=thing_hp[n]))
    fi
  done
  echo "Total hp $hp"
  echo $((round *hp))
  exit 0
}

attack() {
  local x=$1 y=$2 n
  if [ "${grid[$x:$y]}" = $other_thing ] ; then
    for ((n=0;n<thing_count;n++)) ; do
      if (( (thing_x[n] == x) && (thing_y[n] == y) && thing_hp[n] > 0)) ; then
        ((thing_hp[n] -= 3))
        if ((thing_hp[n] <= 0)) ; then
          moved=1
          recalc=1
          grid[$x:$y]='.'
          ((type_count[$other_thing]--))
          if ((type_count[$other_thing] == 0)) ; then
            win
          fi
        fi
      fi
    done
    return 0
  fi
  return 1
}

try_attack() {
  attack $((x+1)) $y && return 0
  attack $((x-1)) $y && return 0
  attack $x $((y+1)) && return 0
  attack $x $((y-1)) && return 0
  return 1
}

is_loc() {
  [ "$1" = $match_loc ]
}

is_other() {
  [ "${grid[$1]}" = $other_thing ]
}

boundary() {
  local d="${distance[$1]}"
  if $flood_test $1 ; then
    target=1
  fi
  if [ "$d" = $step ] ; then
    fill=1
  fi
}

scan() {
  fill=0
  target=0
  boundary $((x-1)):$y
  boundary $((x+1)):$y
  boundary $x:$((y-1))
  boundary $x:$((y+1))
}

flood() {
  local x=$1 y=$2
  flood_x=
  flood_y=
  ((xmin=x-1,xmax=x+1,ymin=y-1,ymax=y+1))
  distance=()
  distance[$x:$y]=0
  step=0
  again=1
  for ((;again;)) ; do
    again=0
#echo "step $step"
    for ((y=ymin;y<=ymax;y++)) ; do
      for ((x=xmin;x<=xmax;x++)) ; do
#echo "$x:$y ${grid[$x:$y]}"
        if [ "${grid[$x:$y]}" != '.' ] || [ -n "${distance[$x:$y]}" ] ; then
          continue
        fi
        scan
        if ((fill)) ; then
          if ((x==xmin && x>0)) ; then
            ((xmin--))
          fi
          if ((x==xmax)) ; then
            ((xmax++))
          fi
          if ((y==ymin && y>0)) ; then
            ((ymin--))
          fi
          if ((y==ymax)) ; then
            ((ymax++))
          fi
          if ((target)) ; then
            flood_x=$x
            flood_y=$y
            return
          fi
          distance[$x:$y]=$((step+1))
          again=1
        fi
      done
    done
    ((step++))
  done
}

scanxy() {
  local x=$flood_x y=$flood_y
  scan
}

move() {
  flood_test=is_other
  flood $x $y
  if [ -n "$flood_x" ] ; then
    match_loc="$x:$y"
    flood_test=is_loc
    scanxy
    if ! ((target)) ; then
      flood $flood_x $flood_y
    fi
    if [ -z "$flood_x" ] ; then
      echo "Bork!"
      exit 1
    fi
    moved=1
    recalc=1
    grid[$x:$y]='.'
    grid[$flood_x:$flood_y]=${thing_type[id]}
    x=$flood_x
    y=$flood_y
    thing_x[id]=$x
    thing_y[id]=$y
  fi
}

run_thing() {
#echo "Running $id ($x:$y) ${thing_type[id]}"
  case ${thing_type[id]} in
    E) other_thing='G' ;;
    G) other_thing='E' ;;
  esac
  if ((thing_hp[id] <= 0)) ; then
    return
  fi
  if try_attack ; then
    return
  fi
  if ((recalc)) ; then
    move
    try_attack
  fi
}

run_step() {
  local n
  moved=0
  for ((n=0;n<thing_count;n++)) ; do
    if ((thing_hp[n] > 0)) ; then
      echo ${thing_x[n]} ${thing_y[n]} $n
    fi
  done | sort -n > $wd/order
  while read x y id ; do
    run_thing
  done < $wd/order
  recalc=$moved
}

for ((y=0;;y++)) ; do
  read line || break
  width=${#line}
  for ((x=0;x<width;x++)) ; do
    c=${line:x:1}
    grid[$x:$y]=$c
    case $c in
      E|G)
        add_thing $c
        ;;
    esac
  done
done < $f
height=$y
round=0
recalc=1
render
for ((;;)) ; do
  ((round++))
  run_step
  render
  #read || break
done
