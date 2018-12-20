#! /bin/bash

f=input.txt
#f=small6.txt

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
    echo "$n ${thing_type[n]} ${thing_x[n]}:${thing_y[n]} ${thing_hp[n]}"
  done
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      echo -n "${grid[$x:$y]}"
    done
    echo
  done
}

win() {
  ((round--))
  #echo "Killed all $other_thing after $round rounds"
  #render
  hp=0
  for ((n=0;n<thing_count;n++)) ; do
    if ((thing_hp[n] > 0)) ; then
      ((hp+=thing_hp[n]))
    fi
  done
  #echo "Total hp $hp"
  end_score="$round * $hp = $((round * hp))"
  running=0
}

attack_scan() {
  local x=$1 y=$2 n
  if [ "${grid[$x:$y]}" = $other_thing ] ; then
    for ((n=0;n<thing_count;n++)) ; do
      if (( (thing_x[n] == x) && (thing_y[n] == y) && thing_hp[n] > 0)) ; then
        if ((thing_hp[n] < attack_hp)) ; then
          attack_hp=${thing_hp[n]}
          attack_id=$n
          attack_loc=$x:$y
        fi
      fi
    done
  fi
}

try_attack() {
  attack_hp=999
  attack_id=
  if ((type_count[$other_thing] == 0)) ; then
    win
    return 0
  fi
  attack_scan $x $((y-1))
  attack_scan $((x-1)) $y
  attack_scan $((x+1)) $y
  attack_scan $x $((y+1))
  if [ -z "$attack_id" ] ; then
    return 1
  fi
#echo "Attacking $attack_id"
  if [ $other_thing = 'E' ] ; then
    ((thing_hp[attack_id] -= 3))
  else
    ((thing_hp[attack_id] -= $power))
  fi
  if ((thing_hp[attack_id] <= 0)) ; then
    if [ $other_thing = 'E' ] ; then
      running=0
    else
      moved=1
      recalc=1
      grid[$attack_loc]='.'
      ((type_count[$other_thing]--))
    fi
  fi
  return 0
}

is_loc() {
  [ "$1" = $match_loc ]
}

is_other() {
  [ "${grid[$1]}" = $other_thing ]
}

boundary() {
  if $flood_test $1 ; then
    target=1
  fi
}

scan() {
  target=0
  boundary $((x-1)):$y
  boundary $((x+1)):$y
  boundary $x:$((y-1))
  boundary $x:$((y+1))
}

surrounds() {
  printf "%03d %03d\n" $((y+1)) $x
  printf "%03d %03d\n" $((y-1)) $x
  printf "%03d %03d\n" $y $((x-1))
  printf "%03d %03d\n" $y $((x+1))
}

flood() {
  local x=$1 y=$2
  flood_x=
  flood_y=
  distance=()
  distance[$x:$y]=0
  step=0
  again=1
  for ((;again;)) ; do
    sort < $wd/check > $wd/pending
    rm -f $wd/check
    again=0
#echo "step $step"
    while read y x ; do
      ((x=10#$x,y=10#$y))
#echo "$x:$y ${grid[$x:$y]}"
      if [ "${grid[$x:$y]}" != '.' ] || [ -n "${distance[$x:$y]}" ] ; then
        continue
      fi
#echo "scan $x:$y"
      scan
      if ((target)) ; then
#echo "found $x:$y"
        flood_x=$x
        flood_y=$y
        return
      fi
      distance[$x:$y]=$((step+1))
      again=1
      surrounds >> $wd/check
    done < $wd/pending
    ((step++))
  done
}

move() {
  flood_test=is_other
  surrounds > $wd/check
  flood
  if [ -n "$flood_x" ] ; then
    match_loc="$x:$y"
    flood_test=is_loc
    echo $flood_y $flood_x > $wd/check
    flood
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
#echo "moving to $x:$y"
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
      printf "%03d %03d %d\n" ${thing_y[n]} ${thing_x[n]} $n
      #echo ${thing_x[n]} ${thing_y[n]} $n
    fi
  done | sort > $wd/order
  while read y x id ; do
    ((x=10#$x,y=10#$y))
    run_thing
    if ((running == 0)) ; then
      break
    fi
  done < $wd/order
  recalc=$moved
}

run() {
  echo "Running power $power"
  end_score=
  thing_count=0
  grid=()
  type_count=()
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
  #render
  running=1
  for ((;running;)) ; do
    ((round++))
    run_step
    #render
    #read || break
  done
}

power=4
fail_power=3
win_power=
for ((;;)) ; do
  run
  if [ -z "$end_score" ] ; then
    echo Fail
    fail_power=$power
  else
    echo Win
    win_power=$power
    win_score="$end_score"
  fi
  if [ -z "$win_power" ] ; then
    ((power*=2))
  elif ((win_power==fail_power+1)) ; then
    echo "Best score $win_score"
    break
  else
    ((power=(fail_power+win_power)/2))
  fi
done
