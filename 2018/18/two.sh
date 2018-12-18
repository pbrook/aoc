#! /bin/bash

f=input.txt
#f=small.txt

declare -A grid newgrid

scan() {
  case "${grid[$1]}" in
    '.') ((open++)) ;;
    '#') ((yard++)) ;;
    '|') ((tree++)) ;;
  esac
}

open=0
yard=0
tree=0
for ((y=0;;y++)) ; do
  read line || break
  width=${#line}
  for ((x=0;x<width;x++)) ; do
    grid[$x:$y]=${line:x:1}
    scan $x:$y
  done
done < $f
height=$y
nopen=$open
ntree=$tree
nyard=$yard

evolve() {
  open=0
  yard=0
  tree=0

  scan $x:$((y+1))
  scan $((x+1)):$((y+1))
  scan $((x+1)):$y
  scan $((x+1)):$((y-1))
  scan $x:$((y-1))
  scan $((x-1)):$((y-1))
  scan $((x-1)):$y
  scan $((x-1)):$((y+1))

  loc="$x:$y"
  c="${grid[$loc]}"
  case "$c" in
    '.')
      if ((tree >= 3)) ; then
        c='|'
        ((nopen--,ntree++))
      fi
      ;;
    '|')
      if ((yard >= 3)); then
        c='#'
        ((ntree--,nyard++))
      fi
      ;;
    '#')
      if (( (yard == 0) || (tree == 0) )) ; then
        ((nyard--,nopen++))
        c='.'
      fi
      ;;
  esac
  newgrid[$loc]=$c
}

render_state() {
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      echo -n ${grid[$x:$y]}
    done
    echo
  done
}

render() {
  echo "cycle $cycle o:$nopen y:$nyard t:$ntree"
  render_state
}

window=16
cycle=0
next_save=$window
saved_cycle=0
saved_state=""
saved_nopen=0
remainder=""
#render
for ((;;)) ; do
  ((cycle++))
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      evolve
    done
  done
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      loc=$x:$y
      grid[$loc]=${newgrid[$loc]}
    done
  done
  echo "$cycle $((cycle - saved_cycle)) $nopen $nyard $ntree"
  if [ -n "$remainder" ] ; then
    ((remainder--))
    if ((remainder == 0)) ; then
      break
    fi
  else
    if [ $saved_nopen = $nopen ] ; then
      new_state="$(render_state)"
      if [ "$new_state" = "$saved_state" ] ; then
        echo "loop from $saved_cycle to $cycle"
        next_save=0
        ((mod=cycle-saved_cycle))
        ((remainder=(1000000000-cycle)%mod ))
        echo "Need $remainder more cycles"
        if ((remainder == 0)) ; then
          break
        fi
      else
        echo "Near loop"
      fi
    fi
    if (( cycle == next_save))  ; then
      new_state="$(render_state)"
      saved_cycle=$cycle
      saved_nopen=$nopen
      saved_state="$new_state"
      echo "$saved_state" >> /tmp/saved
      echo "Save $nopen"
      ((window *= 2, next_save += window))
    fi
  fi
  #render
  #read || break
done
echo $((ntree*nyard))
