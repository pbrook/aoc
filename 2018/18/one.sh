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

render() {
  echo "cycle $cycle o:$nopen y:$nyard t:$ntree"
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      echo -n ${grid[$x:$y]}
    done
    echo
  done
}

cycle=0
#render
for ((;cycle<10;)) ; do
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
  #render
  #read || break
done
echo $((ntree*nyard))
