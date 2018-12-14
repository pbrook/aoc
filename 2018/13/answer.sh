#! /bin/bash

f=input.txt
#f=crash.txt
#f=small.txt

wd=/tmp/aoc13
rm -rf $wd
mkdir -p $wd

declare -A grid
declare -a cstate
sed -e 's/ /x/g;s/\\/L/g' < $f > $wd/inp
first=""
carts=0
y=0
while read row ; do
  width=${#row}
  for ((x=0;x<width;x++)) ; do
    c=${row:x:1}
    tile=""
    case "$c" in
      "<"|">")
        tile="-"
        ;;
      "^"|"v")
        tile="|"
        ;;
      "x")
        c=' '
        ;;
    esac
    grid[$x:$y]="$c"
    if [ -n "$tile" ] ; then
      echo $y $x $c 0 $carts
      cstate[carts]="$tile$x:$y"
      ((carts++))
    fi
  done
  ((y++))
done < $wd/inp > $wd/newcarts

height=$y

render() {
  local x y
  for ((y=0;y<height;y++)) ; do
    for ((x=0;x<width;x++)) ; do
      echo -n "${grid[$x:$y]}"
    done
    echo
  done
}

#render

kill_cart() {
  loc="$1"
  for n in ${!cstate[@]} ; do
    other=${cstate[n]}
    if [ "$loc" = "${other:1}" ] ; then
      grid[$loc]=${other::1}
      cstate[n]=""
      ((carts--))
      break
    fi
  done
}

for ((tick=1;carts>1;tick++)) ; do
  sort -n < $wd/newcarts > $wd/carts
  #echo "Tick $tick ($carts)"
  while read y x c turn cid ; do
    tile="${cstate[cid]::1}"
    if [ -z "$tile" ] ; then
      continue
    fi
    grid[$x:$y]="$tile"
    case "$c" in
      ">") ((x++)) ;;
      "<") ((x--)) ;;
      "^") ((y--)) ;;
      "v") ((y++)) ;;
    esac
    tile=${grid[$x:$y]}
    case "$tile" in
      '/')
        case "$c" in
          ">") c="^" ;;
          "<") c="v" ;;
          "^") c=">" ;;
          "v") c="<" ;;
        esac
        ;;
      'L')
        case "$c" in
          ">") c="v" ;;
          "<") c="^" ;;
          "^") c="<" ;;
          "v") c=">" ;;
        esac
        ;;
      '+')
        case "$c" in
          ">") d="^>v" ;;
          "<") d="v<^" ;;
          "^") d="<^>" ;;
          "v") d=">v<" ;;
        esac
        c=${d:turn:1}
        ((turn=(turn+1)%3))
        ;;
      '>'|'<'|'^'|'v')
        cstate[$cid]=""
        kill_cart "$x:$y" 1>&2
        ((--carts))
        if [ -z "$first" ] ; then
          first="$x,$y"
        fi
#echo "Crash $x $y $cid" 1>&2
        continue
        ;;
    esac
    grid[$x:$y]="$c"
    cstate[cid]="$tile$x:$y"
    echo $y $x $c $turn $cid
#echo Keep: $y $x $c $tile $turn 1>&2
  done < $wd/carts > $wd/newcarts
  #render
  #cat $wd/newcarts
  #for n in ${!cstate[@]} ; do
  #  echo "$n ${cstate[n]}"
  #done
  #read
done

echo "First crash: $first"
for state in ${cstate[@]} ; do
  loc=${state:1}
  echo "Last cart: ${loc/:/,}"
done
