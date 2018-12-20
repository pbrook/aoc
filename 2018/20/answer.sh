#! /bin/bash

furthest=0
far=0

# We have to remember the previous distances because the furthest position
# may be down a detour
declare -A grid

parse() {
  local startx=$1 starty=$2 startd=$3
  local x=$startx y=$starty dist=$startd

  for ((;;)) ; do
    c=${regex:pos:1}
    ((pos++))
    case "$c" in
      '(')
        parse $x $y $dist
        ;;
      '|')
        dist=$startd
        x=$startx
        y=$starty
        ;;
      ')'|'$')
        break
        ;;
      *)
        ((dist++))
        case $c in
          N) ((y--)) ;;
          E) ((x++)) ;;
          S) ((y++)) ;;
          W) ((x--)) ;;
        esac
        prev=${grid[$x:$y]}
        if [ -z "$prev" ] ; then
          if ((dist >= 1000)) ; then
            ((far++))
          fi
        else
          if [ $prev -lt $dist ] ; then
            dist=$prev
          fi
        fi
        grid[$x:$y]=$dist
        if ((dist > furthest)) ; then
          furthest=$dist
        fi
    esac
  done
}

read regex < input.txt

#regex='^ENWWW(NEEE|SSE(EE|N))$'
#regex='^ENNWSWW(NEWS|)SSSEEN(WNSE|)EE(SWEN|)NNN$'
#regex='^ESSWWN(E|NNENN(EESS(WNSE|)SSS|WWWSSSSE(SW|NNNE)))$'
#regex='^WSSEESWWWNW(S|NENNEEEENN(ESSSSW(NWSW|SSEN)|WSWWN(E|WWS(E|SS))))$'
pos=1
parse 0 0 0 
echo $furthest
echo $far
