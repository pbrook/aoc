#! /bin/bash

f=input.txt
#f=small.txt

wd=/tmp/aoc24
mkdir -p $wd

num_units=0
declare -a u_side u_count u_hp u_atype u_apow u_init u_prop1 u_prop2 u_prop3 u_mod1 u_mod2 i_mod3 u_busy
addprop() {
  if [ -z "${u_prop1[n]}" ] ; then
    u_mod1[n]=$1
    u_prop1[n]=$2
  elif [ -z "${u_prop2[n]}" ] ; then
    u_mod2[n]=$1
    u_prop2[n]=$2
  else
    if [ -n "${u_prop3[n]}" ] ; then
      echo "Too many props"
      exit 1
    fi
    u_mod3[n]=$1
    u_prop3[n]=$2
  fi
}

parse_unit() {
  n=$num_units
  u_side[n]=$side
  u_count[n]=$(echo "$line" | cut -d' ' -f 1) 
  u_hp[n]=$(echo "$line" | cut -d' ' -f 5) 
  for x in $(echo "$line" | sed -E -e '/weak/!d;s/.*weak to ([^;)]*)[;)].*/\1/;s/,//') ; do
    addprop weak "$x"
  done
  for x in $(echo "$line" | sed -E -e '/immune/!d;s/.*immune to ([^;)]*)[;)].*/\1/;s/,//') ; do
    addprop immune "$x"
  done
  rest="$(echo "$line" | sed -e 's/.*attack that does //')"
  u_apow[n]=$(echo "$rest" | cut -d' ' -f 1) 
  u_atype[n]=$(echo "$rest" | cut -d' ' -f 2) 
  u_init[n]=$(echo "$rest" | cut -d' ' -f 6) 

  echo "$n: $side c:${u_count[n]} hp:${u_hp[n]} a:${u_apow[n]}:${u_atype[n]} i:${u_init[n]}"
  ((num_units++))
}

parse() {
  while read line ; do
    if [ "$line" = "Immune System:" ] ; then
      side='D'
    elif [ "$line" = "Infection:" ] ; then
      side='A'
    elif [ -n "$line" ] ; then
      parse_unit
    fi
  done
}

calc_damage() {
  ((damage=u_count[n] * u_apow[n]))
  case ${u_atype[n]} in
    ${u_prop1[o]}) mod=${u_mod1[o]} ;;
    ${u_prop2[o]}) mod=${u_mod2[o]} ;;
    ${u_prop3[o]}) mod=${u_mod3[o]} ;;
    *) mod="" ;;
  esac
  case "$mod" in
    weak) ((damage*=2)) ;;
    immune) ((damage=0)) ;;
    '') ;;
    *) echo bad mod ; exit 0 ;;
  esac
}

find_target() {
  side=${u_side[n]}
  #echo "$side$n: c:${u_count[n]} hp:${u_hp[n]} a:${u_apow[n]}" 1>&2
  target=
  target_damage=-1
  for ((o=0; o<num_units;o++)) ; do
    if [ ${u_side[o]} = $side ] || [ -n "${u_busy[o]}" ] || [ ${u_count[o]} -le 0 ] ; then
      continue
    fi
    calc_damage
    if ((damage == target_damage)) ; then
      ((ep_t=u_count[target]*u_apow[target]))
      ((ep_o=u_count[o]*u_apow[o]))
      if ((ep_o < ep_t)) ; then
        damage=-1
      elif ((ep_o == ep_t)) ; then
        if ((u_init[o] < u_init[target] )) ; then
          damage=-1
        fi
      fi
    fi
    if ((damage >= target_damage)) ; then
      target=$o
      target_damage=$damage
    fi
  done
  if [ -n "$target" ] && ((target_damage > 0)) ; then
    u_busy[target]=1
    printf "%03d %d %d\n" ${u_init[n]} $n $target
  fi
}

attack() {
  if ((u_count[n] <= 0)) ; then
    return
  fi
  calc_damage $target
  ((killed=damage/u_hp[o]))
  #echo "${u_side[n]}$n -> $o ($killed/$damage)"
  ((u_count[o]-=killed))
}

count_units() {
  totA=0
  totD=0
  for ((n=0;n<num_units;n++)) ; do
    if ((u_count[n] <=0 )) ; then
      continue
    fi
    ((tot${u_side[n]} += u_count[n]))
  done
}

parse < $f
count_units

for ((;totA && totD;)) ; do
  #echo $totA $totD
  for ((n=0;n<num_units;n++)) ; do
    if ((u_count[n]>0)) ; then
      printf "%06d %03d %d\n" $((u_count[n] * u_apow[n])) ${u_init[n]} $n
    fi
  done | sort -r > $wd/order

  u_busy=()
  while read _power _init n ; do
    find_target
  done < $wd/order | sort -r > $wd/attack

  while read _init n o ; do
    attack
  done < $wd/attack

  count_units
done 2>&1
echo $((totA+totD))
rm -rf $wd
