#! /bin/bash

f=input.txt
#f=simple.txt

shopt -s extglob

parse() {
  read _1 _2 state
  read
  match=""
  while read pattern _op result ; do
    if [ x"$result" = x"#" ] ; then
      match="$match|${pattern@Q}"
      continue
    fi
  done
}

parse < $f
offset=0

eval "do_match() {
  case \"\$1\" in
    ${match:1})
      state=\"\${state}#\"
      ;;
    *)
      state=\"\${state}.\"
      ;;
  esac
}"
echo $state
prev=""
gen=0
while [ "$state" != "$prev" ] ; do
  ((gen++))
  src="....${state%%+(.)}...."
  prev="$state"
  prev_offset=$offset
  state=""
  ((offset-=2))
  len=${#src}
  for ((n=0;n<len-4;n++)) ; do
    do_match "${src:n:5}"
  done
  while [ ${state::1} = '.' ] ; do
    state=${state:1}
    ((offset++))
  done
  printf "%2d %2d: %s\n" $offset $gen  "$state"
done

if [ $((offset - prev_offset)) != 1  ] ; then
  echo Unexpected offset
  exit 1
fi
len=${#state}
result=0
count=0
for ((n=0;n<len;n++)) ; do
  if [ ${state:n:1} = '#' ] ; then
    ((result+=(n+offset) ))
    ((count++))
  fi
done
((high=count*50))
((low=result-(count*gen) ))
echo low:$low result:$result gen:$gen
if [ $low -lt 0 ] ; then
  ((high--))
  ((low+=1000000000))
fi

printf "%d%09d\n" $high $low
