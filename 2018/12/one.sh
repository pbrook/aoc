#! /bin/bash

f=input.txt
#f=simple.txt

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

for ((gen=1;gen<=20;gen++)) ; do
  src="....${state%....}...."
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

len=${#state}
result=0
for ((n=0;n<len;n++)) ; do
  if [ ${state:n:1} = '#' ] ; then
    ((result+=(n+offset)))
  fi
done
echo $result
