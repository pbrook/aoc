#! /bin/sh

set -e

if [ x"$1" = x"--internal" ] ; then
  for n in $(seq $2) ; do
    (
      cd $n
      echo $n
      time target/release/day$n > target/release/result
    )
  done
  exit 0
fi

force=true
if ! [ x"$1" = x"" ] ; then
  force=false
fi

days=25
echo "Building..."
for n in $(seq $days) ; do
  if [ ! -d $n ] ; then
    days=$((n-1))
    break
  fi
  (
    cd $n
    if $force || [ ! -f target/debug/result ] ; then
      mkdir -p target/debug
      cargo run > target/debug/result
      cat target/debug/result
    fi
    RUSTFLAGS="--cfg benchmark" cargo build --release
  )
done

echo "Running release builds..."

time $0 --internal $days

ok=true
for n in $(seq $days) ; do
  if ! diff -u $n/target/debug/result $n/target/release/result ; then
    ok=false
  fi
done
$ok
