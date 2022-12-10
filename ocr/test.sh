#! /bin/sh

for f in test_*; do
  expect=$(head -n 1 $f)
  result=$(tail -n +2 $f | ./ocr.py)
  if [ "x$expect" != "x$result" ] ; then
    echo "FAIL $f"
    echo "expected $expect"
    echo "result   $result"
  fi
done
