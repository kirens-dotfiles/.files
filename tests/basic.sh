#! /bin/sh

function err {
  exitCode=$1
  shift
  echo $@ 1>&2
  exit $exitCode
}

# Assert that laptop build exists
if test ! -e result/laptop
then
  err 1 "There exists no build laptop!"
fi
