#!/bin/bash
set -euo pipefail
set -x

RESULT=$1

if [[ ${RESULT} -eq 0 ]]; then
  exit 0
else
  for i in $(find ./ -maxdepth 1 -name 'core*' -print)
  do
    gdb urbit core* -ex "thread apply all bt" -ex "set pagination 0" -batch
  done
fi

echo "build failed with status code $RESULT"
exit $RESULT
