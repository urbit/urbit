#!/bin/bash
set -euo pipefail
set -x

if [ ! -f ./pin-brass-pill.txt ]; then
  echo "missing .travis/pin-brass-pill.txt"
  exit 1
fi

PILL_NAME=$(cat ./pin-brass-pill.txt | tr -d [:space:])

wget https://bootstrap.urbit.org/$PILL_NAME -O brass.pill && exit 0

echo "$PILL_NAME download failed"
exit 1
