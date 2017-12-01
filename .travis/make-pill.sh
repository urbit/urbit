#!/bin/bash
# XX use -s instead of hash pill
HASH=$(git -C .. log -1 HEAD --format="%H" -- sys/)

echo FIXME blindly assuming master pill
wget https://bootstrap.urbit.org/$HASH-master.pill -O urbit.pill && exit 0

# if wget failed
if [ $TRAVIS_COMMIT ] && [ $TRAVIS_COMMIT != $HASH ]; then
  echo Directory sys/ not modified in commit $TRAVIS_COMMIT
  # echo For auto-build please tag and push $HASH
  echo FIXME Please manually build and upload $HASH-master.pill
  exit 1
fi
echo STUB should build a pill here
exit 1
