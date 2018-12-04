#!/bin/bash
set -euo pipefail
set -x

# add urbit-runner to $PATH
PATH=./node_modules/.bin/:$PATH

# XX use -s instead of hash pill
HASH=$(git -C .. log -1 HEAD --format=%H -- sys/)
export PILL_NAME="git-${HASH:0:10}"

if [ ! ${PILL_FORCE:-} ]; then
  : Trying pill for commit
  wget https://bootstrap.urbit.org/$PILL_NAME.pill -O urbit.pill && exit 0
fi

# if wget failed

if [ ${TRAVIS_COMMIT:-} ] && [ $TRAVIS_COMMIT != $HASH ]; then
  : Directory sys/ not modified in commit $TRAVIS_COMMIT
  : FIXME ignoring, as current sys/ commits are unlikely to contain the pill-build code
  :
#   : For auto-build please tag and push $HASH
#   exit 1
fi

mkdir prev
{
  : Pilling: trying pinned fakezod
  wget -i pin-parent-pill-pier.url -O - | tar xvz -C prev/ &&
  : Downloaded prev/zod &&
   urbit-runner -S prev/zod <<'  .'
    |autoload |
    |mount %
  .
  [ $? = 0 ] && cp -r ../sys prev/zod/home/ &&
    cp ../gen/solid.hoon prev/zod/home/gen/ &&
    cp ../lib/pill.hoon  prev/zod/home/lib/
} || {
  : Pilling: Parent-pill pier not available, trying preceding pill commit
  HASH2=$(git -C .. log -2 $HASH --format=%H -- sys/ | tail -1)
  PILL_NAME2="git-${HASH2:0:10}"
  wget https://bootstrap.urbit.org/$PILL_NAME2.pill -O prev/urbit.pill &&
  ([ -d prev/zod ] && rm -r prev/zod || true) &&
  urbit-runner -A .. -B prev/urbit.pill -cSF zod prev/zod <<'  .'
    %booted-prev-zod
  .
} || {
  : Pilling: Out of ideas
  exit 1
}

: Pier created, soliding actual pill
urbit-runner -S prev/zod <<.
  |label %home %$PILL_NAME
  .urbit/pill +solid /==/$PILL_NAME/sys, =dub &
.

cp prev/zod/.urb/put/urbit.pill urbit.pill
mkdir built-pill; cp urbit.pill built-pill/$PILL_NAME.pill

:
: Created $PILL_NAME.pill, to be uploaded if tests pass
:
