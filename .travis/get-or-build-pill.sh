#!/bin/bash
set -euo pipefail
# set -x

# XX use -s instead of hash pill
HASH=$(git -C .. log -1 HEAD --format=%H -- sys/)
export PILL_NAME="git-${HASH:0:10}"

if [ ! ${PILL_FORCE:-} ]; then
  wget https://bootstrap.urbit.org/$PILL_NAME.pill -O urbit.pill && exit 0
fi

# if wget failed

if [ ${TRAVIS_COMMIT:-} ] && [ $TRAVIS_COMMIT != $HASH ]; then
  echo Directory sys/ not modified in commit $TRAVIS_COMMIT
  echo FIXME ignoring, as current sys/ commits are unlikely to contain the pill-build code
  echo
#   echo For auto-build please tag and push $HASH
#   exit 1
fi

mkdir prev
{
  echo Pilling: trying pinned fakezod
  wget -i pin-parent-pill-pier.url -O - | tar xvz -C prev/ &&
  echo Downloaded prev/zod
} || {
  echo Pilling: Parent-pill pier not available, trying preceding pill commit
  HASH2=$(git -C .. log -2 $HASH --format=%H -- sys/ | tail -1)
  PILL_NAME2="git-${HASH2:0:10}"
  wget https://bootstrap.urbit.org/$PILL_NAME2.pill -O urbit.pill &&
  echo FIXME running test script to create fakezod, this might be overkill &&
  lsc test.ls &&
  mv urbit.pill prev/urbit.pill &&
  mv zod prev/zod &&
  export PIER_FRESH="y"
} || {
  echo Pilling: Out of ideas
  exit 1
}

lsc <<done
do
  require! <[ stream-snitch once recursive-copy wait-on ]>
  pty = require \pty.js
  
  urbit = pty.spawn 'urbit' <[-FI zod prev/zod]>
             .on \data -> process.stdout.write it

  on-next = (re,cb)->
    urbit.pipe (new stream-snitch re).on \match once cb

  on-next /\n(\/~|ford: )/ ->
    console.log "\n\n---\nnode: detected error\n---\n\n"
    set-timeout (-> process.exit 1), 1000
  
  <- on-next /dojo> /
  {PILL_NAME} = process.env
  do-pill = ->
    urbit.write "|label %home %#PILL_NAME\r"
    urbit.write ".urbit/pill +solid /==/#PILL_NAME/sys, =dub &\r"
    <- wait-on resources: <[ prev/zod/.urb/put/urbit.pill ]>
    urbit.write "\04"
    process.exit 0
  #
  if process.env.PIER_FRESH then do-pill!
  urbit.write "|autoload |\r"
  urbit.write "|mount %\r"
  <- wait-on resources: <[ prev/zod/home ]>
  <- recursive-copy '../sys/' 'prev/zod/home/sys/' {+overwrite} .then
  on-next /sync/ do-pill
done
cp prev/zod/.urb/put/urbit.pill urbit.pill
mkdir built-pill; cp urbit.pill built-pill/$PILL_NAME.pill

echo
echo Created $PILL_NAME.pill, to be uploaded if tests pass
echo
