#!/bin/bash
# XX use -s instead of hash pill
HASH=$(git -C .. log -1 HEAD --format="%H" -- sys/)
export PILL_NAME="git-${HASH:0:10}"

if [ ! $PILL_FORCE ]; then
  wget https://bootstrap.urbit.org/$PILL_NAME.pill -O urbit.pill && exit 0
fi

# if wget failed

echo FIXME ignoring CI commit, as current sys/ commits are unlikely to contain this code
# if [ $TRAVIS_COMMIT ] && [ $TRAVIS_COMMIT != $HASH ]; then
#   echo Directory sys/ not modified in commit $TRAVIS_COMMIT
#   echo For auto-build please tag and push $HASH
#   exit 1
# fi

mkdir prev
curl < pin-parent-pill-pier.url | tar xvz -C prev/ ||
{ echo "FIXME not building directly from pill"; exit 1}

lsc -e <<done
do
  require! <[ stream-snitch once recursive-copy wait-on ]>
  pty = require \pty.js
  
  urbit = pty.spawn './urbit' <[-FI zod prev/zod]>
             .on \data -> process.stdout.write it

  on-next = (re,cb)->
    urbit.pipe (new stream-snitch re).on \match once cb

  on-next /\n(\/~|ford: )/ ->
    console.log "\n\n---\nnode: detected error\n---\n\n"
    set-timeout (-> process.exit 1), 1000
  
  <- on-next /dojo> /
  {PILL_NAME} = process.env
  urbit.write "|autoload |\r"
  urbit.write "|mount %\r"
  <- wait-on resources: <[ prev/zod/home ]>
  <- recursive-copy '../sys/' 'prev/zod/home/sys/' {+overwrite} .then
  <- on-next /sync/
  urbit.write "|label %home %#PILL_NAME\r"
  urbit.write ".urbit/pill +solid /==/#PILL_NAME/sys, =dub &\r"
  <- wait-on resources: <[ prev/zod/.urb/put/urbit.pill ]>
  urbit.write "\04"
  process.exit 0
done
cp prev/zod/.urb/put/urbit.pill urbit.pill
mkdir built-pill; cp urbit.pill built-pill/$PILL_NAME.pill
