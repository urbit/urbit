#!/usr/bin/env bash

whitespace=$(find .. -path ../.git -prune -o \
             -type f -exec egrep -l " +$" {} \;);

if [ -n "$whitespace" ]
then
  echo 'found trailing whitespace in:';
  echo $whitespace;
  exit 1;
fi
