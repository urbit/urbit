#!/bin/bash

whitespace=$(find .. -path ../.git -prune -o \
             -type f -exec egrep -l " +$" {} \;);

if [ ! -z $whitespace ]
then
  echo 'found trailing whitespace in:';
  echo $whitespace;
  exit 1;
fi
