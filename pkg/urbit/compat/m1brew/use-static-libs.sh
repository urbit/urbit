#!/bin/bash
set -euo pipefail
declare -a ldirs
for i in $@
do
  case $i in
    -L*) ldirs+=(${i:2});;
  esac
done
for i in $@
do
  case $i in
    -l*)
      lib=$(find ${ldirs[@]} -name lib${i:2}.a)
      if [ "$lib" != "" ]
      then
        echo $lib
      else
        echo $i
      fi;;
    *)  echo $i;;
  esac
done
