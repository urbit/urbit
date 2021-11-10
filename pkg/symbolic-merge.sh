#!/bin/bash

# call from within pkg/ as ./symbolic-merge.sh source-pkg target-pkg

function link() { # source desk, target desk, filepath
  local src=${3:2}; # strip leading ./
  local pax=$src;
  local rel=$1;
  while [[ "." != $(dirname "$pax") ]]; do
    pax=$(dirname "$pax");
    rel="../$rel";
  done;
  ln -sf "../$rel/$src" "../$2/$src"; 
}

# mirror directory structure
cd $1;
find . -type d -exec mkdir -p ../$2/{} \;

# symlink all files, overwriting existing ones
export -f link
find . -type f \
       -not -name '*.bill' \
       -not -name '*.kelvin' \
       -exec bash -c "link $1 $2 {}" \;

