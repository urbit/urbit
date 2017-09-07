#!/usr/bin/env bash

#
# Print the number of enabled CPUs by either using nproc or ncpus.
# If both commands fail, fall back to a platform-independent C++ solution.
# If that also fails, just echo 1...
#

pushd "${0%/*}" &>/dev/null

nproc 2>/dev/null && exit 0 || ncpus 2>/dev/null && exit 0 || {
  if [ ! -f cpucount ]; then
    c++ cpucount.cpp -std=c++0x -o cpucount &>/dev/null || { echo 1 && exit 0; }
  fi
}

./cpucount

popd &>/dev/null
