#!/bin/bash
usage() { printf "Usage: $0 URBIT_PIER_DIRECTORY \n" 1>&2; exit 1; }

if [ $# -eq 0 ]; then
    usage
    exit 2
fi
PIER=$1

cp sys/zuse.hoon $PIER/
echo "zuse copied"

cp lib/bip32.hoon $PIER/
echo "lib/bip32/hoon copied"
