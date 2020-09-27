#!/bin/bash
usage() { printf "Usage: $0 [-w] URBIT_PIER_DIRECTORY  \n(-w: flag to watch and live copy code)\n" 1>&2; exit 1; }

if [ $# -eq 0 ]; then
    usage
    exit 2
fi
PIER=$1

while getopts "w" opt; do
    case ${opt} in
        w) WATCH_MODE="true"
           PIER=$2
           ;;
        *) usage
           ;;
    esac
done

if [ -z "$WATCH_MODE" ]; then
    echo "Installed %coiny-store"
    rsync -r --exclude '.*' --exclude '*.sh' --exclude '*.md' * $PIER/
else
   echo "Watching for changes to copy to ${PIER}..."
   while [ 0 ]
   do
    sleep 0.8
    rsync -r --exclude '.*' --exclude '*.sh' --exclude '*.md' * $PIER/
   done
fi
