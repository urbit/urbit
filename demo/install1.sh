#!/bin/sh
#
X=$1
DEMO=demo
COLD=$DEMO/files
DESK=test/$X/$X/in/base

echo "cp -f $COLD/1-ape-tictactoe.hoon $DESK/ape/tictactoe/core.hook"
mkdir -p $DESK/ape/tictactoe
cp -f $COLD/1-ape-tictactoe.hoon $DESK/ape/tictactoe/core.hook
