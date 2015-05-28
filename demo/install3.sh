#!/bin/sh
#
X=$1
DEMO=demo
COLD=$DEMO/files
DESK=test/$X/$X/in/base

echo "cp -f $COLD/3-ape-tictactoe.hoon $DESK/ape/tictactoe/core.hook"
mkdir -p $DESK/ape/tictactoe
cp -f $COLD/3-ape-tictactoe.hoon $DESK/ape/tictactoe/core.hook

echo "cp -f $COLD/mar-octo-game.hoon $DESK/mar/octo-game/door.hook"
mkdir -p $DESK/mar/octo-game
cp -f $COLD/mar-octo-game.hoon $DESK/mar/octo-game/door.hook

echo "cp -f $COLD/pub-octo-hymn.hoon $DESK/pub/octo/fab/hymn.hook"
mkdir -p $DESK/pub/octo/fab
cp -f $COLD/pub-octo-hymn.hoon $DESK/pub/octo/fab/hymn.hook

echo "cp -f $COLD/pub-octo-main.css $DESK/pub/octo/src/main.css"
mkdir -p $DESK/pub/octo/src
cp -f $COLD/pub-octo-main.css $DESK/pub/octo/src/main.css

echo "cp -f $COLD/pub-octo-main.js $DESK/pub/octo/src/main.js"
cp -f $COLD/pub-octo-main.js $DESK/pub/octo/src/main.js
