#!/bin/sh
#
X=$1
DEMO=demo
COLD=$DEMO/files
DESK=urbit/$X/$X/in/$DEMO

echo "cp -f $COLD/3-ape-tictactoe.hoon $DESK/ape/tictactoe/core/hook"
cp -f $COLD/3-ape-tictactoe.hoon $DESK/ape/tictactoe/core/hook

echo "cp -f $COLD/mar-octo-game.hoon $DESK/mar/octo-game/door.hook"
cp -f $COLD/mar-octo-game.hoon $DESK/mar/octo-game/door.hook

echo "cp -f $COLD/pub-octo-hymn.hoon $DESK/pub/octo/fab/hymn.hook"
cp -f $COLD/pub-octo-hymn.hoon $DESK/pub/octo/fab/hymn.hook

echo "cp -f $COLD/pub-octo-main.css $DESK/pub/octo/src/main.css"
cp -f $COLD/pub-octo-main.css $DESK/pub/octo/src/main.css

echo "cp -f $COLD/pub-octo-main.js $DESK/pub/octo/src/main.js"
cp -f $COLD/pub-octo-main.js $DESK/pub/octo/src/main.js
