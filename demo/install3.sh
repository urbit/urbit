#!/bin/sh
#
X=$1
DEMO=demo
COLD=$DEMO/files
DESK=urbit/$X/$X/in/$DEMO

echo "cp -f $COLD/3-ape-tictactoe.hoon $DESK/ape/tictactoe/core/hook"
cp -f $COLD/3-ape-tictactoe.hoon $DESK/ape/tictactoe/core/hook

echo "cp -f $COLD/mar-tictactoe-game.hoon $DESK/mar/tictactoe-game/door.hook"
cp -f $COLD/mar-tictactoe-game.hoon $DESK/mar/tictactoe-game/door.hook

echo "cp -f $COLD/pub-tictactoe-hymn.hoon $DESK/pub/tictactoe/fab/hymn.hook"
cp -f $COLD/pub-tictactoe-hymn.hoon $DESK/pub/tictactoe/fab/hymn.hook

echo "cp -f $COLD/pub-tictactoe-main.css $DESK/pub/tictactoe/src/main.css"
cp -f $COLD/pub-tictactoe-main.css $DESK/pub/tictactoe/src/main.css

echo "cp -f $COLD/pub-tictactoe-main.js $DESK/pub/tictactoe/src/main.js"
cp -f $COLD/pub-tictactoe-main.js $DESK/pub/tictactoe/src/main.js
