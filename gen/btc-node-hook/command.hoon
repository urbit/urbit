::  Sends a command to the BTC hook app
::
::  Commands:
::
::  [%credentials 'http://127.0.0.1:18443/' 'user' 'password']
::
/-  *btc-node-hook
::
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[comm=btc-node-hook-command ~] ~]
    ==
:-  %btc-node-hook-command
?+  -.comm  ~|  [%unsupported-command -.comm]  !!
      %credentials  comm
      %watch    comm
      %unwatch  comm
      %ping     comm
==
