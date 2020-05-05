::  Sends a command to the BTC hook app
::
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
==
