::  Sends a command to the BTC store app
::
::  Commands:
::
::  > :btc-node-store|command [%switch-wallet 'local']
::
/-  *btc-node-store
::
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[comm=btc-node-store-command ~] ~]
    ==
:-  %btc-node-store-command
?+  -.comm  ~|([%unsupported-command -.comm] !!)
    %switch-wallet  comm
==
