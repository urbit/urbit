::  Sends a command to the BTC Bridge
::
::  Commands:
::
::
::
/-  *btc-bridge
::
:-  %say
|=  $:  [now=@da eny=@uvJ =beak]
        [[comm=command ~] ~]
    ==
:-  %btc-bridge-command
?+    -.comm  ~|  [%unsupported-command -.comm]  !!
    %connect-as-host  comm
    %connect-as-client  comm
    %add-clients  comm
    %broadcast-tx  comm
==
