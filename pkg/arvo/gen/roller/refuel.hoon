::  Bump the roller gas price for sending transactions
::
:-  %say
|=  [* [nonce=@ gas=@ud address=(unit @ux) ~] ~]
[%roller-action %refuel nonce address gas]
