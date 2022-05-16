::  Bumps the gas price for a sending transaction
::
:-  %say
|=  [* [nonce=@ gas=@ud address=(unit @ux) ~] ~]
[%roller-action %refuel nonce address gas]
