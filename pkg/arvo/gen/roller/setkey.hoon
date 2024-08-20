::  Loads a private key into the roller and retrieves its L1 nonce
::
:-  %say
|=  [* [pk=@t ~] ~]
[%roller-action %config %setkey pk]
