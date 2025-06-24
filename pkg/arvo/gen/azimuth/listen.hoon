::  Add a source for Azimuth
::
=>  |%
    +$  src
      $%  [%ship =ship ~]
          [%app =term ~]
      ==
    --
:-  %say
|=  [* [whos=(list ship) =src] ~]
=/  =source:jael
  ?-  -.src
    %ship  [%& ship.src]
    %app   [%| term.src]
  ==
[%azimuth-poke %listen whos source]
