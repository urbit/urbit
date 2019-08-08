=>  |%
    +$  src
      $%  [%ship =ship ~]
          [%app =term ~]
      ==
    --
:-  %say
|=  [* [whos=(list ship) =src] ~]
=/  =source:kale
  ?-  -.src
    %ship  [%& ship.src]
    %app   [%| term.src]
  ==
[%azimuth-tracker-poke %listen whos source]
