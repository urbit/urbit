=>  |%
    +$  src
      $%  [%ship =ship ~]
          [%node url=@t ~]
      ==
    --
:-  %say
|=  [* [whos=(list ship) =src] ~]
=/  =source:kale
  ?-  -.src
    %ship  [%& ship.src]
    %node  [%| ~|(%parsing-url (need (de-purl:html url.src))) *@ud *@da]
  ==
[%azimuth-tracker-poke %listen whos source]
