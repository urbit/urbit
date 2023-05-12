::  Print keys for a ship, as stored in %ames
::
:-  %say
|=  [[now=time @ our=ship ^] [=ship ~] ~]
=+  .^  =ship-state:ames
        %ax  /(scot %p our)//(scot %da now)/peers/(scot %p ship)
    ==
=/  =peer-state:ames  ?>(?=(%known -.ship-state) +.ship-state)
:-  %noun
[life=life rift=rift]:peer-state
