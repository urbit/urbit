::  Print keys for a ship, as stored in %ames
::
:-  %say
|=  [[now=time @ our=ship ^] [=ship ~] ~]
=+  .^  =ship-state:ames
        %ax  /(scot %p our)//(scot %da now)/peers/(scot %p ship)
    ==
:-  %noun
?.  ?=(%known -.ship-state)
  %ship-still-alien
[life=life rift=rift]:+.ship-state
