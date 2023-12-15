::  Print keys for a ship, as stored in %ames
::
:-  %say
|=  [[now=@da tick=@ud @ our=ship ^] [=ship ~] ~]
=+  .^  =ship-state:ames
        %ax  (en-bema [our %$ [da+now ud+tick]] /peers/(scot %p ship))
    ==
:-  %noun
?.  ?=(%known -.ship-state)
  %ship-still-alien
[life=life rift=rift]:+.ship-state
