::  +gall-nonces: print %gall agent subscription nonces, highest-last
::
:-  %say
|=  [[now=@da tick=@ud @ our=@p ^] ~ ~]
:-  %noun
^-  (list [dude:gall @ud])
%+  sort
  %~  tap  by
  .^((map dude:gall @ud) %gf (en-bema [our %$ [da+now ud+tick]] /$))
|=([[* a=@ud] [* b=@ud]] (lth a b))
